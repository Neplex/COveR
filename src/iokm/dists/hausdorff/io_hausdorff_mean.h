#ifndef __IO_HAUS_MEAN_H
#define __IO_HAUS_MEAN_H

#include "../../../helpers.h"

double io_hausdorff_mean_distanceToClusters(Interval *elem, Interval **centers,
                                            bool *asso, unsigned nb_clusters,
                                            unsigned nb_interval) {
  Interval mean_prototype[nb_interval];

  // For all intervals
  for (size_t j = 0; j < nb_interval; j++) {
    mean_prototype[j].min = 0;
    mean_prototype[j].max = 0;
    unsigned nbc = 0;

    // For all associated clusters
    for (size_t k = 0; k < nb_clusters; k++) {
      if (asso[k]) {
        mean_prototype[j].min += centers[k][j].min;
        mean_prototype[j].max += centers[k][j].max;
        nbc++;
      }
    }

    if (nbc) { // If the element have associated clusters
      mean_prototype[j].min /= nbc;
      mean_prototype[j].max /= nbc;
    } else { // If the element have no cluster
      mean_prototype[j].min = mean_prototype[j].max = INFINITY;
    }
  }

  return haus_distance(elem, mean_prototype, nb_interval);
}

void swap(unsigned *index, unsigned i, unsigned j) {
  unsigned tmp = index[i];
  index[i] = index[j];
  index[j] = tmp;
}

int partition(double *array, unsigned *index, int left, int right, int pivot) {
  swap(index, pivot, right);
  int j = left;
  for (size_t i = left; i < right; i++) {
    if (array[index[i]] <= array[index[right]]) {
      swap(index, i, j);
      j++;
    }
  }
  swap(index, right, j);
  return j;
}

void get_sort_order(double *array, unsigned *index, int left, int right) {
  if (left < right) {
    int pivot = (left + right) / 2;
    pivot = partition(array, index, left, right, pivot);
    get_sort_order(array, index, left, pivot - 1);
    get_sort_order(array, index, pivot + 1, right);
  }
}

double getMedian(double *b, double *z, unsigned nb_elements) {
  unsigned index[nb_elements];
  for (size_t i = 0; i < nb_elements; i++)
    index[i] = i;

  get_sort_order(b, index, 0, nb_elements - 1);

  double sum_left = 0;
  double sum_right = sum_double_array(z, nb_elements);
  unsigned i;

  for (i = 0; i < nb_elements; i++) {
    sum_left += z[index[i]];
    sum_right -= z[index[i]];

    if (sum_left > sum_right)
      break;
  }

  return b[index[i]];
}

void io_hausdorff_mean_std_update(Interval **elements, Interval **centers,
                                  bool **asso, unsigned nb_elements,
                                  unsigned nb_clusters, unsigned nb_interval,
                                  bool need_valid, double *withinss) {
  // Σ|yi − a * zi|

  // Get number of associate clusters by elements
  unsigned nb_asso[nb_elements];
  for (size_t i = 0; i < nb_elements; i++) {
    nb_asso[i] = 0;
    for (size_t j = 0; j < nb_clusters; j++) {
      nb_asso[i] += asso[i][j];
    }
  }

  // Update cluster by cluster
  for (size_t k = 0; k < nb_clusters; k++) {

    // For all intervals
    for (size_t j = 0; j < nb_interval; j++) {

      double z[nb_elements];
      double b1[nb_elements]; // Center
      double b2[nb_elements]; // Half-size
      unsigned nb_elem = 0;

      // For all elements
      for (size_t i = 0; i < nb_elements; i++) {
        if (asso[i][k]) {

          // Get the mean prototype of all associate class without k
          double c = 0;
          double hs = 0;
          for (size_t l = 0; l < nb_clusters; l++) {
            if (asso[i][l] && l != k) {

              c += get_center(centers[l][j]);
              hs += get_half_size(centers[l][j]);
            }
          }

          z[nb_elem] = 1.0 / nb_asso[i];
          b1[nb_elem] = get_center(elements[i][j]) * nb_asso[i] - c;
          b2[nb_elem] = get_half_size(elements[i][j]) * nb_asso[i] - hs;
          nb_elem++;
        }
      }

      double c = getMedian(b1, z, nb_elem);
      double hs = getMedian(b2, z, nb_elem);

      if (need_valid && hs < 0)
        hs = 0;

      centers[k][j].min = c - hs;
      centers[k][j].max = c + hs;
    }
  }

  // Update withinss
  for (size_t i = 0; i < nb_elements; i++) {
    withinss[i] = io_hausdorff_mean_distanceToClusters(
        elements[i], centers, asso[i], nb_clusters, nb_interval);
  }
}

void io_hausdorff_mean_update(Interval **elements, Interval **centers,
                              bool **asso, unsigned nb_elements,
                              unsigned nb_clusters, unsigned nb_interval,
                              Algorithm algo, bool need_valid,
                              double *withinss) {
  switch (algo) {
  case STD:
    io_hausdorff_mean_std_update(elements, centers, asso, nb_elements,
                                 nb_clusters, nb_interval, need_valid,
                                 withinss);
    break;

  case MATRIX:
    error("NOT IMPLEMENT\n");
    break;
  }
}

#endif
