#ifndef __IO_HAUS_SUM_H
#define __IO_HAUS_SUM_H

#include "../../../helpers.h"

double io_hausdorff_sum_distanceToClusters(Interval *elem, Interval **centers,
                                           bool *asso, unsigned nb_clusters,
                                           unsigned nb_interval) {
  Interval sum_prototype[nb_interval];

  // For all intervals
  for (size_t j = 0; j < nb_interval; j++) {
    sum_prototype[j].min = 0;
    sum_prototype[j].max = 0;
    unsigned nbc = 0;

    // For all associated clusters
    for (size_t k = 0; k < nb_clusters; k++) {
      if (asso[k]) {
        sum_prototype[j].min += centers[k][j].min;
        sum_prototype[j].max += centers[k][j].max;
        nbc++;
      }
    }

    if (!nbc) { // If the element have no cluster
      sum_prototype[j].min = sum_prototype[j].max = INFINITY;
    }
  }

  return haus_distance(elem, sum_prototype, nb_interval);
}

void io_hausdorff_sum_std_update(Interval **elements, Interval **centers,
                                 bool **asso, unsigned nb_elements,
                                 unsigned nb_clusters, unsigned nb_interval,
                                 bool need_valid, double *withinss) {

  // Update cluster by cluster
  for (size_t k = 0; k < nb_clusters; k++) {

    // For all intervals
    for (size_t j = 0; j < nb_interval; j++) {

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

          b1[nb_elem] = get_center(elements[i][j]) - c;
          b2[nb_elem] = get_half_size(elements[i][j]) - hs;
          nb_elem++;
        }
      }

      double c = median(b1, nb_elem);
      double hs = median(b2, nb_elem);

      if (need_valid && hs < 0)
        hs = 0;

      centers[k][j].min = c - hs;
      centers[k][j].max = c + hs;
    }
  }

  // Update withinss
  for (size_t i = 0; i < nb_elements; i++) {
    withinss[i] = io_hausdorff_sum_distanceToClusters(
        elements[i], centers, asso[i], nb_clusters, nb_interval);
  }
}

void io_hausdorff_sum_update(Interval **elements, Interval **centers,
                             bool **asso, unsigned nb_elements,
                             unsigned nb_clusters, unsigned nb_interval,
                             Algorithm algo, bool need_valid,
                             double *withinss) {
  switch (algo) {
  case STD:
    io_hausdorff_sum_std_update(elements, centers, asso, nb_elements,
                                nb_clusters, nb_interval, need_valid, withinss);
    break;

  case MATRIX:
    error("NOT IMPLEMENT\n");
    break;
  }
}

#endif
