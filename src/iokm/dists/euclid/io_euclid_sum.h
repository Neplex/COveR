#ifndef __IO_EUCLID_SUM_H
#define __IO_EUCLID_SUM_H

#include "../../../helpers.h"

double io_euclid_sum_distanceToClusters(Interval *elem, Interval **centers,
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

  return square_distance(elem, sum_prototype, nb_interval);
}

void io_euclid_sum_std_update(Interval **elements, Interval **centers,
                              bool **asso, unsigned nb_elements,
                              unsigned nb_clusters, unsigned nb_interval,
                              bool need_valid, double *withinss) {
  // Update cluster by cluster
  for (size_t k = 0; k < nb_clusters; k++) {

    Interval center[nb_interval]; // New center for cluster k

    // Update interval by interval
    for (size_t j = 0; j < nb_interval; j++) {
      double nb_elem = 0;
      center[j].min = 0;
      center[j].max = 0;

      // For all elements in cluster
      for (size_t i = 0; i < nb_elements; i++) {
        if (asso[i][k]) {
          nb_elem++;
          center[j].min += elements[i][j].min;
          center[j].max += elements[i][j].max;

          // For all associated class without k
          for (size_t l = 0; l < nb_clusters; l++) {
            if (asso[i][l] && l != k) {

              center[j].min -= centers[l][j].min;
              center[j].max -= centers[l][j].max;
            }
          }
        }
      }

      center[j].min /= nb_elem;
      center[j].max /= nb_elem;

      if (need_valid && center[j].min > center[j].max)
        center[j].min = center[j].max = get_center(center[j]);
    }

    for (size_t j = 0; j < nb_interval; j++) {
    }

    copy_array(center, centers[k], nb_interval); // save new center
  }

  // Update withinss
  for (size_t i = 0; i < nb_elements; i++) {
    withinss[i] = io_euclid_sum_distanceToClusters(
        elements[i], centers, asso[i], nb_clusters, nb_interval);
  }
}

/* TODO: GSL on CRAN
void io_euclid_sum_matrix_update(Interval **elements, Interval **centers,
                                 bool **asso, unsigned nb_elements,
                                 unsigned nb_clusters, unsigned nb_interval,
                                 bool need_valid, double *withinss) {

  gsl_matrix *aso_matrix = gsl_matrix_alloc(nb_elements, nb_clusters);
  for (size_t i = 0; i < nb_elements; i++) {
    for (size_t j = 0; j < nb_clusters; j++) {
      gsl_matrix_set(aso_matrix, i, j, (double)asso[i][j]);
    }
  }

  // Pseudo inverse
  gsl_matrix *inv = moore_penrose_pinv(aso_matrix, 1E-15);

  // Multiply matrix (inv * elements)
  Interval res[nb_clusters][nb_interval];
  for (size_t i = 0; i < nb_clusters; i++) {
    for (size_t j = 0; j < nb_interval; j++) {
      res[i][j].min = 0;
      res[i][j].max = 0;

      for (size_t k = 0; k < nb_elements; k++) {
        res[i][j].min += gsl_matrix_get(inv, i, k) * elements[k][j].min;
        res[i][j].max += gsl_matrix_get(inv, i, k) * elements[k][j].max;
      }
    }
  }

  copy_matrix(res, centers, nb_clusters, nb_interval); // Save centers

  // Update withinss
  for (size_t i = 0; i < nb_elements; i++) {
    withinss[i] = io_euclid_sum_distanceToClusters(
        elements[i], centers, asso[i], nb_clusters, nb_interval);
  }

  gsl_matrix_free(aso_matrix);
  gsl_matrix_free(inv);
}
*/

void io_euclid_sum_update(Interval **elements, Interval **centers, bool **asso,
                          unsigned nb_elements, unsigned nb_clusters,
                          unsigned nb_interval, Algorithm algo, bool need_valid,
                          double *withinss) {
  switch (algo) {
  case STD:
    io_euclid_sum_std_update(elements, centers, asso, nb_elements, nb_clusters,
                             nb_interval, need_valid, withinss);
    break;

  case MATRIX:
    error("NOT IMPLEMENT\n");
    /* TODO: GSL on CRAN
    io_euclid_sum_matrix_update(elements, centers, asso, nb_elements,
                                nb_clusters, nb_interval, need_valid, withinss);*/
    break;
  }
}

#endif
