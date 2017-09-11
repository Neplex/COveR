#ifndef __IO_EUCLID_MEAN_H
#define __IO_EUCLID_MEAN_H

#include "../../../helpers.h"

double io_euclid_mean_distanceToClusters(Interval *elem, Interval **centers,
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

  return square_distance(elem, mean_prototype, nb_interval);
}

void io_euclid_mean_std_update(Interval **elements, Interval **centers,
                               bool **asso, unsigned nb_elements,
                               unsigned nb_clusters, unsigned nb_interval,
                               bool need_valid, double *withinss) {

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
    double res = 0; ///< Sum of weight

    // New center for cluster k
    Interval center[nb_interval];
    for (size_t j = 0; j < nb_interval; j++) {
      center[j].min = 0;
      center[j].max = 0;
    }

    // For all elements in cluster
    for (size_t i = 0; i < nb_elements; i++) {
      if (asso[i][k]) {

        double weight = 1.0 / sqr(nb_asso[i]);
        res += weight;

        Interval copy[nb_interval];
        copy_array(elements[i], copy, nb_interval);

        for (size_t j = 0; j < nb_interval; j++) {
          copy[j].min *= nb_asso[i];
          copy[j].max *= nb_asso[i];

          for (size_t l = 0; l < nb_clusters; l++) {
            if (asso[i][l] && l != k) {
              copy[j].min -= centers[l][j].min;
              copy[j].max -= centers[l][j].max;
            }
          }

          copy[j].min *= weight;
          copy[j].max *= weight;

          center[j].min += copy[j].min;
          center[j].max += copy[j].max;
        }
      }
    }

    // Weighted mean
    for (size_t j = 0; j < nb_interval; j++) {
      center[j].min /= res;
      center[j].max /= res;

      if (need_valid && center[j].min > center[j].max)
        center[j].min = center[j].max = get_center(center[j]);
    }

    copy_array(center, centers[k], nb_interval); // save new center
  }

  // Update withinss
  for (size_t i = 0; i < nb_elements; i++) {
    withinss[i] = io_euclid_mean_distanceToClusters(
        elements[i], centers, asso[i], nb_clusters, nb_interval);
  }
}

/* TODO: GSL on CRAN
void io_euclid_mean_matrix_update(Interval **elements, Interval **centers,
                                  bool **asso, unsigned nb_elements,
                                  unsigned nb_clusters, unsigned nb_interval,
                                  bool need_valid, double *withinss) {

  // Normalisation
  gsl_matrix *norm = gsl_matrix_alloc(nb_elements, nb_clusters);
  for (size_t i = 0; i < nb_elements; i++) {
    double nbc = 0;
    for (size_t j = 0; j < nb_clusters; j++)
      nbc += asso[i][j];

    for (size_t j = 0; j < nb_clusters; j++) {
      gsl_matrix_set(norm, i, j, (double)asso[i][j] / nbc);
    }
  }

  // Pseudo inverse
  gsl_matrix *inv = moore_penrose_pinv(norm, 1E-15);

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
    withinss[i] = io_euclid_mean_distanceToClusters(
        elements[i], centers, asso[i], nb_clusters, nb_interval);
  }

  gsl_matrix_free(norm);
  gsl_matrix_free(inv);
}*/

void io_euclid_mean_update(Interval **elements, Interval **centers, bool **asso,
                           unsigned nb_elements, unsigned nb_clusters,
                           unsigned nb_interval, Algorithm algo,
                           bool need_valid, double *withinss) {
  switch (algo) {
  case STD:
    io_euclid_mean_std_update(elements, centers, asso, nb_elements, nb_clusters,
                              nb_interval, need_valid, withinss);
    break;

  case MATRIX:
    error("NOT IMPLEMENT\n");
    /* TODO: GSL on CRAN
    io_euclid_mean_matrix_update(elements, centers, asso, nb_elements,
                                 nb_clusters, nb_interval, need_valid,
                                 withinss);*/
    break;
  }
}

#endif
