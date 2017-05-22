#ifndef __IC_EUCLID_H
#define __IC_EUCLID_H

#include "../../helpers.h"

void ic_euclid_update(Interval **elements, Interval **centers, double **asso,
                      unsigned nb_elements, unsigned nb_clusters,
                      unsigned nb_interval, double m, double *withinss) {

  double wss[nb_clusters]; ///< New withinss

  // Update class by class
  for (size_t k = 0; k < nb_clusters; k++) {
    wss[k] = 0; ///< withinss inside class k

    // For all intervals compute weighted mean of all elements in class
    for (size_t j = 0; j < nb_interval; j++) {
      double min = 0;
      double max = 0;
      double weight_sum = 0; ///< Sum of weight in cluster k

      // For elements in class
      for (size_t i = 0; i < nb_elements; i++) {
        double w = pow(asso[i][k], m);

        min += elements[i][j].min * w;
        max += elements[i][j].max * w;
        weight_sum += w;
      }

      if (weight_sum) { // If is not empty, update to mean of elements in it
        centers[k][j].min = min / weight_sum;
        centers[k][j].max = max / weight_sum;
      } else { // If is empty, remove class
        centers[k][j].min = centers[k][j].max = NAN;
      }
    }

    // Update withinss
    for (size_t i = 0; i < nb_elements; i++) {
      if (asso[i][k])
        wss[k] += square_distance(elements[i], centers[k], nb_interval) *
                  pow(asso[i][k], m);
    }
  }

  copy_array(wss, withinss, nb_clusters); // save withinss
}

#endif
