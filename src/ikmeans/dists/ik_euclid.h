#ifndef __IK_EUCLID_H
#define __IK_EUCLID_H

#include "../../helpers.h"

void ik_euclid_update(Interval **elements, Interval **centers, unsigned *asso,
                      unsigned nb_elements, unsigned nb_clusters,
                      unsigned nb_interval, double *withinss) {

  double wss[nb_clusters]; ///< New withinss

  // Update class by class
  for (size_t k = 0; k < nb_clusters; k++) {
    wss[k] = 0; ///< withinss inside class k

    // For all intervals compute mean of all elements in class
    for (size_t j = 0; j < nb_interval; j++) {
      double min = 0;
      double max = 0;
      unsigned nb_elem = 0; ///< Nb elem in cluster k

      // For elements in class
      for (size_t i = 0; i < nb_elements; i++) {
        if (asso[i] == k) {

          Interval r = elements[i][j];
          min += r.min;
          max += r.max;
          nb_elem++;
        }
      }

      if (nb_elem) { // If is not empty, update to mean of elements in it
        centers[k][j].min = min / nb_elem;
        centers[k][j].max = max / nb_elem;
      } else { // If is empty, remove class
        centers[k][j].min = centers[k][j].max = NAN;
      }
    }

    // Update withinss
    for (size_t i = 0; i < nb_elements; i++) {
      if (asso[i] == k) {
        wss[k] += square_distance(elements[i], centers[k], nb_interval);
      }
    }
  }

  copy_array(wss, withinss, nb_clusters); // save withinss
}

#endif
