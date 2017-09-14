#ifndef __IC_HAUS_H
#define __IC_HAUS_H

#include "../../helpers.h"

void ic_hausdorff_update(Interval **elements, Interval **centers, double **asso,
                         unsigned nb_elements, unsigned nb_clusters,
                         unsigned nb_interval, double m, double *withinss) {

  double wss[nb_clusters]; ///< New withinss

  // Update cluster by cluster
  for (size_t k = 0; k < nb_clusters; k++) {
    wss[k] = 0;

    // Compute the membership degree
    double weight[nb_elements];
    for (size_t i = 0; i < nb_elements; i++) {
      weight[i] = pow(asso[i][k], m);
    }

    // For all intervals, compute the min and max
    for (size_t j = 0; j < nb_interval; j++) {

      double b1[nb_elements]; // Center
      double b2[nb_elements]; // Half-size

      // For all elements
      for (size_t i = 0; i < nb_elements; i++) {
        b1[i] = get_center(elements[i][j]);
        b2[i] = get_half_size(elements[i][j]);
      }

      double c = weighted_median(b1, weight, nb_elements);
      double hs = weighted_median(b2, weight, nb_elements);

      centers[k][j].min = c - hs;
      centers[k][j].max = c + hs;
    }

    // Update withinss
    for (size_t i = 0; i < nb_elements; i++) {
      if (asso[i][k])
        wss[k] += haus_distance(elements[i], centers[k], nb_interval) *
                  pow(asso[i][k], m);
    }
  }

  copy_array(wss, withinss, nb_clusters); // save withinss
}

#endif
