#ifndef __IK_HAUS_H
#define __IK_HAUS_H

#include "../../helpers.h"

void ik_hausdorff_update(Interval **elements, Interval **centers,
                         unsigned *asso, unsigned nb_elements,
                         unsigned nb_clusters, unsigned nb_interval,
                         double *withinss) {

  // Update cluster by cluster
  for (size_t k = 0; k < nb_clusters; k++) {
    withinss[k] = 0;

    for (size_t j = 0; j < nb_interval; j++) {
      double c_a[nb_elements];
      double hs_a[nb_elements];
      unsigned nb_elem = 0;

      for (size_t i = 0; i < nb_elements; i++) {
        if (asso[i] == k) {

          c_a[nb_elem] = get_center(elements[i][j]);
          hs_a[nb_elem] = get_half_size(elements[i][j]);

          nb_elem++;
        }
      }

      double c = median(c_a, nb_elem);
      double hs = median(hs_a, nb_elem);

      centers[k][j].min = c - hs;
      centers[k][j].max = c + hs;
    }

    for (size_t i = 0; i < nb_elements; i++) {
      if (asso[i] == k) {
        withinss[k] += haus_distance(centers[k], elements[i], nb_interval);
      }
    }
  }
}
#endif
