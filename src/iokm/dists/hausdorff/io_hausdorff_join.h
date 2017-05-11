#ifndef __IO_HAUS_JOIN_H
#define __IO_HAUS_JOIN_H

#include "../../../helpers.h"

double io_hausdorff_join_distanceToClusters(Interval *elem, Interval **centers,
                                            bool *asso, unsigned nb_clusters,
                                            unsigned nb_interval) {
  Interval join_prototype[nb_interval];

  // For all intervals
  for (size_t j = 0; j < nb_interval; j++) {
    join_prototype[j].min = INFINITY;
    join_prototype[j].max = -INFINITY;

    // For all associated clusters
    for (size_t k = 0; k < nb_clusters; k++) {
      if (asso[k]) {
        join_prototype[j].min = min(join_prototype[j].min, centers[k][j].min);
        join_prototype[j].max = max(join_prototype[j].max, centers[k][j].max);
      }
    }
  }

  return haus_distance(elem, join_prototype, nb_interval);
}

void io_hausdorff_join_update(Interval **elements, Interval **centers,
                              bool **asso, unsigned nb_elements,
                              unsigned nb_clusters, unsigned nb_interval,
                              Algorithm algo, bool need_valid,
                              double *withinss) {
  switch (algo) {
  case STD:
    error("NOT IMPLEMENT\n");
    break;

  case MATRIX:
    error("NOT IMPLEMENT\n");
    break;
  }
}

#endif
