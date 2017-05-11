#ifndef __IO_EUCLID_MEET_H
#define __IO_EUCLID_MEET_H

#include "../../../helpers.h"

double io_euclid_meet_distanceToClusters(Interval *elem, Interval **centers,
                                         bool *asso, unsigned nb_clusters,
                                         unsigned nb_interval) {
  Interval meet_prototype[nb_interval];

  // For all intervals
  for (size_t j = 0; j < nb_interval; j++) {
    meet_prototype[j].min = -INFINITY;
    meet_prototype[j].max = INFINITY;

    // For all associated clusters
    for (size_t k = 0; k < nb_clusters; k++) {
      if (asso[k]) {
        meet_prototype[j].min = max(meet_prototype[j].min, centers[k][j].min);
        meet_prototype[j].max = min(meet_prototype[j].max, centers[k][j].max);

        // If no intersection return infinity distance
        if (meet_prototype[j].max < meet_prototype[j].min)
          return INFINITY;
      }
    }
  }

  return square_distance(elem, meet_prototype, nb_interval);
}

void io_euclid_meet_update(Interval **elements, Interval **centers, bool **asso,
                           unsigned nb_elements, unsigned nb_clusters,
                           unsigned nb_interval, Algorithm algo,
                           bool need_valid, double *withinss) {
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
