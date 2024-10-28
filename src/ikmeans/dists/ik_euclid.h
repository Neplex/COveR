#ifndef IK_EUCLID_H
#define IK_EUCLID_H

#include "interval.h"

void ik_euclid_update(const Interval *const *elements, Interval **centers, const unsigned *asso,
                      unsigned nb_elements, unsigned nb_clusters, unsigned nb_interval,
                      double *withinss);

#endif
