#ifndef IC_EUCLID_H
#define IC_EUCLID_H

#include "interval.h"

void ic_euclid_update(const Interval *const *elements, Interval **centers, double **asso,
                      unsigned nb_elements, unsigned nb_clusters, unsigned nb_interval, double m,
                      double *withinss);

#endif
