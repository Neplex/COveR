#ifndef IC_HAUS_H
#define IC_HAUS_H

#include "interval.h"

void ic_hausdorff_update(const Interval *const *elements, Interval **centers, double **asso,
                         unsigned nb_elements, unsigned nb_clusters, unsigned nb_interval, double m,
                         double *withinss);

#endif
