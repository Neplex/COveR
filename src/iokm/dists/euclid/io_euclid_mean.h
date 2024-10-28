#ifndef IO_EUCLID_MEAN_H
#define IO_EUCLID_MEAN_H

#include <stdbool.h>

#include "helpers.h"
#include "interval.h"

double io_euclid_mean_distanceToClusters(const Interval *elem, const Interval *const *centers,
                                         const bool *asso, unsigned nb_clusters,
                                         unsigned nb_interval);

void io_euclid_mean_update(const Interval *const *elements, Interval **centers, bool **asso,
                           unsigned nb_elements, unsigned nb_clusters, unsigned nb_interval,
                           Algorithm algo, bool need_valid, double *withinss);

#endif
