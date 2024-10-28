#ifndef IO_EUCLID_H
#define IO_EUCLID_H

#include <stdbool.h>

#include "helpers.h"
#include "interval.h"

double io_euclid_distanceToClusters(const Interval *elem, const Interval *const *centers,
                                    const bool *asso, unsigned nb_clusters, unsigned nb_interval,
                                    Update up);

void io_euclid_update(const Interval *const *elements, Interval **centers, bool **asso,
                      unsigned nb_elements, unsigned nb_clusters, unsigned nb_interval,
                      Algorithm algo, Update up, bool need_valid, double *withinss);

#endif
