#ifndef IO_HAUS_JOIN_H
#define IO_HAUS_JOIN_H

#include <stdbool.h>

#include "helpers.h"
#include "interval.h"

double io_hausdorff_join_distanceToClusters(const Interval *elem, const Interval *const *centers,
                                            const bool *asso, unsigned nb_clusters,
                                            unsigned nb_interval);

void io_hausdorff_join_update(const Interval *const *elements, Interval **centers, bool **asso,
                              unsigned nb_elements, unsigned nb_clusters, unsigned nb_interval,
                              Algorithm algo, bool need_valid, double *withinss);

#endif
