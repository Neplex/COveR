#include "io_euclid_join.h"

#include <stdbool.h>

#include "distance.h"
#include "helpers.h"
#include "interval.h"

double io_euclid_join_distanceToClusters(const Interval *elem, const Interval *const *centers,
                                         const bool *asso, const unsigned nb_clusters,
                                         const unsigned nb_interval) {
    Interval join_prototype[nb_interval];

    // For all intervals
    for (unsigned j = 0; j < nb_interval; j++) {
        join_prototype[j].min = INFINITY;
        join_prototype[j].max = -INFINITY;

        // For all associated clusters
        for (unsigned k = 0; k < nb_clusters; k++) {
            if (asso[k]) {
                join_prototype[j].min = min(join_prototype[j].min, centers[k][j].min);
                join_prototype[j].max = max(join_prototype[j].max, centers[k][j].max);
            }
        }
    }

    return square_distance(elem, join_prototype, nb_interval);
}

void io_euclid_join_update(const Interval *const *elements, Interval **centers, bool **asso,
                           const unsigned nb_elements, const unsigned nb_clusters,
                           const unsigned nb_interval, const Algorithm algo, const bool need_valid,
                           double *withinss) {
    switch (algo) {
        case STD:
        case MATRIX:
            error("NOT IMPLEMENT\n");
    }
}
