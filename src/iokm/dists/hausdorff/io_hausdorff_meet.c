#include "io_hausdorff_meet.h"

#include <stdbool.h>

#include "distance.h"
#include "helpers.h"
#include "interval.h"

double io_hausdorff_meet_distanceToClusters(const Interval *elem, const Interval *const *centers,
                                            const bool *asso, const unsigned nb_clusters,
                                            const unsigned nb_interval) {
    Interval meet_prototype[nb_interval];

    // For all intervals
    for (unsigned j = 0; j < nb_interval; j++) {
        meet_prototype[j].min = -INFINITY;
        meet_prototype[j].max = INFINITY;

        // For all associated clusters
        for (unsigned k = 0; k < nb_clusters; k++) {
            if (asso[k]) {
                meet_prototype[j].min = max(meet_prototype[j].min, centers[k][j].min);
                meet_prototype[j].max = min(meet_prototype[j].max, centers[k][j].max);

                // If no intersection return infinity distance
                if (meet_prototype[j].max < meet_prototype[j].min) return INFINITY;
            }
        }
    }

    return haus_distance(elem, meet_prototype, nb_interval);
}

void io_hausdorff_meet_update(const Interval *const *elements, Interval **centers, bool **asso,
                              const unsigned nb_elements, const unsigned nb_clusters,
                              const unsigned nb_interval, const Algorithm algo,
                              const bool need_valid, double *withinss) {
    switch (algo) {
        case STD:
        case MATRIX:
            error("NOT IMPLEMENT\n");
    }
}
