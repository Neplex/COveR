#include "io_hausdorff_mean.h"

#include <stdbool.h>

#include "distance.h"
#include "helpers.h"
#include "interval.h"

double io_hausdorff_mean_distanceToClusters(const Interval *elem, const Interval *const *centers,
                                            const bool *asso, const unsigned nb_clusters,
                                            const unsigned nb_interval) {
    Interval mean_prototype[nb_interval];

    // For all intervals
    for (unsigned j = 0; j < nb_interval; j++) {
        mean_prototype[j].min = 0;
        mean_prototype[j].max = 0;
        unsigned nbc = 0;

        // For all associated clusters
        for (unsigned k = 0; k < nb_clusters; k++) {
            if (asso[k]) {
                mean_prototype[j].min += centers[k][j].min;
                mean_prototype[j].max += centers[k][j].max;
                nbc++;
            }
        }

        if (nbc) {  // If the element have associated clusters
            mean_prototype[j].min /= nbc;
            mean_prototype[j].max /= nbc;
        } else {  // If the element have no cluster
            mean_prototype[j].min = mean_prototype[j].max = INFINITY;
        }
    }

    return haus_distance(elem, mean_prototype, nb_interval);
}

void io_hausdorff_mean_std_update(const Interval *const *elements, Interval **centers, bool **asso,
                                  const unsigned nb_elements, const unsigned nb_clusters,
                                  const unsigned nb_interval, const bool need_valid,
                                  double *withinss) {
    // Σ|yi − a * zi|

    // Get number of associate clusters by elements
    unsigned nb_asso[nb_elements];
    for (unsigned i = 0; i < nb_elements; i++) {
        nb_asso[i] = 0;
        for (unsigned j = 0; j < nb_clusters; j++) {
            nb_asso[i] += asso[i][j];
        }
    }

    // Update cluster by cluster
    for (unsigned k = 0; k < nb_clusters; k++) {
        // For all intervals
        for (unsigned j = 0; j < nb_interval; j++) {
            double z[nb_elements];
            double b1[nb_elements];  // Center
            double b2[nb_elements];  // Half-size
            unsigned nb_elem = 0;

            // For all elements
            for (unsigned i = 0; i < nb_elements; i++) {
                if (asso[i][k]) {
                    // Get the mean prototype of all associate class without k
                    double c = 0;
                    double hs = 0;
                    for (unsigned l = 0; l < nb_clusters; l++) {
                        if (asso[i][l] && l != k) {
                            c += get_center(centers[l][j]);
                            hs += get_half_size(centers[l][j]);
                        }
                    }

                    z[nb_elem] = 1.0 / nb_asso[i];
                    b1[nb_elem] = get_center(elements[i][j]) * nb_asso[i] - c;
                    b2[nb_elem] = get_half_size(elements[i][j]) * nb_asso[i] - hs;
                    nb_elem++;
                }
            }

            double c = weighted_median(b1, z, nb_elem);
            double hs = weighted_median(b2, z, nb_elem);

            if (need_valid && hs < 0) hs = 0;

            centers[k][j].min = c - hs;
            centers[k][j].max = c + hs;
        }
    }

    // Update withinss
    for (unsigned i = 0; i < nb_elements; i++) {
        withinss[i] = io_hausdorff_mean_distanceToClusters(
            elements[i], (const Interval *const *)centers, asso[i], nb_clusters, nb_interval);
    }
}

void io_hausdorff_mean_update(const Interval *const *elements, Interval **centers, bool **asso,
                              const unsigned nb_elements, const unsigned nb_clusters,
                              const unsigned nb_interval, const Algorithm algo,
                              const bool need_valid, double *withinss) {
    switch (algo) {
        case STD:
            io_hausdorff_mean_std_update(elements, centers, asso, nb_elements, nb_clusters,
                                         nb_interval, need_valid, withinss);
            break;

        case MATRIX:
            error("NOT IMPLEMENT\n");
    }
}
