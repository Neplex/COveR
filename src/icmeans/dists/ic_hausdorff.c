#include "ic_hausdorff.h"

#include <math.h>

#include "distance.h"
#include "helpers.h"
#include "interval.h"

void ic_hausdorff_update(const Interval *const *elements, Interval **centers, double **asso,
                         unsigned nb_elements, unsigned nb_clusters, unsigned nb_interval, double m,
                         double *withinss) {
    // Update cluster by cluster
    for (unsigned k = 0; k < nb_clusters; k++) {
        withinss[k] = 0;

        // Compute the membership degree
        double weight[nb_elements];
        for (unsigned i = 0; i < nb_elements; i++) {
            weight[i] = pow(asso[i][k], m);
        }

        // For all intervals, compute the min and max
        for (unsigned j = 0; j < nb_interval; j++) {
            double b1[nb_elements];  // Center
            double b2[nb_elements];  // Half-size

            // For all elements
            for (unsigned i = 0; i < nb_elements; i++) {
                b1[i] = get_center(elements[i][j]);
                b2[i] = get_half_size(elements[i][j]);
            }

            const double c = weighted_median(b1, weight, nb_elements);
            const double hs = weighted_median(b2, weight, nb_elements);

            centers[k][j].min = c - hs;
            centers[k][j].max = c + hs;
        }

        // Update withinss
        for (unsigned i = 0; i < nb_elements; i++) {
            if (asso[i][k]) {
                const double distance = haus_distance(elements[i], centers[k], nb_interval);
                withinss[k] += distance * pow(asso[i][k], m);
            }
        }
    }
}