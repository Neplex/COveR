#include "ic_euclid.h"

#include <math.h>

#include "distance.h"
#include "helpers.h"
#include "interval.h"

void ic_euclid_update(const Interval *const *elements, Interval **centers, double **asso,
                      const unsigned nb_elements, const unsigned nb_clusters,
                      const unsigned nb_interval, const double m, double *withinss) {
    // Update class by class
    for (unsigned k = 0; k < nb_clusters; k++) {
        withinss[k] = 0;  ///< withinss inside class k

        // For all intervals compute weighted mean of all elements in class
        for (unsigned j = 0; j < nb_interval; j++) {
            double min = 0;
            double max = 0;
            double weight_sum = 0;  ///< Sum of weight in cluster k

            // For elements in class
            for (unsigned i = 0; i < nb_elements; i++) {
                const double w = pow(asso[i][k], m);

                min += elements[i][j].min * w;
                max += elements[i][j].max * w;
                weight_sum += w;
            }

            if (weight_sum) {
                // If is not empty, update to mean of elements in it
                centers[k][j].min = min / weight_sum;
                centers[k][j].max = max / weight_sum;
            } else {
                // If is empty, remove class
                centers[k][j].min = centers[k][j].max = NAN;
            }
        }

        // Update withinss
        for (unsigned i = 0; i < nb_elements; i++) {
            if (asso[i][k]) {
                const double distance = square_distance(elements[i], centers[k], nb_interval);
                withinss[k] += distance * pow(asso[i][k], m);
            }
        }
    }
}
