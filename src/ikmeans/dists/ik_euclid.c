#include "ik_euclid.h"

#include "distance.h"
#include "helpers.h"
#include "interval.h"

void ik_euclid_update(const Interval *const *elements, Interval **centers, const unsigned *asso,
                      const unsigned nb_elements, const unsigned nb_clusters,
                      const unsigned nb_interval, double *withinss) {
    // Update class by class
    for (unsigned k = 0; k < nb_clusters; k++) {
        withinss[k] = 0;  ///< withinss inside class k

        // For all intervals compute mean of all elements in class
        for (unsigned j = 0; j < nb_interval; j++) {
            double min = 0;
            double max = 0;
            unsigned nb_elem = 0;  ///< Nb elem in cluster k

            // For elements in class
            for (unsigned i = 0; i < nb_elements; i++) {
                if (asso[i] == k) {
                    const Interval r = elements[i][j];
                    min += r.min;
                    max += r.max;
                    nb_elem++;
                }
            }

            if (nb_elem) {
                // If is not empty, update to mean of elements in it
                centers[k][j].min = min / nb_elem;
                centers[k][j].max = max / nb_elem;
            } else {
                // If is empty, remove class
                centers[k][j].min = centers[k][j].max = NAN;
            }
        }

        // Update withinss
        for (unsigned i = 0; i < nb_elements; i++) {
            if (asso[i] == k) {
                withinss[k] += square_distance(elements[i], centers[k], nb_interval);
            }
        }
    }
}
