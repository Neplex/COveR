#include "ik_hausdorff.h"

#include "distance.h"
#include "helpers.h"
#include "interval.h"

void ik_hausdorff_update(const Interval *const *elements, Interval **centers, const unsigned *asso,
                         unsigned nb_elements, unsigned nb_clusters, unsigned nb_interval,
                         double *withinss) {
    // Update class by class
    for (unsigned k = 0; k < nb_clusters; k++) {
        withinss[k] = 0;  ///< withinss inside class k

        // For all intervals compute mean of all elements in class
        for (unsigned j = 0; j < nb_interval; j++) {
            double c_a[nb_elements];
            double hs_a[nb_elements];
            unsigned nb_elem = 0;  ///< Nb elem in cluster k

            // For elements in class
            for (unsigned i = 0; i < nb_elements; i++) {
                if (asso[i] == k) {
                    c_a[nb_elem] = get_center(elements[i][j]);
                    hs_a[nb_elem] = get_half_size(elements[i][j]);

                    nb_elem++;
                }
            }

            const double c = median(c_a, nb_elem);
            const double hs = median(hs_a, nb_elem);

            centers[k][j].min = c - hs;
            centers[k][j].max = c + hs;
        }

        // Update withinss
        for (unsigned i = 0; i < nb_elements; i++) {
            if (asso[i] == k) {
                withinss[k] += haus_distance(centers[k], elements[i], nb_interval);
            }
        }
    }
}