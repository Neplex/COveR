#include "ikmeans.h"

#include "distance.h"
#include "dists/ik_euclid.h"
#include "dists/ik_hausdorff.h"
#include "helpers.h"

// ===== Functions =====

void ik_assign(const Interval *const *elements, const Interval *const *centers, unsigned *asso,
               const unsigned nb_elements, const unsigned nb_clusters, const unsigned nb_interval,
               const Distance dist, double *withinss) {
    // Assign element by element
    for (unsigned i = 0; i < nb_elements; i++) {
        double min_dist = INFINITY;

        // For all clusters, get the class with the min distance
        for (unsigned k = 0; k < nb_clusters; k++) {
            double d;

            switch (dist) {
                case HAUSDORFF:
                    d = haus_distance(elements[i], centers[k], nb_interval);
                    break;

                case EUCLIDEAN:
                default:
                    d = square_distance(elements[i], centers[k], nb_interval);
                    break;
            }

            if (d < min_dist) {
                min_dist = d;
                asso[i] = k;
            }
        }

        // Update withinss
        withinss[asso[i]] += min_dist;
    }
}

void ik_update(const Interval *const *elements, Interval **centers, const unsigned *asso,
               const unsigned nb_elements, const unsigned nb_clusters, const unsigned nb_interval,
               const Distance dist, double *withinss) {
    switch (dist) {
        case HAUSDORFF:
            ik_hausdorff_update(elements, centers, asso, nb_elements, nb_clusters, nb_interval,
                                withinss);
            break;

        case EUCLIDEAN:
            ik_euclid_update(elements, centers, asso, nb_elements, nb_clusters, nb_interval,
                             withinss);
            break;
    }
}

double ik_getBetweenss(const Interval *const *centers, const unsigned nb_clusters,
                       const unsigned nb_interval, const Distance dist) {
    double res = 0;
    // For all clusters
    for (unsigned k = 0; k < nb_clusters; k++) {
        // Get the mean element of other centers
        Interval *mean = new_array_Interval(nb_interval);
        for (unsigned j = 0; j < nb_interval; j++) {
            mean[j].min = 0;
            mean[j].max = 0;

            for (unsigned i = 0; i < nb_clusters; i++) {
                if (i != k) {
                    mean[j].min += centers[i][j].min;
                    mean[j].max += centers[i][j].max;
                }
            }

            mean[j].min /= nb_clusters;
            mean[j].max /= nb_clusters;
        }

        // Sum distance
        switch (dist) {
            case EUCLIDEAN:
                res += square_distance(centers[k], mean, nb_interval);
                break;

            case HAUSDORFF:
                res += haus_distance(centers[k], mean, nb_interval);
                break;
        }

        delete_array((void **)&mean);
    }

    return res;
}

// ===== I-Kmeans =====

void ikmeans(const Interval *const *elements, Interval **centers, unsigned *asso,
             const unsigned nb_elements, const unsigned nb_clusters, const unsigned nb_interval,
             const Distance dist, const bool trace, const unsigned max_iter, double *withinss,
             double *tot, double *totwss, unsigned *iter) {
    unsigned iteration = 0;
    double totwss_pre;
    *totwss = INFINITY;

    do {
        iteration++;
        totwss_pre = *totwss;

        // Assign all elements to a class
        ik_assign(elements, (const Interval *const *)centers, asso, nb_elements, nb_clusters,
                  nb_interval, dist, withinss);
        const double va = sum_double_array(withinss, nb_clusters);

        // Update all centers
        ik_update(elements, centers, (const unsigned *)asso, nb_elements, nb_clusters, nb_interval,
                  dist, withinss);
        *totwss = sum_double_array(withinss, nb_clusters);

        PRINT_ITER(trace, iteration, va, *totwss);
    } while (iteration < max_iter && totwss_pre > *totwss);  ///< While is not stable

    *tot =
        ik_getBetweenss((const Interval *const *)centers, nb_clusters, nb_interval, dist) + *totwss;
    *iter = iteration;
}
