#include "icmeans.h"

#include "dists/ic_euclid.h"
#include "dists/ic_hausdorff.h"
#include "helpers.h"

// ===== Functions =====

void ic_assign(const Interval *const *elements, Interval **centers, double **asso,
               const unsigned nb_elements, const unsigned nb_clusters, const unsigned nb_interval,
               const double m, const Distance dist, double *withinss) {
    // Assign element by element
    for (unsigned i = 0; i < nb_elements; i++) {
        double dists[nb_clusters];

        // For all clusters compute distance
        for (unsigned k = 0; k < nb_clusters; k++) {
            switch (dist) {
                case EUCLIDEAN:
                    dists[k] = square_distance(elements[i], centers[k], nb_interval);
                    break;

                case HAUSDORFF:
                    dists[k] = haus_distance(elements[i], centers[k], nb_interval);
                    break;
            }
        }

        // For all clusters update weigh
        for (unsigned k = 0; k < nb_clusters; k++) {
            if (!dists[k]) {
                // Element i equal to class k
                asso[i][k] = 1;
            } else {
                double sum = 0;

                // For all clusters
                for (unsigned l = 0; l < nb_clusters; l++) {
                    if (dists[l]) {
                        sum += pow(dists[k] / dists[l], 1.0 / (m - 1));
                    } else {
                        // Element i equal to cluster l
                        sum = 0;
                        break;
                    }
                }

                asso[i][k] = sum > 0 ? 1.0 / sum : 0;

                // Update withinss
                withinss[k] += dists[k] * pow(asso[i][k], m);
            }
        }
    }
}

void ic_update(const Interval *const *elements, Interval **centers, double **asso,
               const unsigned nb_elements, const unsigned nb_clusters, const unsigned nb_interval,
               const double m, const Distance dist, double *withinss) {
    switch (dist) {
        case HAUSDORFF:
            ic_hausdorff_update(elements, centers, asso, nb_elements, nb_clusters, nb_interval, m,
                                withinss);
            break;

        case EUCLIDEAN:
            ic_euclid_update(elements, centers, asso, nb_elements, nb_clusters, nb_interval, m,
                             withinss);
            break;
    }
}

double ic_getBetweenss(Interval **centers, const unsigned nb_clusters, const unsigned nb_interval,
                       const Distance dist) {
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

            mean[j].min /= (nb_clusters - 1);
            mean[j].max /= (nb_clusters - 1);
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

// ===== I-Cmeans =====

void icmeans(const Interval *const *elements, Interval **centers, double **asso,
             const unsigned nb_elements, const unsigned nb_clusters, const unsigned nb_interval,
             const double m, const Distance dist, const bool trace, const unsigned max_iter,
             double *withinss, double *tot, double *totwss, unsigned *iter) {
    unsigned iteration = 0;
    double totwss_pre;
    *totwss = INFINITY;

    do {
        iteration++;
        totwss_pre = *totwss;

        // Assign all elements to a class
        ic_assign(elements, centers, asso, nb_elements, nb_clusters, nb_interval, m, dist,
                  withinss);
        const double va = sum_double_array(withinss, nb_clusters);

        // Update all centers
        ic_update(elements, centers, asso, nb_elements, nb_clusters, nb_interval, m, dist,
                  withinss);
        *totwss = sum_double_array(withinss, nb_clusters);

        PRINT_ITER(trace, iteration, va, *totwss);
    } while (iteration < max_iter && (totwss_pre - *totwss) > 1E-6);  ///< While is not stable

    *tot = ic_getBetweenss(centers, nb_clusters, nb_interval, dist) + *totwss;
    *iter = iteration;
}
