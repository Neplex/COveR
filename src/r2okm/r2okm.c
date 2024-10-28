#include "r2okm.h"

#include <stdbool.h>

#include "distance.h"
#include "helpers.h"

// == Betwennss ==

double r2_betweenss(double **centers, unsigned nb_clusters, unsigned nb_dim) {
    double res = 0;

    // For all clusters
    for (unsigned k = 0; k < nb_clusters; k++) {
        // Get the mean element of other clusters center
        double *mean = new_array_double(nb_dim);
        for (unsigned j = 0; j < nb_dim; j++) {
            for (unsigned i = 0; i < nb_clusters; i++) {
                if (i != k) {
                    mean[j] += centers[i][j];
                }
            }
            mean[j] /= nb_clusters;
        }

        // Sum distance
        res += vector_square_distance(centers[k], mean, nb_dim);

        delete_array((void **)&mean);
    }

    return res;
}

// == Assign & Update ==

double r2_distanceToClusters(const double *elem, double **centers, const bool *asso,
                             unsigned nb_clusters, unsigned nb_dim, double lambda) {
    double mean_prototype[nb_dim];
    double tmp = 0.0;
    unsigned nbc = 0;

    // Compute the sum of distance between element and all associate cluster
    for (unsigned k = 0; k < nb_clusters; k++) {
        if (asso[k]) {
            nbc++;
            tmp += vector_square_distance(elem, centers[k], nb_dim);
        }
    }

    // Compute the mean prototype og associate cluster
    for (unsigned j = 0; j < nb_dim; j++) {
        mean_prototype[j] = 0;

        // For all associated clusters
        for (unsigned k = 0; k < nb_clusters; k++) {
            if (asso[k]) {
                mean_prototype[j] += centers[k][j];
            }
        }

        mean_prototype[j] = (nbc) ? mean_prototype[j] / nbc : INFINITY;
    }

    return vector_square_distance(elem, mean_prototype, nb_dim) + lambda * (tmp / nbc);
}

void r2_assign(const double *const *elements, double **centers, bool **asso, unsigned nb_elements,
               unsigned nb_clusters, unsigned nb_dim, double lambda, double *withinss) {
    // Assign element by element
    for (unsigned i = 0; i < nb_elements; i++) {
        double new_dist = INFINITY;
        bool end;  // False if new cluster is add to asso

        // Future association if is better
        bool new_asso[nb_clusters];
        memset(new_asso, false, nb_clusters * sizeof(bool));

        // Pre-process distance between element and clusters center
        double dists[nb_clusters];
        for (unsigned k = 0; k < nb_clusters; k++) {
            dists[k] = vector_square_distance(elements[i], centers[k], nb_dim);
        }

        unsigned nb_clusters_check = 0;

        do {
            double min_dist = INFINITY;
            unsigned ck = 0;  ///< The next closest cluster
            end = true;

            nb_clusters_check++;

            // Search the closest cluster, that not associated
            for (unsigned k = 0; k < nb_clusters; k++) {
                if (!new_asso[k]) {
                    const double d = dists[k];

                    if (d < min_dist) {
                        min_dist = d;
                        ck = k;
                    }
                }
            }

            // Next new asso if is better with cluster ck
            bool tmp_asso[nb_clusters];
            copy_array(new_asso, tmp_asso, nb_clusters);
            tmp_asso[ck] = true;

            double next_new_dist =
                r2_distanceToClusters(elements[i], centers, tmp_asso, nb_clusters, nb_dim, lambda);

            // If with the new cluster the result is better, add it to new
            // association and search for the closest cluster again
            if (next_new_dist < new_dist) {
                new_asso[ck] = true;       // Add new cluster
                new_dist = next_new_dist;  // Save distance
                end = false;               // Not the end
            }
        } while (!end && nb_clusters_check <= nb_clusters);

        // If the new association is better than previous iteration, choose it
        if (new_dist <= withinss[i]) {
            copy_array(new_asso, asso[i], nb_clusters);  // save asso
            withinss[i] = new_dist;                      // save withinss
        }
    }
}

void r2_update(const double *const *elements, double **centers, bool **asso, unsigned nb_elements,
               unsigned nb_clusters, unsigned nb_dim, double lambda, double *withinss) {
    // Get number of associate clusters by elements and compute weight
    unsigned nb_asso[nb_elements];
    double weight[nb_elements];
    double penality[nb_elements];
    for (unsigned i = 0; i < nb_elements; i++) {
        nb_asso[i] = 0;
        for (unsigned k = 0; k < nb_clusters; k++) {
            nb_asso[i] += asso[i][k];
        }
        weight[i] = 1.0 / pow(nb_asso[i], 2);
        penality[i] = lambda / nb_asso[i];
    }

    // Update cluster by cluster
    for (unsigned k = 0; k < nb_clusters; k++) {
        // Update dim by dim
        for (unsigned j = 0; j < nb_dim; j++) {
            double res = 0.0;
            double ws = 0.0;

            // For all elements in cluster
            for (unsigned i = 0; i < nb_elements; i++) {
                if (asso[i][k]) {
                    double tmp = elements[i][j];

                    // Compute x^i
                    tmp *= nb_asso[i];
                    for (unsigned l = 0; l < nb_clusters; l++) {
                        if (asso[i][l] && l != k) {
                            tmp -= centers[l][j];
                        }
                    }

                    // Compute sums
                    ws += weight[i] + penality[i];
                    res += tmp * weight[i] + penality[i] * elements[i][j];
                }
            }

            // Compute Weighted mean
            centers[k][j] = res / ws;
        }
    }

    // Update withinss
    for (unsigned i = 0; i < nb_elements; i++) {
        withinss[i] =
            r2_distanceToClusters(elements[i], centers, asso[i], nb_clusters, nb_dim, lambda);
    }
}

// == R2-OKM ==

void r2okm(const double *const *elements, double **centers, bool **asso, const unsigned nb_elements,
           const unsigned nb_clusters, const unsigned nb_dim, const double lambda, const bool trace,
           const unsigned max_iter, double *withinss, double *tot, double *totwss, unsigned *iter) {
    unsigned iteration = 0;
    double totwss_pre;

    // Initialization
    *totwss = INFINITY;
    for (unsigned i = 0; i < nb_elements; i++) {
        withinss[i] = INFINITY;
    }

    do {
        iteration++;
        totwss_pre = *totwss;

        // Assign all elements to a class
        r2_assign(elements, centers, asso, nb_elements, nb_clusters, nb_dim, lambda, withinss);
        const double va = sum_double_array(withinss, nb_elements);

        // Update all centers
        r2_update(elements, centers, asso, nb_elements, nb_clusters, nb_dim, lambda, withinss);
        *totwss = sum_double_array(withinss, nb_elements);

        PRINT_ITER(trace, iteration, va, *totwss);
    } while (iteration < max_iter && totwss_pre > *totwss);  ///< While is not stable

    *tot = *totwss + r2_betweenss(centers, nb_clusters, nb_dim);
    *iter = iteration;
}
