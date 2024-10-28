#include "neokm.h"

#include <stdbool.h>

#include "distance.h"
#include "helpers.h"

// == Betwennss ==

double neo_betweenss(const double **centers, const unsigned nb_clusters, const unsigned nb_dim) {
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

void neo_assign(const double *const *elements, const double **centers, bool **asso,
                const unsigned nb_elements, const unsigned nb_clusters, const unsigned nb_dim,
                const double alpha, const double beta, double *withinss) {
    bool T[nb_elements][nb_clusters];
    bool S[nb_elements];
    double dists[nb_elements][nb_clusters];
    unsigned p = 0;

    // Init arrays
    memset(withinss, 0, nb_clusters * sizeof(double));
    memset(S, false, nb_elements * sizeof(bool));

    for (unsigned i = 0; i < nb_elements; i++) {
        memset(T[i], false, nb_clusters * sizeof(bool));
        memset(asso[i], false, nb_clusters * sizeof(bool));

        // Compute distances between every data point and clusters
        for (unsigned k = 0; k < nb_clusters; k++) {
            dists[i][k] = vector_square_distance(elements[i], centers[k], nb_dim);
        }
    }

    while (p < (nb_elements + alpha * nb_elements)) {
        unsigned i = 0;
        unsigned j = 0;
        double dist = INFINITY;

        if (p < (nb_elements - beta * nb_elements)) {
            // find minimum distance between an element and a cluster
            for (unsigned l = 0; l < nb_elements; l++) {
                for (unsigned k = 0; k < nb_clusters; k++) {
                    if (dists[l][k] < dist && !T[l][k] && !S[l]) {
                        dist = dists[l][k];
                        i = l;
                        j = k;
                    }
                }
            }

            asso[i][j] = true;
            S[i] = true;
        } else {
            // find minimum distance between an element and a cluster
            for (unsigned l = 0; l < nb_elements; l++) {
                for (unsigned k = 0; k < nb_clusters; k++) {
                    if (dists[l][k] < dist && !T[l][k]) {
                        dist = dists[l][k];
                        i = l;
                        j = k;
                    }
                }
            }

            asso[i][j] = true;
        }

        T[i][j] = true;
        withinss[j] += dist;
        p++;
    }
}

void neo_update(const double *const *elements, double **centers, const bool **asso,
                const unsigned nb_elements, const unsigned nb_clusters, const unsigned nb_dim,
                double *withinss) {
    // Update cluster by cluster
    for (unsigned k = 0; k < nb_clusters; k++) {
        withinss[k] = 0;

        // Update dim by dim
        for (unsigned j = 0; j < nb_dim; j++) {
            double sum = 0;        ///< The sum of elements in cluster k
            unsigned nb_elem = 0;  ///< The Number of elements in cluster k

            // For all elements in cluster k
            for (unsigned i = 0; i < nb_elements; i++) {
                if (asso[i][k]) {
                    sum += elements[i][j];
                    nb_elem++;
                }
            }

            // Update center
            centers[k][j] = nb_elem > 0 ? sum / nb_elem : NAN;
        }

        // Update withinss
        for (unsigned i = 0; i < nb_elements; i++) {
            if (asso[i][k]) {
                withinss[k] += vector_square_distance(elements[i], centers[k], nb_dim);
            }
        }
    }
}

// == NEOKM ==

void neokm(const double *const *elements, double **centers, bool **asso, const unsigned nb_elements,
           const unsigned nb_clusters, const unsigned nb_dim, const double alpha, const double beta,
           const bool trace, const unsigned max_iter, double *withinss, double *tot, double *totwss,
           unsigned *iter) {
    unsigned iteration = 0;
    double totwss_pre;
    *totwss = INFINITY;

    do {
        iteration++;
        totwss_pre = *totwss;

        // Assign all elements to a class
        neo_assign(elements, (const double **)centers, asso, nb_elements, nb_clusters, nb_dim,
                   alpha, beta, withinss);
        const double va = sum_double_array(withinss, nb_clusters);

        // Update all centers
        neo_update(elements, centers, (const bool **)asso, nb_elements, nb_clusters, nb_dim,
                   withinss);
        *totwss = sum_double_array(withinss, nb_clusters);

        PRINT_ITER(trace, iteration, va, *totwss);
    } while (iteration < max_iter && totwss_pre > *totwss);  ///< While is not stable

    *tot = *totwss + neo_betweenss((const double **)centers, nb_clusters, nb_dim);
    *iter = iteration;
}
