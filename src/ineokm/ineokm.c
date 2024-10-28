#include "ineokm.h"

#include <math.h>
#include <stdbool.h>

#include "distance.h"
#include "helpers.h"
#include "interval.h"

double ineo_betweenss(const Interval *const *centers, const unsigned nb_clusters,
                      const unsigned nb_interval) {
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
        res += square_distance(centers[k], mean, nb_interval);

        delete_array((void **)&mean);
    }

    return res;
}

void ineo_assign(const Interval *const *elements, Interval **centers, bool **asso,
                 const unsigned nb_elements, const unsigned nb_clusters, const unsigned nb_interval,
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
            dists[i][k] = square_distance(elements[i], centers[k], nb_interval);
        }
    }

    while (p < nb_elements + alpha * nb_elements) {
        unsigned i = 0;
        unsigned j = 0;
        double dist = INFINITY;

        if (p < nb_elements - beta * nb_elements) {
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

void ineo_update(const Interval *const *elements, Interval **centers, const bool **asso,
                 const unsigned nb_elements, const unsigned nb_clusters, const unsigned nb_interval,
                 double *withinss) {
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
                if (asso[i][k]) {
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
            if (asso[i][k]) {
                withinss[k] += square_distance(elements[i], centers[k], nb_interval);
            }
        }
    }
}

void ineokm(const Interval *const *elements, Interval **centers, bool **asso,
            const unsigned nb_elements, const unsigned nb_clusters, const unsigned nb_interval,
            const double alpha, const double beta, const bool trace, const unsigned max_iter,
            double *withinss, double *tot, double *totwss, unsigned *iter) {
    unsigned iteration = 0;
    double totwss_pre;
    *totwss = INFINITY;

    do {
        iteration++;
        totwss_pre = *totwss;

        // Assign all elements to a class
        ineo_assign(elements, centers, asso, nb_elements, nb_clusters, nb_interval, alpha, beta,
                    withinss);
        const double va = sum_double_array(withinss, nb_clusters);

        // Update all centers
        ineo_update(elements, centers, (const bool **)asso, nb_elements, nb_clusters, nb_interval,
                    withinss);
        *totwss = sum_double_array(withinss, nb_clusters);

        PRINT_ITER(trace, iteration, va, *totwss);
    } while (iteration < max_iter && totwss_pre > *totwss);  ///< While is not stable

    *tot = *totwss + ineo_betweenss((const Interval *const *)centers, nb_clusters, nb_interval);
    *iter = iteration;
}
