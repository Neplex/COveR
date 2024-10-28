#include "iokm.h"

#include "distance.h"
#include "dists/io_euclid.h"
#include "dists/io_hausdorff.h"
#include "helpers.h"
#include "interval.h"

// ===== Functions =====

void io_assign(const Interval *const *elements, Interval **centers, bool **asso,
               unsigned nb_elements, unsigned nb_clusters, unsigned nb_interval, Distance dist,
               Update up, double *withinss) {
    // Assign element by element
    for (unsigned i = 0; i < nb_elements; i++) {
        double new_dist = INFINITY, next_new_dist;
        bool end;  // False if new cluster is add to asso

        // Future association if is better
        bool new_asso[nb_clusters];
        memset(new_asso, false, nb_clusters * sizeof(bool));

        // Pre-process distance between element and clusters center
        double dists[nb_clusters];
        for (unsigned k = 0; k < nb_clusters; k++) switch (dist) {
                case EUCLIDEAN:
                    dists[k] = square_distance(elements[i], centers[k], nb_interval);
                    break;

                case HAUSDORFF:
                    dists[k] = haus_distance(elements[i], centers[k], nb_interval);
                    break;
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
                    double d = dists[k];

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

            switch (dist) {
                case HAUSDORFF:
                    next_new_dist = io_hausdorff_distanceToClusters(
                        elements[i], (const Interval *const *)centers, tmp_asso, nb_clusters,
                        nb_interval, up);
                    break;

                case EUCLIDEAN:
                default:
                    next_new_dist =
                        io_euclid_distanceToClusters(elements[i], (const Interval *const *)centers,
                                                     tmp_asso, nb_clusters, nb_interval, up);
                    break;
            }

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

void io_update(const Interval *const *elements, Interval **centers, bool **asso,
               unsigned nb_elements, unsigned nb_clusters, unsigned nb_interval, Distance dist,
               Algorithm algo, Update up, bool need_valid, double *withinss) {
    switch (dist) {
        case HAUSDORFF:
            io_hausdorff_update(elements, centers, asso, nb_elements, nb_clusters, nb_interval,
                                algo, up, need_valid, withinss);
            break;

        case EUCLIDEAN:
            io_euclid_update(elements, centers, asso, nb_elements, nb_clusters, nb_interval, algo,
                             up, need_valid, withinss);
            break;
    }
}

double io_getBetweenss(Interval **centers, unsigned nb_clusters, unsigned nb_interval,
                       Distance dist) {
    double res = 0;
    // For all clusters
    for (unsigned k = 0; k < nb_clusters; k++) {
        // Get the mean element of other clusters center
        Interval *mean = new_array_Interval(nb_interval);
        for (unsigned j = 0; j < nb_interval; j++) {
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

        delete_array((void *)&mean);
    }

    return res;
}

// ===== I-OKM =====

void iokm(const Interval *const *elements, Interval **centers, bool **asso, unsigned nb_elements,
          unsigned nb_clusters, unsigned nb_interval, Distance dist, Algorithm algo, Update up,
          bool trace, unsigned max_iter, bool need_valid, double *withinss, double *tot,
          double *totwss, unsigned *iter) {
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

        // Assign all element to a class
        io_assign(elements, centers, asso, nb_elements, nb_clusters, nb_interval, dist, up,
                  withinss);
        double va = sum_double_array(withinss, nb_elements);

        // Update all centers
        io_update(elements, centers, asso, nb_elements, nb_clusters, nb_interval, dist, algo, up,
                  need_valid, withinss);
        *totwss = sum_double_array(withinss, nb_elements);

        PRINT_ITER(trace, iteration, va, *totwss);
    } while (iteration < max_iter && totwss_pre > *totwss);  // While is not stable

    *tot = io_getBetweenss(centers, nb_clusters, nb_interval, dist) + *totwss;
    *iter = iteration;
}
