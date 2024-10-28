#include "io_euclid.h"

#include <stdbool.h>

#include "euclid/io_euclid_join.h"
#include "euclid/io_euclid_mean.h"
#include "euclid/io_euclid_meet.h"
#include "euclid/io_euclid_sum.h"
#include "helpers.h"
#include "interval.h"

double io_euclid_distanceToClusters(const Interval *elem, const Interval *const *centers,
                                    const bool *asso, const unsigned nb_clusters,
                                    const unsigned nb_interval, const Update up) {
    double res = 0;

    switch (up) {
        case MEAN:
            res = io_euclid_mean_distanceToClusters(elem, centers, asso, nb_clusters, nb_interval);
            break;

        case SUM:
            res = io_euclid_sum_distanceToClusters(elem, centers, asso, nb_clusters, nb_interval);
            break;

        case JOIN:
            res = io_euclid_join_distanceToClusters(elem, centers, asso, nb_clusters, nb_interval);
            break;

        case MEET:
            res = io_euclid_meet_distanceToClusters(elem, centers, asso, nb_clusters, nb_interval);
            break;
    }

    return res;
}

void io_euclid_update(const Interval *const *elements, Interval **centers, bool **asso,
                      const unsigned nb_elements, const unsigned nb_clusters,
                      const unsigned nb_interval, const Algorithm algo, const Update up,
                      const bool need_valid, double *withinss) {
    switch (up) {
        case MEAN:
            io_euclid_mean_update(elements, centers, asso, nb_elements, nb_clusters, nb_interval,
                                  algo, need_valid, withinss);
            break;

        case SUM:
            io_euclid_sum_update(elements, centers, asso, nb_elements, nb_clusters, nb_interval,
                                 algo, need_valid, withinss);
            break;

        case JOIN:
            io_euclid_join_update(elements, centers, asso, nb_elements, nb_clusters, nb_interval,
                                  algo, need_valid, withinss);
            break;

        case MEET:
            io_euclid_meet_update(elements, centers, asso, nb_elements, nb_clusters, nb_interval,
                                  algo, need_valid, withinss);
            break;
    }
}
