#include "distance.h"

#include <math.h>

#include "interval.h"

double vector_square_distance(const double *v1, const double *v2, const unsigned nb_dim) {
    double dist = 0.0;

    for (unsigned j = 0; j < nb_dim; j++) {
        dist += pow(v1[j] - v2[j], 2);
    }

    return dist;
}

double square_distance(const Interval *r1, const Interval *r2, const unsigned nb_interval) {
    double dist = 0.0;

    for (unsigned i = 0; i < nb_interval; i++) {
        dist += pow(r1[i].min - r2[i].min, 2) + pow(r1[i].max - r2[i].max, 2);
    }

    return dist;
}

double haus_distance(const Interval *r1, const Interval *r2, const unsigned nb_interval) {
    double dist = 0.0;

    for (unsigned i = 0; i < nb_interval; i++) {
        dist += fabs(get_center(r1[i]) - get_center(r2[i])) +
                fabs(get_half_size(r1[i]) - get_half_size(r2[i]));
    }

    return dist;
}
