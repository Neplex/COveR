#ifndef DISTANCE_H
#define DISTANCE_H

#include "interval.h"

typedef enum { EUCLIDEAN, HAUSDORFF } Distance;

double vector_square_distance(const double *v1, const double *v2, unsigned nb_dim);

double square_distance(const Interval *r1, const Interval *r2, unsigned nb_interval);

double haus_distance(const Interval *r1, const Interval *r2, unsigned nb_interval);

#endif
