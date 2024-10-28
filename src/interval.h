#ifndef INTERVAL_H
#define INTERVAL_H

/**
 * @brief Struct to store an interval (min:double, max:double)
 * TODO: float are better (less memory, faster to compute), but error is too big
 */
typedef struct {
    double min;
    double max;
} Interval;

double get_center(Interval i);

double get_half_size(Interval i);

#endif
