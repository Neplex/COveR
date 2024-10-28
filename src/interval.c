#include "interval.h"

double get_center(const Interval i) { return (i.min + i.max) * .5; }

double get_half_size(const Interval i) { return (i.max - i.min) * .5; }
