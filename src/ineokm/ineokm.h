#ifndef INEOKM_H
#define INEOKM_H

#include <stdbool.h>

#include "distance.h"
#include "interval.h"

/**
 * @brief I-NEOKM (NEOKM for interval data)
 * @param elements the elements to compute
 * @param centers the centers of clusters
 * @param asso an unsigned array to associate elements with class
 * @param nb_elements the number of elements
 * @param nb_clusters the number of clusters
 * @param nb_interval the number of interval
 * @param trace show trace ?
 * @param max_iter the maximum number of iteration
 * @param withinss a container to return withinss
 * @param tot a container to return total sum of square
 * @param totwss a container to return total withinss
 * @param iter a container to return the number of iteration
 */
void ineokm(const Interval *const *elements, Interval **centers, bool **asso, unsigned nb_elements,
            unsigned nb_clusters, unsigned nb_interval, double alpha, double beta, bool trace,
            unsigned max_iter, double *withinss, double *tot, double *totwss, unsigned *iter);

#endif
