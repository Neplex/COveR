#ifndef ICMEANS_H
#define ICMEANS_H

#include <stdbool.h>

#include "distance.h"
#include "interval.h"

/**
 * @brief Fuzzy I-Cmeans (fuzzy cmeans for interval data)
 * @param elements the elements to compute
 * @param centers the centers of clusters
 * @param asso a double matrix to associate element with weight in class
 * @param nb_elements the number of elements
 * @param nb_clusters the number of clusters
 * @param nb_interval the number of interval
 * @param m the degree of fuzzification
 * @param dist the distance to use
 * @param trace show trace ?
 * @param max_iter the maximum number of iteration
 * @param withinss a container to return withinss
 * @param tot a container to return total sum of square
 * @param totwss a container to return total withinss
 * @param iter a container to return the number of iteration
 */
void icmeans(const Interval *const *elements, Interval **centers, double **asso,
             unsigned nb_elements, unsigned nb_clusters, unsigned nb_interval, double m,
             Distance dist, bool trace, unsigned max_iter, double *withinss, double *tot,
             double *totwss, unsigned *iter);

#endif
