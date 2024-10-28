#ifndef IOKM_H
#define IOKM_H

#include "distance.h"
#include "helpers.h"

/**
 * @brief I-OKM (overlapping kmeans for interval data)
 * @param elements the elements to compute
 * @param centers the centers of clusters
 * @param asso a boolean matrix to associate element with class
 * @param nb_elements the number of elements
 * @param nb_clusters the number of clusters
 * @param nb_interval the number of interval
 * @param dist the distance to use
 * @param algo the algorithm to use
 * @param up the update to use
 * @param trace show trace ?
 * @param max_iter the maximum number of iteration
 * @param need_valid interval need to be valid ?
 * @param withinss a container to return withinss
 * @param tot a container to return total sum of square
 * @param totwss a container to return total withinss
 * @param iter a container to return the number of iteration
 */
void iokm(const Interval *const *elements, Interval **centers, bool **asso, unsigned nb_elements,
          unsigned nb_clusters, unsigned nb_interval, Distance dist, Algorithm algo, Update up,
          bool trace, unsigned max_iter, bool need_valid, double *withinss, double *tot,
          double *totwss, unsigned *iter);

#endif
