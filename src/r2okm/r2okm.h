#ifndef R2OKM_H
#define R2OKM_H

#include <stdbool.h>

/**
 * @brief R2-OKM
 * @param elements the elements to compute
 * @param centers the centers of clusters
 * @param asso an boolean matrix to associate element with class
 * @param nb_elements the number of elements
 * @param nb_clusters the number of clusters
 * @param nb_dim the number of dim
 * @param lambda
 * @param trace show trace ?
 * @param max_iter the maximum number of iteration
 * @param withinss a container to return withinss
 * @param tot a container to return total sum of square
 * @param totwss a container to return total withinss
 * @param iter a container to return the number of iteration
 */
void r2okm(const double *const *elements, double **centers, bool **asso, unsigned nb_elements,
           unsigned nb_clusters, unsigned nb_dim, double lambda, bool trace, unsigned max_iter,
           double *withinss, double *tot, double *totwss, unsigned *iter);

#endif
