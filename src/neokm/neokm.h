#ifndef __NEOKM_H
#define __NEOKM_H

#include "../helpers.h"

// == Betwennss ==

double neo_betweenss(double **centers, unsigned nb_clusters, unsigned nb_dim) {

  double res = 0;

  // For all clusters
  for (size_t k = 0; k < nb_clusters; k++) {

    // Get the mean element of other clusters center
    double* mean = new_array_double(nb_dim);
    for (size_t j = 0; j < nb_dim; j++) {
      for (size_t i = 0; i < nb_clusters; i++) {
        if (i != k) {
          mean[j] += centers[i][j];
        }
      }
      mean[j] /= nb_clusters;
    }

    // Sum distance
    res += vector_square_distance(centers[k], mean, nb_dim);

    delete_array(&mean);
  }

  return res;
}

// == Assign & Update ==

void neo_assign(double **elements, double **centers, bool **asso,
                unsigned nb_elements, unsigned nb_clusters, unsigned nb_dim,
                double alpha, double beta, double *withinss) {

  // Clear withinss
  for (size_t k = 0; k < nb_clusters; k++) {
    withinss[k] = 0;
  }

  // Compute distances between every data point and clusters
  double dists[nb_elements][nb_clusters];
  for (size_t i = 0; i < nb_elements; i++) {
    for (size_t k = 0; k < nb_clusters; k++) {
      dists[i][k] = vector_square_distance(elements[i], centers[k], nb_dim);
    }
  }

  bool T[nb_elements][nb_clusters];
  bool S[nb_elements];
  unsigned p = 0;

  // Init arrays
  for (size_t i = 0; i < nb_elements; i++) {
    for (size_t k = 0; k < nb_clusters; k++) {
      asso[i][k] = false;
      T[i][k] = false;
    }
    S[i] = false;
  }

  while (p < (nb_elements + alpha * nb_elements)) {

    unsigned i, j;
    double dist = INFINITY;

    if (p < (nb_elements - beta * nb_elements)) {

      // find minimum distance between an element and a cluster
      for (size_t l = 0; l < nb_elements; l++) {
        for (size_t k = 0; k < nb_clusters; k++) {
          if (dists[l][k] < dist && !T[l][k] && !S[l]) {
            dist = dists[l][k];
            i = l;
            j = k;
          }
        }
      }

      asso[i][j] = true;
      S[i] = true;

    } else {

      // find minimum distance between an element and a cluster
      for (size_t l = 0; l < nb_elements; l++) {
        for (size_t k = 0; k < nb_clusters; k++) {
          if (dists[l][k] < dist && !T[l][k]) {
            dist = dists[l][k];
            i = l;
            j = k;
          }
        }
      }

      asso[i][j] = true;
    }

    T[i][j] = true;
    withinss[j] += dist;
    p++;
  }
}

void neo_update(double **elements, double **centers, bool **asso,
                unsigned nb_elements, unsigned nb_clusters, unsigned nb_dim,
                double *withinss) {

  // Update cluster by cluster
  for (size_t k = 0; k < nb_clusters; k++) {
    withinss[k] = 0;

    // Update dim by dim
    for (size_t j = 0; j < nb_dim; j++) {

      double sum = 0;       ///< The sum of elements in cluster k
      unsigned nb_elem = 0; ///< The Number of elements in cluster k

      // For all elemets in cluster k
      for (size_t i = 0; i < nb_elements; i++) {
        if (asso[i][k]) {

          sum += elements[i][j];
          nb_elem++;
        }
      }

      // Update center
      centers[k][j] = (nb_elem) ? sum / nb_elem : NAN;
    }

    // Update withinss
    for (size_t i = 0; i < nb_elements; i++) {
      if (asso[i][k]) {
        withinss[k] += vector_square_distance(elements[i], centers[k], nb_dim);
      }
    }
  }
}

// == NEOKM ==

/**
 * @brief NEOKM (Non-exhaustive overlapping kmeans)
 * @param elements the elements to compute
 * @param centers the centers of clusters
 * @param asso an boolean matrix to associate element with class
 * @param nb_elements the number of elements
 * @param nb_clusters the number of clusters
 * @param nb_dim the number of dim
 * @param alpha (overlap)
 * @param beta (non-exhaustiveness)
 * @param trace show trace ?
 * @param max_iter the maximum number of iteration
 * @param withinss a container to return withinss
 * @param tot a container to return total sum of square
 * @param totwss a container to return total withinss
 * @param iter a container to return the number of iteration
 */
void neokm(double **elements, double **centers, bool **asso,
           unsigned nb_elements, unsigned nb_clusters, unsigned nb_dim,
           double alpha, double beta, bool trace, unsigned max_iter,
           double *withinss, double *tot, double *totwss,
           unsigned short *iter) {

  unsigned short i = 0; ///< The current iteration
  double totwss_pre;
  *totwss = INFINITY;

  do {
    i++;
    totwss_pre = *totwss;

    // Assign all elements to a class
    neo_assign(elements, centers, asso, nb_elements, nb_clusters, nb_dim, alpha,
               beta, withinss);
    double va = sum_double_array(withinss, nb_clusters);

    // Update all centers
    neo_update(elements, centers, asso, nb_elements, nb_clusters, nb_dim,
               withinss);
    *totwss = sum_double_array(withinss, nb_clusters);

    PRINT_ITER(trace, i, va, *totwss);

  } while (i < max_iter && totwss_pre > *totwss); ///< While is not stable

  *tot = *totwss + neo_betweenss(centers, nb_clusters, nb_dim);
  *iter = i;
}

#endif
