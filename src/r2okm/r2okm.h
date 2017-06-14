#ifndef __R2OKM_H
#define __R2OKM_H

#include "../helpers.h"

// == Betwennss ==

double r2_betweenss(double **centers, unsigned nb_clusters, unsigned nb_dim) {

  double res = 0;

  // For all clusters
  for (size_t k = 0; k < nb_clusters; k++) {

    // Get the mean element of other clusters center
    double mean[nb_dim];
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
  }

  return res;
}

// == Assign & Update ==

double r2_distanceToClusters(double *elem, double **centers, bool *asso,
                             unsigned nb_clusters, unsigned nb_dim,
                             double lambda) {
  double mean_prototype[nb_dim]

      // For all dim
      for (size_t j = 0; j < nb_dim; j++) {
    mean_prototype[j] = 0;
    unsigned nbc = 0;

    // For all associated clusters
    for (size_t k = 0; k < nb_clusters; k++) {
      if (asso[k]) {
        mean_prototype[j] += centers[k][j];
        nbc++;
      }
    }

    mean_prototype[j] = (nbc) ? mean_prototype[j] / nbc : INFINITY;
  }

  return vector_square_distance(elem, mean_prototype, nb_dim);
}

void r2_assign(double **elements, double **centers, bool **asso,
               unsigned nb_elements, unsigned nb_clusters, unsigned nb_dim,
               double lambda, double *withinss) {

  // Assign element by element
  for (size_t i = 0; i < nb_elements; i++) {
    double new_dist = INFINITY, next_new_dist;
    bool end; // False if new cluster is add to asso

    // Future association if is better
    bool new_asso[nb_clusters];
    for (size_t k = 0; k < nb_clusters; k++)
      new_asso[k] = false;

    // Pre process distance between element and clusters center
    double dists[nb_clusters];
    for (size_t k = 0; k < nb_clusters; k++) {
      dists[k] = vector_square_distance(elements[i], centers[k], nb_dim);
    }

    do {
      double min_dist = INFINITY;
      unsigned ck = 0; ///< The next closest cluster
      end = true;

      // Search closest cluster, that not associated
      for (size_t k = 0; k < nb_clusters; k++) {
        if (!new_asso[k]) {
          double d = dists[k];

          if (d < min_dist) {
            min_dist = d;
            ck = k;
          }
        }
      }

      // Next new asso if is better with cluster ck
      bool tmp_asso[nb_clusters];
      copy_array(new_asso, tmp_asso, nb_clusters);
      tmp_asso[ck] = true;

      next_new_dist = r2_distanceToClusters(elements[i], centers, tmp_asso,
                                            nb_clusters, nb_dim, lambda);

      // If with the new cluster the result is better, add it to new association
      // and search for the closest cluster again
      if (next_new_dist < new_dist) {
        new_asso[ck] = true;      // Add new cluster
        new_dist = next_new_dist; // Save distance
        end = false;              // Not the end
      }

    } while (!end);

    // If the new association is better than previous iteration, choose it
    if (new_dist <= withinss[i]) {
      copy_array(new_asso, asso[i], nb_clusters); // save asso
      withinss[i] = new_dist;                     // save withinss
    }
  }
}

void r2_update(double **elements, double **centers, bool **asso,
               unsigned nb_elements, unsigned nb_clusters, unsigned nb_dim,
               double lambda, double *withinss) {

  // Get number of associate clusters by elements
  unsigned nb_asso[nb_elements];
  for (size_t i = 0; i < nb_elements; i++) {
    nb_asso[i] = 0;
    for (size_t j = 0; j < nb_clusters; j++) {
      nb_asso[i] += asso[i][j];
    }
  }

  // Update cluster by cluster
  for (size_t k = 0; k < nb_clusters; k++) {
    double res = 0; ///< Sum of weight

    // New center for cluster k
    double center[nb_dim];
    for (size_t j = 0; j < nb_dim; j++) {
      center[j] = 0;
    }

    // For all elements in cluster
    for (size_t i = 0; i < nb_elements; i++) {
      if (asso[i][k]) {

        double weight = 1.0 / sqr(nb_asso[i]);
        res += weight;

        double copy[nb_dim];
        copy_array(elements[i], copy, nb_dim);

        for (size_t j = 0; j < nb_dim; j++) {
          copy[j] *= nb_asso[i];

          for (size_t l = 0; l < nb_clusters; l++) {
            if (asso[i][l] && l != k) {
              copy[j] -= centers[l][j];
            }
          }

          copy[j] *= weight;
          center[j] += copy[j];
        }
      }
    }

    // Weighted mean
    for (size_t j = 0; j < nb_dim; j++) {
      center[j] /= res;
    }

    copy_array(center, centers[k], nb_dim); // save new center
  }

  // Update withinss
  for (size_t i = 0; i < nb_elements; i++) {
    withinss[i] = r2_distanceToClusters(elements[i], centers, asso[i],
                                        nb_clusters, nb_dim, lambda);
  }
}

// == R2-OKM ==

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
void r2okm(double **elements, double **centers, bool **asso,
           unsigned nb_elements, unsigned nb_clusters, unsigned nb_dim,
           double lambda, bool trace, unsigned max_iter, double *withinss,
           double *tot, double *totwss, unsigned short *iter) {

  unsigned short i = 0; ///< The current iteration
  double totwss_pre;
  *totwss = INFINITY;
  for (size_t i = 0; i < nb_elements; i++)
    withinss[i] = INFINITY;

  do {
    i++;
    totwss_pre = *totwss;

    // Assign all elements to a class
    r2_assign(elements, centers, asso, nb_elements, nb_clusters, nb_dim, lambda,
              withinss);
    double va = sum_double_array(withinss, nb_clusters);

    // Update all centers
    r2_update(elements, centers, asso, nb_elements, nb_clusters, nb_dim, lambda,
              withinss);
    *totwss = sum_double_array(withinss, nb_clusters);

    PRINT_ITER(trace, i, va, *totwss);

  } while (i < max_iter && totwss_pre > *totwss); ///< While is not stable

  *tot = *totwss + r2_betweenss(centers, nb_clusters, nb_dim);
  *iter = i;
}

#endif
