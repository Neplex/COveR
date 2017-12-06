#ifndef __R1OKM_H
#define __R1OKM_H

#include "../helpers.h"

// == Betwennss ==

double r1_betweenss(double **centers, unsigned nb_clusters, unsigned nb_dim) {

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

double r1_distanceToClusters(double *elem, double **centers, bool *asso,
                             unsigned nb_clusters, unsigned nb_dim,
                             double alpha) {
  double mean_prototype[nb_dim];
  unsigned nbc = 0;
  for (size_t k = 0; k < nb_clusters; k++) {
    nbc += asso[k];
  }

  // For all dim
  for (size_t j = 0; j < nb_dim; j++) {
    mean_prototype[j] = 0;

    // For all associated clusters
    for (size_t k = 0; k < nb_clusters; k++) {
      if (asso[k]) {
        mean_prototype[j] += centers[k][j];
      }
    }

    mean_prototype[j] = (nbc) ? mean_prototype[j] / nbc : INFINITY;
  }

  return vector_square_distance(elem, mean_prototype, nb_dim) * pow(nbc, alpha);
}

void r1_assign(double **elements, double **centers, bool **asso,
               unsigned nb_elements, unsigned nb_clusters, unsigned nb_dim,
               double alpha, double *withinss) {

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

    unsigned nb_clusters_check = 0;

    do {
      double min_dist = INFINITY;
      unsigned ck = 0; ///< The next closest cluster
      end = true;

      nb_clusters_check++;

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

      next_new_dist = r1_distanceToClusters(elements[i], centers, tmp_asso,
                                            nb_clusters, nb_dim, alpha);

      // If with the new cluster the result is better, add it to new association
      // and search for the closest cluster again
      if (next_new_dist < new_dist) {
        new_asso[ck] = true;      // Add new cluster
        new_dist = next_new_dist; // Save distance
        end = false;              // Not the end
      }

    } while (!end && nb_clusters_check<=nb_clusters);

    // If the new association is better than previous iteration, choose it
    if (new_dist <= withinss[i]) {
      copy_array(new_asso, asso[i], nb_clusters); // save asso
      withinss[i] = new_dist;                     // save withinss
    }
  }
}

void r1_update(double **elements, double **centers, bool **asso,
               unsigned nb_elements, unsigned nb_clusters, unsigned nb_dim,
               double alpha, double *withinss) {

  // Get number of associate clusters by elements and compute weight
  unsigned nb_asso[nb_elements];
  double weight[nb_elements];
  for (size_t i = 0; i < nb_elements; i++) {
    nb_asso[i] = 0;
    for (size_t k = 0; k < nb_clusters; k++) {
      nb_asso[i] += asso[i][k];
    }
    weight[i] = 1.0 / pow((double)nb_asso[i], 2 - alpha);
  }

  // Update cluster by cluster
  for (size_t k = 0; k < nb_clusters; k++) {

    // Update dim by dim
    for (size_t j = 0; j < nb_dim; j++) {
      double res = 0.0;
      double ws = 0.0;

      // For all elements in cluster
      for (size_t i = 0; i < nb_elements; i++) {
        if (asso[i][k]) {
          double tmp = elements[i][j];

          // Compute x^i
          tmp *= nb_asso[i];
          for (size_t l = 0; l < nb_clusters; l++) {
            if (asso[i][l] && l != k) {
              tmp -= centers[l][j];
            }
          }

          // Compute sums
          ws += weight[i];
          res += tmp * weight[i];
        }
      }

      // Compute Weighted mean
      centers[k][j] = res / ws;
    }
  }

  // Update withinss
  for (size_t i = 0; i < nb_elements; i++) {
    withinss[i] = r1_distanceToClusters(elements[i], centers, asso[i],
                                        nb_clusters, nb_dim, alpha);
  }
}

// == R1-OKM ==

/**
 * @brief R1-OKM
 * @param elements the elements to compute
 * @param centers the centers of clusters
 * @param asso an boolean matrix to associate element with class
 * @param nb_elements the number of elements
 * @param nb_clusters the number of clusters
 * @param nb_dim the number of dim
 * @param alpha
 * @param trace show trace ?
 * @param max_iter the maximum number of iteration
 * @param withinss a container to return withinss
 * @param tot a container to return total sum of square
 * @param totwss a container to return total withinss
 * @param iter a container to return the number of iteration
 */
void r1okm(double **elements, double **centers, bool **asso,
           unsigned nb_elements, unsigned nb_clusters, unsigned nb_dim,
           double alpha, bool trace, unsigned max_iter, double *withinss,
           double *tot, double *totwss, unsigned short *iter) {

  unsigned short i = 0; ///< The current iteration
  double totwss_pre, va;
  *totwss = INFINITY;
  for (size_t i = 0; i < nb_elements; i++)
    withinss[i] = INFINITY;

  do {
    i++;
    totwss_pre = *totwss;

    // Assign all elements to a class
    r1_assign(elements, centers, asso, nb_elements, nb_clusters, nb_dim, alpha,
              withinss);
    va = sum_double_array(withinss, nb_elements);

    // Update all centers
    r1_update(elements, centers, asso, nb_elements, nb_clusters, nb_dim, alpha,
              withinss);
    *totwss = sum_double_array(withinss, nb_elements);

    PRINT_ITER(trace, i, va, *totwss);

  } while (i < max_iter && totwss_pre > *totwss); ///< While is not stable

  *tot = *totwss + r1_betweenss(centers, nb_clusters, nb_dim);
  *iter = i;
}

#endif
