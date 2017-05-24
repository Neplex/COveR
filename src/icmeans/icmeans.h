#ifndef __ICMEANS_H
#define __ICMEANS_H

#include "../helpers.h"
#include "dists/ic_euclid.h"
#include "dists/ic_hausdorff.h"

// ===== Functions =====

void ic_assign(Interval **elements, Interval **centers, double **asso,
               unsigned nb_elements, unsigned nb_clusters, unsigned nb_interval,
               double m, Distance dist, double *withinss) {

  double wss[nb_clusters]; ///< New withinss
  for (size_t k = 0; k < nb_clusters; k++)
    wss[k] = 0;

  // Assign element by element
  for (size_t i = 0; i < nb_elements; i++) {
    double dists[nb_clusters];

    // For all clusters compute distance
    for (size_t k = 0; k < nb_clusters; k++) {
      switch (dist) {
      case EUCLIDEAN:
        dists[k] = square_distance(elements[i], centers[k], nb_interval);
        break;

      case HAUSDORFF:
        dists[k] = haus_distance(elements[i], centers[k], nb_interval);
        break;
      }
    }

    // For all clusters update weigth
    for (size_t k = 0; k < nb_clusters; k++) {
      double sum = 0;

      if (!dists[k]) { // Element i equal to class k
        asso[i][k] = 1;
      } else {
        // For all clusters
        for (size_t l = 0; l < nb_clusters; l++) {
          if (dists[l]) {
            sum += pow(dists[k] / dists[l], 1.0 / (m - 1));
          } else { // Element i equal to cluster l
            sum = 0;
            break;
          }
        }

        if (sum)
          asso[i][k] = 1.0 / sum;
        else
          asso[i][k] = 0;

        // Update withinss
        wss[k] += dists[k] * pow(asso[i][k], m);
      }
    }
  }

  copy_array(wss, withinss, nb_clusters); // save withinss
}

void ic_update(Interval **elements, Interval **centers, double **asso,
               unsigned nb_elements, unsigned nb_clusters, unsigned nb_interval,
               double m, Distance dist, double *withinss) {

  switch (dist) {

  case HAUSDORFF:
    ic_hausdorff_update(elements, centers, asso, nb_elements, nb_clusters,
                        nb_interval, m, withinss);
    break;

  case EUCLIDEAN:
    ic_euclid_update(elements, centers, asso, nb_elements, nb_clusters,
                     nb_interval, m, withinss);
    break;
  }
}

double ic_getBetweenss(Interval **centers, unsigned nb_clusters,
                       unsigned nb_interval, Distance dist) {
  double res = 0;

  // For all clusters
  for (size_t k = 0; k < nb_clusters; k++) {

    // Get the mean element of other centers
    Interval mean[nb_interval];
    for (size_t j = 0; j < nb_interval; j++) {
      mean[j].min = 0;
      mean[j].max = 0;

      for (size_t i = 0; i < nb_clusters; i++) {
        if (i != k) {
          mean[j].min += centers[i][j].min;
          mean[j].max += centers[i][j].max;
        }
      }

      mean[j].min /= (nb_clusters - 1);
      mean[j].max /= (nb_clusters - 1);
    }

    // Sum distance
    switch (dist) {
    case EUCLIDEAN:
      res += square_distance(centers[k], mean, nb_interval);
      break;

    case HAUSDORFF:
      res += haus_distance(centers[k], mean, nb_interval);
      break;
    }
  }

  return res;
}

// ===== I-Kmeans =====

/**
 * @brief Fuzzy I-Cmeans (fuzzy cmeans for interval data)
 * @param elements the elements to compute
 * @param centers the centers of clusters
 * @param asso an double matrix to associate element with weight in class
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
void icmeans(Interval **elements, Interval **centers, double **asso,
             unsigned nb_elements, unsigned nb_clusters, unsigned nb_interval,
             double m, Distance dist, bool trace, unsigned max_iter,
             double *withinss, double *tot, double *totwss,
             unsigned short *iter) {
  unsigned short i = 0; ///< The current iteration
  double totwss_pre;
  *totwss = INFINITY;

  do {
    i++;
    totwss_pre = *totwss;

    // Assign all elements to a class
    ic_assign(elements, centers, asso, nb_elements, nb_clusters, nb_interval, m,
              dist, withinss);
    double va = sum_double_array(withinss, nb_clusters);

    // Update all centers
    ic_update(elements, centers, asso, nb_elements, nb_clusters, nb_interval, m,
              dist, withinss);
    *totwss = sum_double_array(withinss, nb_clusters);

    PRINT_ITER(trace, i, va, *totwss);

  } while (i < max_iter &&
           (totwss_pre - *totwss) > 1E-6); ///< While is not stable

  *tot = ic_getBetweenss(centers, nb_clusters, nb_interval, dist) + *totwss;
  *iter = i;
}

#endif
