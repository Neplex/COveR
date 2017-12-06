#ifndef __IKMEANS_H
#define __IKMEANS_H

#include "../helpers.h"
#include "dists/ik_euclid.h"
#include "dists/ik_hausdorff.h"

// ===== Functions =====

void ik_assign(Interval **elements, Interval **centers, unsigned *asso,
               unsigned nb_elements, unsigned nb_clusters, unsigned nb_interval,
               Distance dist, double *withinss) {

  double wss[nb_clusters]; ///< New withinss
  for (size_t k = 0; k < nb_clusters; k++)
    wss[k] = 0;

  // Assign element by element
  for (size_t i = 0; i < nb_elements; i++) {
    double min_dist = INFINITY;

    // For all clusters, get the class with the min distance
    for (size_t k = 0; k < nb_clusters; k++) {
      double d;

      switch (dist) {
      case EUCLIDEAN:
        d = square_distance(elements[i], centers[k], nb_interval);
        break;

      case HAUSDORFF:
        d = haus_distance(elements[i], centers[k], nb_interval);
        break;
      }

      if (d < min_dist) {
        min_dist = d;
        asso[i] = k;
      }
    }

    // Update withinss
    wss[asso[i]] += min_dist;
  }

  copy_array(wss, withinss, nb_clusters); // save withinss
}

void ik_update(Interval **elements, Interval **centers, unsigned *asso,
               unsigned nb_elements, unsigned nb_clusters, unsigned nb_interval,
               Distance dist, double *withinss) {

  switch (dist) {

  case HAUSDORFF:
    ik_hausdorff_update(elements, centers, asso, nb_elements, nb_clusters,
                        nb_interval, withinss);
    break;

  case EUCLIDEAN:
    ik_euclid_update(elements, centers, asso, nb_elements, nb_clusters,
                     nb_interval, withinss);
    break;
  }
}

double ik_getBetweenss(Interval **centers, unsigned nb_clusters,
                       unsigned nb_interval, Distance dist) {
  double res = 0;
  // For all clusters
  for (size_t k = 0; k < nb_clusters; k++) {

    // Get the mean element of other centers
    Interval* mean = new_array_Interval(nb_interval);
    for (size_t j = 0; j < nb_interval; j++) {
      mean[j].min = 0;
      mean[j].max = 0;

      for (size_t i = 0; i < nb_clusters; i++) {
        if (i != k) {
          mean[j].min += centers[i][j].min;
          mean[j].max += centers[i][j].max;
        }
      }

      mean[j].min /= nb_clusters;
      mean[j].max /= nb_clusters;
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

    delete_array(&mean);
  }

  return res;
}

// ===== I-Kmeans =====

/**
 * @brief I-Kmeans (kmeans for interval data)
 * @param elements the elements to compute
 * @param centers the centers of clusters
 * @param asso an unsigned array to associate elements with class
 * @param nb_elements the number of elements
 * @param nb_clusters the number of clusters
 * @param nb_interval the number of interval
 * @param dist the distance to use
 * @param trace show trace ?
 * @param max_iter the maximum number of iteration
 * @param withinss a container to return withinss
 * @param tot a container to return total sum of square
 * @param totwss a container to return total withinss
 * @param iter a container to return the number of iteration
 */
void ikmeans(Interval **elements, Interval **centers, unsigned *asso,
             unsigned nb_elements, unsigned nb_clusters, unsigned nb_interval,
             Distance dist, bool trace, unsigned max_iter, double *withinss,
             double *tot, double *totwss, unsigned short *iter) {
  unsigned short i = 0; ///< The current iteration
  double totwss_pre;
  *totwss = INFINITY;

  do {
    i++;
    totwss_pre = *totwss;

    // Assign all elements to a class
    ik_assign(elements, centers, asso, nb_elements, nb_clusters, nb_interval,
              dist, withinss);
    double va = sum_double_array(withinss, nb_clusters);

    // Update all centers
    ik_update(elements, centers, asso, nb_elements, nb_clusters, nb_interval,
              dist, withinss);
    *totwss = sum_double_array(withinss, nb_clusters);

    PRINT_ITER(trace, i, va, *totwss);

  } while (i < max_iter && totwss_pre > *totwss); ///< While is not stable

  *tot = ik_getBetweenss(centers, nb_clusters, nb_interval, dist) + *totwss;
  *iter = i;
}

#endif
