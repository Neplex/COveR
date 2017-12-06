#ifndef __INEOKM_H
#define __INEOKM_H

#include "../helpers.h"

// == Betwennss ==

double ineo_betweenss(Interval **centers, unsigned nb_clusters,
                      unsigned nb_interval) {

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
    res += square_distance(centers[k], mean, nb_interval);

    delete_array(&mean);
  }

  return res;
}

// == Assign & Update ==

void ineo_assign(Interval **elements, Interval **centers, bool **asso,
                 unsigned nb_elements, unsigned nb_clusters,
                 unsigned nb_interval, double alpha, double beta,
                 double *withinss) {

  // Clear withinss
  for (size_t k = 0; k < nb_clusters; k++) {
    withinss[k] = 0;
  }

  // Compute distances between every data point and clusters
  double dists[nb_elements][nb_clusters];
  for (size_t i = 0; i < nb_elements; i++) {
    for (size_t k = 0; k < nb_clusters; k++) {
      dists[i][k] = square_distance(elements[i], centers[k], nb_interval);
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

void ineo_update(Interval **elements, Interval **centers, bool **asso,
                 unsigned nb_elements, unsigned nb_clusters,
                 unsigned nb_interval, double *withinss) {

  // Update class by class
  for (size_t k = 0; k < nb_clusters; k++) {
    withinss[k] = 0; ///< withinss inside class k

    // For all intervals compute mean of all elements in class
    for (size_t j = 0; j < nb_interval; j++) {
      double min = 0;
      double max = 0;
      unsigned nb_elem = 0; ///< Nb elem in cluster k

      // For elements in class
      for (size_t i = 0; i < nb_elements; i++) {
        if (asso[i][k]) {

          Interval r = elements[i][j];
          min += r.min;
          max += r.max;
          nb_elem++;
        }
      }

      if (nb_elem) { // If is not empty, update to mean of elements in it
        centers[k][j].min = min / nb_elem;
        centers[k][j].max = max / nb_elem;
      } else { // If is empty, remove class
        centers[k][j].min = centers[k][j].max = NAN;
      }
    }

    // Update withinss
    for (size_t i = 0; i < nb_elements; i++) {
      if (asso[i][k]) {
        withinss[k] += square_distance(elements[i], centers[k], nb_interval);
      }
    }
  }
}

// == I-NEOKM ==

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
void ineokm(Interval **elements, Interval **centers, bool **asso,
            unsigned nb_elements, unsigned nb_clusters, unsigned nb_interval,
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
    ineo_assign(elements, centers, asso, nb_elements, nb_clusters, nb_interval,
                alpha, beta, withinss);
    double va = sum_double_array(withinss, nb_clusters);

    // Update all centers
    ineo_update(elements, centers, asso, nb_elements, nb_clusters, nb_interval,
                withinss);
    *totwss = sum_double_array(withinss, nb_clusters);

    PRINT_ITER(trace, i, va, *totwss);

  } while (i < max_iter && totwss_pre > *totwss); ///< While is not stable

  *tot = *totwss + ineo_betweenss(centers, nb_clusters, nb_interval);
  *iter = i;
}

#endif
