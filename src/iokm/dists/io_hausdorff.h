#ifndef __IO_HAUSDORFF_H
#define __IO_HAUSDORFF_H

#include "hausdorff/io_hausdorff_join.h"
#include "hausdorff/io_hausdorff_mean.h"
#include "hausdorff/io_hausdorff_meet.h"
#include "hausdorff/io_hausdorff_sum.h"

double io_hausdorff_distanceToClusters(Interval *elem, Interval **centers,
                                       bool *asso, unsigned nb_clusters,
                                       unsigned nb_interval, Update up) {
  double res = 0;

  switch (up) {
  case MEAN:
    res = io_hausdorff_mean_distanceToClusters(elem, centers, asso, nb_clusters,
                                               nb_interval);
    break;

  case SUM:
    res = io_hausdorff_sum_distanceToClusters(elem, centers, asso, nb_clusters,
                                              nb_interval);
    break;

  case JOIN:
    res = io_hausdorff_join_distanceToClusters(elem, centers, asso, nb_clusters,
                                               nb_interval);
    break;

  case MEET:
    res = io_hausdorff_meet_distanceToClusters(elem, centers, asso, nb_clusters,
                                               nb_interval);
    break;
  }

  return res;
}

void io_hausdorff_update(Interval **elements, Interval **centers, bool **asso,
                         unsigned nb_elements, unsigned nb_clusters,
                         unsigned nb_interval, Algorithm algo, Update up,
                         bool need_valid, double *withinss) {
  switch (up) {
  case MEAN:
    io_hausdorff_mean_update(elements, centers, asso, nb_elements, nb_clusters,
                             nb_interval, algo, need_valid, withinss);
    break;

  case SUM:
    io_hausdorff_sum_update(elements, centers, asso, nb_elements, nb_clusters,
                            nb_interval, algo, need_valid, withinss);
    break;

  case JOIN:
    io_hausdorff_join_update(elements, centers, asso, nb_elements, nb_clusters,
                             nb_interval, algo, need_valid, withinss);
    break;

  case MEET:
    io_hausdorff_meet_update(elements, centers, asso, nb_elements, nb_clusters,
                             nb_interval, algo, need_valid, withinss);
    break;
  }
}

#endif
