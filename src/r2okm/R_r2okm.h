/*
This file is used to make interface between R and C code (C code is independant)
*/

#include "r2okm.h"

// ===== Function =====

/**
 * @brief Interface between R and C code for r2okm
 * @param Rvec the R vector of data
 * @param Rx the number of elements (dim x)
 * @param Ry the number of dimentions (dim y)
 * @param Rnc the number of wanted clusters
 * @param Rl lambda
 * @param Rns the number of execution
 * @param Rt true for trace
 * @param Rim max iteration
 * @param Rcent the generate centers
 * @return a R list with results
 */
SEXP R_r2okm(SEXP Rvec, SEXP Rx, SEXP Ry, SEXP Rnc, SEXP Rl, SEXP Rns, SEXP Rt,
             SEXP Rim, SEXP Rcent) {

  unsigned n, x, y, nc, ns, im, i, j;
  double lambda;
  bool t, pre_init = false;
  double *data_vec, *centers_vec;
  SEXP Rcluster, Rcenters, Rtot, Rwss, Rtotwss, Riter, Rlist;

  // R types to C types
  data_vec = NUMERIC_POINTER(Rvec);   // Data vector (elements)
  n = (unsigned)GET_LENGTH(Rvec);     // Length of data vector
  x = (unsigned)INTEGER_VALUE(Rx);    // First dim size
  y = (unsigned)INTEGER_VALUE(Ry);    // Second dim size
  nc = (unsigned)INTEGER_VALUE(Rnc);  // Number of clusters
  lambda = (double)NUMERIC_VALUE(Rl); // Alpha
  ns = (unsigned)INTEGER_VALUE(Rns);  // Nb of execution to find the best result
  t = (bool)LOGICAL_VALUE(Rt);        // Trace
  im = (unsigned)INTEGER_VALUE(Rim);  // Nb of maximum iteration

  // Pre generate centers
  if (Rcent != NULL_USER_OBJECT) {
    centers_vec = NUMERIC_POINTER(Rcent);
    pre_init = true;
    ns = 1;
  }

  // Transform the data vector to an Intervall 2D array
  double **elements = new_matrix_double(x, y);
  for (i = 0; i < x; i++) {
    for (j = 0; j < y; j++) {
      elements[i][j] = data_vec[i + j * x];
    }
  }

  // Transform the centers vector to an Intervall 2D array
  double centers_init[nc][y];
  if (pre_init) {
    for (i = 0; i < nc; i++) {
      for (j = 0; j < y; j++) {
        centers_init[i][j] = centers_vec[i + j * nc];
      }
    }
  }

  double **centers = new_matrix_double(nc, y);
  bool **asso = new_matrix_bool(x, nc);
  double *withinss = new_array_double(x);
  double tot, totwss = INFINITY;
  unsigned short iteration = 0;

  // Execute r2okm
  for (j = 0; j < ns; j++) {
    double **c = new_matrix_double(nc, y);
    bool **a = new_matrix_bool(x, nc);
    double *w = new_array_double(x);
    double to, tw;
    unsigned short i;

    PRINT_START(t, "r2okm", j);

    if (pre_init) {
      copy_matrix(centers_init, c, nc, y);
    } else {
      initVectorClusters(elements, c, x, nc, y);
    }

    r2okm(elements, c, a, x, nc, y, lambda, t, im, w, &to, &tw, &i);

    if (tw < totwss) {
      copy_matrix(c, centers, nc, y);
      copy_matrix(a, asso, x, nc);
      copy_array(w, withinss, x);
      tot = to;
      totwss = tw;
      iteration = i;

      if (tw == 0)
        break;
    }

    delete_matrix(&c, nc);
    delete_matrix(&a, x);
    delete_array(&w);
  }

  // Results
  PROTECT(Rcluster = allocMatrix(LGLSXP, x, nc));
  for (i = 0; i < x; i++)
    for (j = 0; j < nc; j++)
      LOGICAL(Rcluster)[i + j * x] = asso[i][j];

  PROTECT(Rcenters = allocMatrix(REALSXP, nc, y));
  for (i = 0; i < nc; i++)
    for (j = 0; j < y; j++)
      REAL(Rcenters)[i + j * nc] = centers[i][j];

  PROTECT(Rtot = ScalarReal(tot));

  PROTECT(Rwss = NEW_NUMERIC(nc));
  for (i = 0; i < nc; i++)
    REAL(Rwss)[i] = withinss[i];

  PROTECT(Rtotwss = ScalarReal(totwss));

  PROTECT(Riter = ScalarInteger(iteration));

  // Build result list
  PROTECT(Rlist = NEW_LIST(6));
  SET_ELEMENT(Rlist, 0, Rcluster);
  SET_ELEMENT(Rlist, 1, Rcenters);
  SET_ELEMENT(Rlist, 2, Rtot);
  SET_ELEMENT(Rlist, 3, Rwss);
  SET_ELEMENT(Rlist, 4, Rtotwss);
  SET_ELEMENT(Rlist, 5, Riter);

  UNPROTECT(7);

  delete_matrix(&elements, x);
  delete_matrix(&centers, nc);
  delete_matrix(&asso, x);
  delete_array(&withinss);

  return Rlist;
}
