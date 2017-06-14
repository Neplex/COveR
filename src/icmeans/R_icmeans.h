/*
This file is used to make interface between R and C code (C code is independant)
*/

#include "icmeans.h"

// ===== Function =====

/**
 * @brief Interface between R and C code for icmeans
 * @param Rdata the R vector of data
 * @param Rx the number of elements (dim x)
 * @param Ry the number of values (dim y), always 2 (min, max)
 * @param Rz the number of intervals (dim y)
 * @param Rnc the number of wanted clusters
 * @param Rm the degree of fuzzification
 * @param Rns the number of execution
 * @param Rd the distance to use
 * @param Rt true for trace
 * @param Rim max iteration
 * @param Rcent the generate centers
 * @return a R list with results
 */
SEXP R_icmeans(SEXP Rdata, SEXP Rx, SEXP Ry, SEXP Rz, SEXP Rnc, SEXP Rm,
               SEXP Rns, SEXP Rd, SEXP Rt, SEXP Rim, SEXP Rcent) {

  unsigned n, x, y, z, nc, ns, d, im, i, j;
  bool t, pre_init = false;
  double *data_vec, *centers_vec, m;
  SEXP Rcluster, Rcenters, Rtot, Rwss, Rtotwss, Riter, Rlist;

  // R types to C types
  data_vec = NUMERIC_POINTER(Rdata); // Data vector (elements)
  n = (unsigned)GET_LENGTH(Rdata);   // Length of data vector
  x = (unsigned)INTEGER_VALUE(Rx);   // First dim size
  y = (unsigned)INTEGER_VALUE(Ry);   // Second dim size
  z = (unsigned)INTEGER_VALUE(Rz);   // Third dim size
  nc = (unsigned)INTEGER_VALUE(Rnc); // Number of clusters
  m = (unsigned)NUMERIC_VALUE(Rm);   // Degree of fuzzification
  ns = (unsigned)INTEGER_VALUE(Rns); // Nb of execution to find the best result
  d = (unsigned)INTEGER_VALUE(Rd);   // Distance to use
  t = (unsigned)LOGICAL_VALUE(Rt);   // Trace
  im = (unsigned)INTEGER_VALUE(Rim); // Nb of maximum iteration

  // Pre generate centers
  if (Rcent != NULL_USER_OBJECT) {
    centers_vec = NUMERIC_POINTER(Rcent);
    pre_init = true;
    ns = 1;
  }

  // Transform the data vector to an Intervall 2D array
  Interval **elements = new_matrix_Interval(x, z);
  for (i = 0; i < x; i++) {
    for (j = 0; j < z; j++) {
      elements[i][j].min = data_vec[i + j * 2 * x];
      elements[i][j].max = data_vec[i + j * 2 * x + x];
    }
  }

  // Transform the centers vector to an Intervall 2D array
  Interval centers_init[nc][z];
  if (pre_init) {
    for (i = 0; i < nc; i++) {
      for (j = 0; j < z; j++) {
        centers_init[i][j].min = centers_vec[i + j * 2 * nc];
        centers_init[i][j].max = centers_vec[i + j * 2 * nc + nc];
      }
    }
  }

  Interval **centers = new_matrix_Interval(nc, z);
  double **asso = new_matrix_double(x, nc);
  double *withinss = new_array_double(nc);
  double tot, totwss = INFINITY;
  unsigned short iteration = 0;

  // Execute icmeans
  for (j = 0; j < ns; j++) {
    Interval **c = new_matrix_Interval(nc, z);
    double **a = new_matrix_double(x, nc);
    double *w = new_array_double(nc);
    double to, tw;
    unsigned short i;

    PRINT_START(t, "icmeans", j);

    if (pre_init) {
      copy_matrix(centers_init, c, nc, z);
    } else {
      initClusters(elements, c, x, nc, z);
    }

    icmeans(elements, c, a, x, nc, z, m, d, t, im, w, &to, &tw, &i);

    if (tw < totwss) {
      copy_matrix(c, centers, nc, z);
      copy_matrix(a, asso, x, nc);
      copy_array(w, withinss, nc);
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
  PROTECT(Rcluster = allocMatrix(REALSXP, x, nc));
  for (i = 0; i < x; i++)
    for (j = 0; j < nc; j++)
      REAL(Rcluster)[i + j * x] = asso[i][j];

  PROTECT(Rcenters = alloc3DArray(REALSXP, nc, 2, z));
  for (i = 0; i < nc; i++) {
    for (j = 0; j < z; j++) {
      REAL(Rcenters)[i + j * 2 * nc] = centers[i][j].min;
      REAL(Rcenters)[i + j * 2 * nc + nc] = centers[i][j].max;
    }
  }

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
