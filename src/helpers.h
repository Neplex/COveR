#ifndef __HELPERS_H
#define __HELPERS_H

#include <R.h>
#include <Rdefines.h>
#include <math.h>
#include <memory.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

// GNU Scientific Library
#include <gsl/gsl_linalg.h>

// Usefull math functions
#define sqr(x) ((x) * (x))
#define max(x, y) ((x > y) ? x : y)
#define min(x, y) ((x < y) ? x : y)

int range_rand(int a, int b) { return (int)unif_rand() * (b - a) + a; }

// ===== Type definition =====

typedef enum { EUCLIDEAN, HAUSDORFF } Distance;
typedef enum { STD, MATRIX } Algorithm;
typedef enum { MEAN, SUM, JOIN, MEET } Update;

/**
 * @brief Struct to store an interval (min:double, max:double)
 */
typedef struct {
  double min;
  double max;
} Interval;
// TODO: float are better (less memory, faster to compute), but error is too big

// ===== Alloc =====

// Macro to automaticly create function to alloc array of type T with default
// value Dval
#define define_array(T, Dval)                                                  \
  T *new_array_##T(unsigned length) {                                          \
    T *a = (T *)malloc(sizeof(T) * length);                                    \
    for (size_t i = 0; i < length; i++) {                                      \
      a[i] = Dval;                                                             \
    }                                                                          \
    return a;                                                                  \
  }

define_array(unsigned, 0);
define_array(double, 0);
define_array(Interval, ((Interval){0, 0}));

// Macro to automaticly create function to alloc matrix of type T with default
// value Dval
#define define_matrix(T, Dval)                                                 \
  T **new_matrix_##T(unsigned size_x, unsigned size_y) {                       \
    T **m = (T **)malloc(sizeof(T *) * size_x);                                \
    for (size_t i = 0; i < size_x; i++) {                                      \
      m[i] = (T *)malloc(sizeof(T) * size_y);                                  \
      for (size_t j = 0; j < size_y; j++) {                                    \
        m[i][j] = Dval;                                                        \
      }                                                                        \
    }                                                                          \
    return m;                                                                  \
  }

define_matrix(bool, true);
define_matrix(double, 0);
define_matrix(Interval, ((Interval){0, 0}));

// Delete array, not safe as function with void** but not need to cast
#define delete_array(a)                                                        \
  {                                                                            \
    if (*a != NULL) {                                                          \
      free(*a);                                                                \
      *a = NULL;                                                               \
    }                                                                          \
  }

// Delete matrix, not safe as function with void*** but not need to cast
#define delete_matrix(m, size_x)                                               \
  {                                                                            \
    if (*m != NULL) {                                                          \
      for (size_t i = 0; i < size_x; i++)                                      \
        free((*m)[i]);                                                         \
      free(*m);                                                                \
      *m = NULL;                                                               \
    }                                                                          \
  }

// ===== Copy =====

// Copy array src to dest, not safe as pointer function but not need to cast
#define copy_array(src, dest, length) memcpy(dest, src, length * sizeof(*src))

// Copy matrix src to dest, not safe as pointer function but not need to cast
#define copy_matrix(src, dest, size_x, size_y)                                 \
  for (size_t i = 0; i < size_x; i++)                                          \
    copy_array(src[i], dest[i], size_y);

// ===== Operator =====

// Sum all element in array
double sum_double_array(const double *a, const size_t length) {
  double sum = 0;
  for (size_t i = 0; i < length; i++) {
    sum += a[i];
  }
  return sum;
}

int cmpfunc(const void *a, const void *b) {
  return (*(double *)a * 1000 - *(double *)b * 1000);
}

double median(double *array, unsigned length) {
  qsort(array, length, sizeof(double), cmpfunc);
  if (length % 2 == 1) {
    return array[length / 2];
  }
  return (array[length / 2] + array[length / 2 - 1]) / 2;
}

double get_center(Interval i) { return (i.min + i.max) * .5; }
double get_half_size(Interval i) { return (i.max - i.min) * .5; }

// ===== Distance =====

// Compute square distance between two elements
double square_distance(const Interval *r1, const Interval *r2,
                       const unsigned nb_interval) {
  double dist = 0;

  // For all interval in elements
  for (size_t i = 0; i < nb_interval; i++) {
    dist += sqr(r1[i].min - r2[i].min);
    dist += sqr(r1[i].max - r2[i].max);
  }

  return dist;
}

// Compute Hausdorff distance between two elements
double haus_distance(const Interval *r1, const Interval *r2,
                     const unsigned nb_interval) {
  double dist = 0;

  for (size_t i = 0; i < nb_interval; i++) {
    dist += fabs(get_center(r1[i]) - get_center(r2[i]));
    dist += fabs(get_half_size(r1[i]) - get_half_size(r2[i]));
  }

  return dist;
}

// ===== Init =====

// Init clusters with random values from elements
void initClusters(Interval **elements, Interval **clusters,
                  unsigned nb_elements, unsigned nb_clusters,
                  unsigned nb_interval) {
  GetRNGstate();

  // All available index
  unsigned t[nb_elements];
  for (size_t i = 0; i < nb_elements; i++)
    t[i] = i;

  for (size_t i = 0; i < nb_clusters; i++) {
    // Get a random index
    unsigned ind = range_rand(0, nb_elements - i);
    // Get the element at this index
    Interval *e = elements[t[ind]];
    // Remove index from list (swap to the end)
    unsigned old = t[nb_elements - i - 1];
    t[nb_elements - i] = t[ind];
    t[ind] = old;
    // Set cluster values to element values
    for (size_t j = 0; j < nb_interval; j++) {
      clusters[i][j] = e[j];
    }
  }

  PutRNGstate();
}

// ===== Trace =====

// Macro for 'trace', used when call
#define PRINT_START(t, l, n)                                                   \
  if (t)                                                                       \
    Rprintf("%s: %u\n", l, n);

// Macro for 'trace', used at all iterations
#define PRINT_ITER(t, i, va, vu)                                               \
  if (t) {                                                                     \
    Rprintf("\t(iter: %u, assign: %f, update: %f)", i, va, vu);                \
    if (va < vu)                                                               \
      Rprintf("\tWarning: bad update");                                        \
    Rprintf("\n");                                                             \
  }

// ===== PSEUDO-INVERSE =====

// Code from: https://gist.github.com/turingbirds/5e99656e08dbe1324c99
gsl_matrix *moore_penrose_pinv(gsl_matrix *A, const double rcond) {

  gsl_matrix *V, *Sigma_pinv, *U, *A_pinv;
  gsl_matrix *_tmp_mat = NULL;
  gsl_vector *_tmp_vec;
  gsl_vector *u;
  double x, cutoff;
  size_t i, j;
  unsigned int n = A->size1;
  unsigned int m = A->size2;
  bool was_swapped = false;

  if (m > n) {
    /* libgsl SVD can only handle the case m <= n - transpose matrix */
    was_swapped = true;
    _tmp_mat = gsl_matrix_alloc(m, n);
    gsl_matrix_transpose_memcpy(_tmp_mat, A);
    A = _tmp_mat;
    i = m;
    m = n;
    n = i;
  }

  /* do SVD */
  V = gsl_matrix_alloc(m, m);
  u = gsl_vector_alloc(m);
  _tmp_vec = gsl_vector_alloc(m);
  gsl_linalg_SV_decomp(A, V, u, _tmp_vec);
  gsl_vector_free(_tmp_vec);

  /* compute Σ⁻¹ */
  Sigma_pinv = gsl_matrix_alloc(m, n);
  gsl_matrix_set_zero(Sigma_pinv);
  cutoff = rcond * gsl_vector_max(u);

  for (i = 0; i < m; ++i) {
    if (gsl_vector_get(u, i) > cutoff) {
      x = 1. / gsl_vector_get(u, i);
    } else {
      x = 0.;
    }
    gsl_matrix_set(Sigma_pinv, i, i, x);
  }

  /* libgsl SVD yields "thin" SVD - pad to full matrix by adding zeros */
  U = gsl_matrix_alloc(n, n);
  gsl_matrix_set_zero(U);

  for (i = 0; i < n; ++i) {
    for (j = 0; j < m; ++j) {
      gsl_matrix_set(U, i, j, gsl_matrix_get(A, i, j));
    }
  }

  if (_tmp_mat != NULL) {
    gsl_matrix_free(_tmp_mat);
  }

  /* two dot products to obtain pseudoinverse */
  _tmp_mat = gsl_matrix_alloc(m, n);
  gsl_blas_dgemm(CblasNoTrans, CblasNoTrans, 1., V, Sigma_pinv, 0., _tmp_mat);

  if (was_swapped) {
    A_pinv = gsl_matrix_alloc(n, m);
    gsl_blas_dgemm(CblasNoTrans, CblasTrans, 1., U, _tmp_mat, 0., A_pinv);
  } else {
    A_pinv = gsl_matrix_alloc(m, n);
    gsl_blas_dgemm(CblasNoTrans, CblasTrans, 1., _tmp_mat, U, 0., A_pinv);
  }

  gsl_matrix_free(_tmp_mat);
  gsl_matrix_free(U);
  gsl_matrix_free(Sigma_pinv);
  gsl_vector_free(u);
  gsl_matrix_free(V);

  return A_pinv;
}

#endif
