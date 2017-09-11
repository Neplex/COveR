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
// TODO: GSL on CRAN #include <gsl/gsl_linalg.h>

// Usefull math functions
#define sqr(x) ((x) * (x))
#define max(x, y) ((x > y) ? x : y)
#define min(x, y) ((x < y) ? x : y)

int range_rand(int a, int b) {
  GetRNGstate();
  int v = unif_rand() * (b - a) + a;
  PutRNGstate();
  return (int)v;
}

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

define_array(unsigned, 0)
define_array(double, 0)
define_array(Interval, ((Interval){0, 0}))

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

define_matrix(bool, true)
define_matrix(double, 0)
define_matrix(Interval, ((Interval){0, 0}))

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
  return (*(double *)a * 1E6 - *(double *)b * 1E6);
}

double median(double *array, unsigned length) {
  qsort(array, length, sizeof(double), cmpfunc);
  if (length % 2 == 1) {
    return array[length / 2];
  }
  return (array[length / 2] + array[length / 2 - 1]) / 2;
}

void swap(unsigned *index, unsigned i, unsigned j) {
  unsigned tmp = index[i];
  index[i] = index[j];
  index[j] = tmp;
}

int partition(double *array, unsigned *index, int left, int right, int pivot) {
  swap(index, pivot, right);
  int j = left;
  for (size_t i = left; i < right; i++) {
    if (array[index[i]] <= array[index[right]]) {
      swap(index, i, j);
      j++;
    }
  }
  swap(index, right, j);
  return j;
}

void get_sort_order(double *array, unsigned *index, int left, int right) {
  if (left < right) {
    int pivot = (left + right) / 2;
    pivot = partition(array, index, left, right, pivot);
    get_sort_order(array, index, left, pivot - 1);
    get_sort_order(array, index, pivot + 1, right);
  }
}

double weighted_median(double *b, double *z, unsigned nb_elements) {
  unsigned index[nb_elements];
  for (size_t i = 0; i < nb_elements; i++)
    index[i] = i;

  get_sort_order(b, index, 0, nb_elements - 1);

  double sum_left = 0;
  double sum_right = sum_double_array(z, nb_elements);
  unsigned i;

  for (i = 0; i < nb_elements; i++) {
    sum_left += z[index[i]];
    sum_right -= z[index[i]];

    if (sum_left > sum_right)
      break;
  }

  return b[index[i]];
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

double vector_square_distance(const double *v1, const double *v2,
                              const unsigned nb_dim) {
  double dist = 0;

  // For all dim in elements
  for (size_t j = 0; j < nb_dim; j++) {
    dist += sqr(v1[j] - v2[j]);
  }

  return dist;
}

// ===== Init =====

// Init clusters with random values from elements
void initClusters(Interval **elements, Interval **clusters,
                  unsigned nb_elements, unsigned nb_clusters,
                  unsigned nb_interval) {

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
}

// Init clusters with random values from elements for non Interval data
void initVectorClusters(double **elements, double **clusters,
                        unsigned nb_elements, unsigned nb_clusters,
                        unsigned nb_dim) {

  // All available index
  unsigned t[nb_elements];
  for (size_t i = 0; i < nb_elements; i++)
    t[i] = i;

  for (size_t i = 0; i < nb_clusters; i++) {
    // Get a random index
    unsigned ind = range_rand(0, nb_elements - i);
    // Get the element at this index
    double *e = elements[t[ind]];
    // Remove index from list (swap to the end)
    unsigned old = t[nb_elements - i - 1];
    t[nb_elements - i] = t[ind];
    t[ind] = old;
    // Set cluster values to element values
    for (size_t j = 0; j < nb_dim; j++) {
      clusters[i][j] = e[j];
    }
  }
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

/* TODO: GSL on CRAN
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
    // libgsl SVD can only handle the case m <= n - transpose matrix
    was_swapped = true;
    _tmp_mat = gsl_matrix_alloc(m, n);
    gsl_matrix_transpose_memcpy(_tmp_mat, A);
    A = _tmp_mat;
    i = m;
    m = n;
    n = i;
  }

  // do SVD
  V = gsl_matrix_alloc(m, m);
  u = gsl_vector_alloc(m);
  _tmp_vec = gsl_vector_alloc(m);
  gsl_linalg_SV_decomp(A, V, u, _tmp_vec);
  gsl_vector_free(_tmp_vec);

  // compute Σ⁻¹
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

  // libgsl SVD yields "thin" SVD - pad to full matrix by adding zeros
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

  // two dot products to obtain pseudoinverse
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
*/

#endif
