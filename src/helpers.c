#include "helpers.h"

#include <R.h>
#include <math.h>
#include <memory.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#include "interval.h"

#ifdef GSL_VERSION
// GNU Scientific Library
#include <gsl/gsl_linalg.h>
#endif

unsigned range_rand(const unsigned a, const unsigned b) {
    GetRNGstate();
    const unsigned v = (unsigned)(unif_rand() * (b - a) + a);
    PutRNGstate();
    return v;
}

// ===== Alloc =====

// Macro to automatically create function to alloc array of type T with default
// value Dval
#define define_array(T, Dval)                   \
    T *new_array_##T(const unsigned length) {   \
        T *a = (T *)malloc(sizeof(T) * length); \
        if (!a) return NULL;                    \
        for (unsigned i = 0; i < length; i++) { \
            a[i] = (Dval);                      \
        }                                       \
        return a;                               \
    }

define_array(unsigned, 0) define_array(double, 0) define_array(Interval, ((Interval){0, 0}))
// Macro to automatically create function to alloc matrix of type T with
// default value Dval
#define define_matrix(T, Dval)                                         \
    T **new_matrix_##T(const unsigned size_x, const unsigned size_y) { \
        T **m = (T **)malloc(sizeof(T *) * size_x);                    \
        if (!m) return NULL;                                           \
        for (unsigned i = 0; i < size_x; i++) {                        \
            m[i] = (T *)malloc(sizeof(T) * size_y);                    \
            if (!m[i]) {                                               \
                for (unsigned j = 0; j < i; j++) free(m[j]);           \
                free(m);                                               \
                return NULL;                                           \
            }                                                          \
            for (unsigned j = 0; j < size_y; j++) {                    \
                m[i][j] = (Dval);                                      \
            }                                                          \
        }                                                              \
        return m;                                                      \
    }

    define_matrix(bool, true) define_matrix(unsigned, 0) define_matrix(double, 0)
        define_matrix(Interval, ((Interval){0, 0}))

            void delete_array(void **array) {
    if (*array != NULL) {
        free(*array);
        *array = NULL;
    }
}

void delete_matrix(void ***matrix, const unsigned size_x) {
    if (*matrix != NULL) {
        for (unsigned i = 0; i < size_x; i++) {
            if ((*matrix)[i] != NULL) {
                free((*matrix)[i]);
            }
        }
        free(*matrix);
        *matrix = NULL;
    }
}

// ===== Operator =====

// Sum all element in array
double sum_double_array(const double *a, const unsigned length) {
    double sum = 0;
    for (unsigned i = 0; i < length; i++) {
        sum += a[i];
    }
    return sum;
}

int cmpfunc(const void *a, const void *b) {
    return (*(double *)a > *(double *)b) - (*(double *)a < *(double *)b);
}

double median(double *array, const unsigned length) {
    qsort(array, length, sizeof(double), cmpfunc);
    return (length % 2 == 1) ? array[length / 2] : (array[length / 2] + array[length / 2 - 1]) / 2;
}

void swap(unsigned *index, const unsigned i, const unsigned j) {
    const unsigned tmp = index[i];
    index[i] = index[j];
    index[j] = tmp;
}

unsigned partition(const double *array, unsigned *index, const unsigned left, const unsigned right,
                   const unsigned pivot) {
    swap(index, pivot, right);
    unsigned j = left;
    for (unsigned i = left; i < right; i++) {
        if (array[index[i]] <= array[index[right]]) {
            swap(index, i, j);
            j++;
        }
    }
    swap(index, right, j);
    return j;
}

void get_sort_order(double *array, unsigned *index, const unsigned left, const unsigned right) {
    if (left < right) {
        const unsigned pivot = partition(array, index, left, right, (left + right) / 2);
        get_sort_order(array, index, left, pivot - 1);
        get_sort_order(array, index, pivot + 1, right);
    }
}

double weighted_median(double *b, const double *z, const unsigned nb_elements) {
    unsigned index[nb_elements];
    for (unsigned i = 0; i < nb_elements; i++) index[i] = i;

    get_sort_order(b, index, 0, nb_elements - 1);

    double sum_left = 0;
    double sum_right = sum_double_array(z, nb_elements);
    unsigned i;

    for (i = 0; i < nb_elements; i++) {
        sum_left += z[index[i]];
        sum_right -= z[index[i]];

        if (sum_left > sum_right) break;
    }

    return b[index[i]];
}

// ===== Init =====

// Init clusters with random values from elements
void initClusters(const Interval *const *elements, Interval **clusters, const unsigned nb_elements,
                  const unsigned nb_clusters, const unsigned nb_interval) {
    // Initialize index array with all available indices
    unsigned t[nb_elements];
    for (unsigned i = 0; i < nb_elements; i++) t[i] = i;

    for (unsigned i = 0; i < nb_clusters; i++) {
        // Get a random index
        const unsigned ind = range_rand(0, nb_elements - i - 1);

        // Copy the element at the random index to the cluster
        for (unsigned j = 0; j < nb_interval; j++) {
            clusters[i][j] = elements[t[ind]][j];
        }

        // Swap the selected index with the last unprocessed one (swap to the
        // end) This remove the index from the random choice
        swap(t, ind, nb_elements - i - 1);
    }
}

// Init clusters with random values from elements for non Interval data
void initVectorClusters(const double *const *elements, double **clusters,
                        const unsigned nb_elements, const unsigned nb_clusters,
                        const unsigned nb_dim) {
    // Initialize index array with all available indices
    unsigned t[nb_elements];
    for (unsigned i = 0; i < nb_elements; i++) t[i] = i;

    for (unsigned i = 0; i < nb_clusters; i++) {
        // Get a random index
        const unsigned ind = range_rand(0, nb_elements - i - 1);

        // Copy the element at the random index to the cluster
        for (unsigned j = 0; j < nb_dim; j++) {
            clusters[i][j] = elements[t[ind]][j];
        }

        // Swap the selected index with the last unprocessed one (swap to the
        // end) This remove the index from the random choice
        swap(t, ind, nb_elements - i - 1);
    }
}

// ===== PSEUDO-INVERSE =====

#ifdef GSL_VERSION
// Code from: https://gist.github.com/turingbirds/5e99656e08dbe1324c99
gsl_matrix *moore_penrose_pinv(gsl_matrix *A, const double rcond) {
    gsl_matrix *V, *Sigma_pinv, *U, *A_pinv;
    gsl_matrix *_tmp_mat = NULL;
    gsl_vector *_tmp_vec;
    gsl_vector *u;
    double x, cutoff;
    unsigned i, j;
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
#endif
