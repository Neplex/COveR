#ifndef HELPERS_H
#define HELPERS_H

#include <R.h>
#include <Rdefines.h>
#include <math.h>
#include <memory.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#include "interval.h"

#ifdef GSL_VERSION
// GNU Scientific Library
#include <gsl/gsl_linalg.h>
#endif

// Useful math functions
#define max(x, y) ((x > y) ? x : y)
#define min(x, y) ((x < y) ? x : y)

// ===== Type definition =====

typedef enum { STD, MATRIX } Algorithm;

typedef enum { MEAN, SUM, JOIN, MEET } Update;

// ===== Alloc =====

// Allocate an array
unsigned *new_array_unsigned(unsigned length);

double *new_array_double(unsigned length);

Interval *new_array_Interval(unsigned length);

// Allocate a matrix
bool **new_matrix_bool(unsigned size_x, unsigned size_y);

unsigned **new_matrix_unsigned(unsigned size_x, unsigned size_y);

double **new_matrix_double(unsigned size_x, unsigned size_y);

Interval **new_matrix_Interval(unsigned size_x, unsigned size_y);

/**
 * Delete an array and free up memory
 */
void delete_array(void **array);

/**
 * Delete a matrix and free up memory
 */
void delete_matrix(void ***matrix, unsigned size_x);

// ===== Copy =====

// Copy array src to dest, not safe as pointer function but not need to cast
#define copy_array(src, dest, length) memcpy(dest, src, length * sizeof(*src))

// Copy matrix src to dest, not safe as pointer function but not need to cast
#define copy_matrix(src, dest, size_x, size_y) \
    for (unsigned i = 0; i < size_x; i++) copy_array(src[i], dest[i], size_y);

// ===== Operator =====

// Sum all element in array
double sum_double_array(const double *a, unsigned length);

double median(double *array, unsigned length);

void swap(unsigned *index, unsigned i, unsigned j);

unsigned partition(const double *array, unsigned *index, unsigned left, unsigned right,
                   unsigned pivot);

void get_sort_order(double *array, unsigned *index, unsigned left, unsigned right);

double weighted_median(double *b, const double *z, unsigned nb_elements);

// ===== Init =====

// Init clusters with random values from elements
void initClusters(const Interval *const *elements, Interval **clusters, unsigned nb_elements,
                  unsigned nb_clusters, unsigned nb_interval);

// Init clusters with random values from elements for non Interval data
void initVectorClusters(const double *const *elements, double **clusters, unsigned nb_elements,
                        unsigned nb_clusters, unsigned nb_dim);

// ===== Trace =====

// Macro for 'trace', used when call
#define PRINT_START(trace_flag, label, count) \
    if (trace_flag) Rprintf("%s: %u\n", label, count);

// Macro for 'trace', used at all iterations
#define PRINT_ITER(trace_flag, iteration, assign_val, update_val)                              \
    if (trace_flag)                                                                            \
        Rprintf("\t(iter: %u, assign: %f, update: %f)%s\n", iteration, assign_val, update_val, \
                (assign_val < update_val) ? "\tWarning: bad update" : "");

// ===== PSEUDO-INVERSE =====

#ifdef GSL_VERSION
/**
 *Code from: https://gist.github.com/turingbirds/5e99656e08dbe1324c99
 **/
gsl_matrix *moore_penrose_pinv(gsl_matrix *A, const double rcond);
#endif

#endif
