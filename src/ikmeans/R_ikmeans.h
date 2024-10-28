/*
This file is used to make interface between R and C code (C code is independent)
*/
#ifndef R_IKMEANS_H
#define R_IKMEANS_H

#include <Rdefines.h>

/**
 * @brief Interface between R and C code for ikmeans
 * @param Rdata the R vector of data
 * @param Rx the number of elements (dim x)
 * @param Ry the number of values (dim y), always 2 (min, max)
 * @param Rz the number of intervals (dim y)
 * @param Rnc the number of wanted clusters
 * @param Rns the number of execution
 * @param Rd the distance to use
 * @param Rt true for trace
 * @param Rim max iteration
 * @param Rcent the generate centers
 * @return an R list with results
 */
SEXP R_ikmeans(SEXP Rdata, SEXP Rx, SEXP Ry, SEXP Rz, SEXP Rnc, SEXP Rns, SEXP Rd, SEXP Rt,
               SEXP Rim, SEXP Rcent);

#endif
