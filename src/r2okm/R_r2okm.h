/*
This file is used to make interface between R and C code (C code is
independent)
*/

#ifndef R_R2OKM_H
#define R_R2OKM_H

#include <Rdefines.h>

/**
 * @brief Interface between R and C code for r2okm
 * @param Rvec the R vector of data
 * @param Rx the number of elements (dim x)
 * @param Ry the number of dimensions (dim y)
 * @param Rnc the number of wanted clusters
 * @param Rl lambda
 * @param Rns the number of execution
 * @param Rt true for trace
 * @param Rim max iteration
 * @param Rcent the generate centers
 * @return a R list with results
 */
SEXP R_r2okm(SEXP Rvec, SEXP Rx, SEXP Ry, SEXP Rnc, SEXP Rl, SEXP Rns, SEXP Rt, SEXP Rim,
             SEXP Rcent);

#endif
