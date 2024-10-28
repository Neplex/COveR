/*
This file is used to make interface between R and C code (C code is independent)
*/
#ifndef R_R1OKM_H
#define R_R1OKM_H

#include <Rdefines.h>

/**
 * @brief Interface between R and C code for r1okm
 * @param Rvec the R vector of data
 * @param Rx the number of elements (dim x)
 * @param Ry the number of dimensions (dim y)
 * @param Rnc the number of wanted clusters
 * @param Ra alpha
 * @param Rns the number of execution
 * @param Rt true for trace
 * @param Rim max iteration
 * @param Rcent the generate centers
 * @return an R list with results
 */
SEXP R_r1okm(SEXP Rvec, SEXP Rx, SEXP Ry, SEXP Rnc, SEXP Ra, SEXP Rns, SEXP Rt, SEXP Rim,
             SEXP Rcent);

#endif
