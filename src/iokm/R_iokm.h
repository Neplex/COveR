/*
This file is used to make interface between R and C code (C code is independent)
*/
#ifndef R_IOKM_H
#define R_IOKM_H

#include <Rdefines.h>

// ===== Function =====

/**
 * @brief Interface between R and C code for iokm
 * @param Rvec the R vector of data
 * @param Rx the number of elements (dim x)
 * @param Ry the number of values (dim y), always 2 (min, max)
 * @param Rz the number of intervals (dim y)
 * @param Rnc the number of wanted clusters
 * @param Rns the number of execution
 * @param Rd the distance to use
 * @param Ra the algorithm to use
 * @param Ru the update to use
 * @param Rt true for trace
 * @param Rim max iteration
 * @param Rs
 * @param Rcent the generate centers
 * @return an R list with results
 */
SEXP R_iokm(SEXP Rvec, SEXP Rx, SEXP Ry, SEXP Rz, SEXP Rnc, SEXP Rns, SEXP Rd, SEXP Ra, SEXP Ru,
            SEXP Rt, SEXP Rim, SEXP Rs, SEXP Rcent);

#endif
