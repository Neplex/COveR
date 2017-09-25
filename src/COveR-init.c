// Interval clustering
#include "icmeans/R_icmeans.h"
#include "ikmeans/R_ikmeans.h"
#include "ineokm/R_ineokm.h"
#include "iokm/R_iokm.h"

// Classic clustering
#include "neokm/R_neokm.h"
#include "okm/okm.h"
#include "r1okm/R_r1okm.h"
#include "r2okm/R_r2okm.h"

#include <R.h>
#include <R_ext/Rdynload.h>
#include <Rinternals.h>

static const R_CallMethodDef callMethods[] = {
    {"_icmeans", (DL_FUNC)&R_icmeans, 11}, {"_ikmeans", (DL_FUNC)&R_ikmeans, 10},
    {"_ineokm", (DL_FUNC)&R_ineokm, 11},   {"_iokm", (DL_FUNC)&R_iokm, 13},
    {"_neokm", (DL_FUNC)&R_neokm, 10},     {"_r1okm", (DL_FUNC)&R_r1okm, 9},
    {"_r2okm", (DL_FUNC)&R_r2okm, 9},      {NULL, NULL, 0}};

static R_NativePrimitiveArgType okm_t[] = {
    REALSXP, REALSXP, INTSXP, INTSXP,  INTSXP, INTSXP,  INTSXP, REALSXP,
    REALSXP, INTSXP,  REALSXP, REALSXP, REALSXP, REALSXP, REALSXP, INTSXP};
static const R_CMethodDef cMethods[] = {{"_okm", (DL_FUNC)&R_okm, 16, okm_t},
                                           {NULL, NULL, 0}};

void R_init_COveR(DllInfo *info) {
  R_registerRoutines(info, cMethods, callMethods, NULL, NULL);
  R_useDynamicSymbols(info, FALSE);
  R_forceSymbols(info, FALSE);
}
