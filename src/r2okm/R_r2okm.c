#include "R_r2okm.h"

#include <Rdefines.h>

#include "helpers.h"
#include "r2okm.h"

SEXP R_r2okm(SEXP Rvec, SEXP Rx, SEXP Ry, SEXP Rnc, SEXP Rl, SEXP Rns, SEXP Rt, SEXP Rim,
             SEXP Rcent) {
    unsigned i, j;
    SEXP Rcluster, Rcenters, Rtot, Rwss, Rtotwss, Riter, Rlist;

    // R types to C types
    const double *data_vec = NUMERIC_POINTER(Rvec);  // Data vector (elements)
    // const unsigned n = GET_LENGTH(Rvec);             // Length of data vector
    const unsigned x = INTEGER_VALUE(Rx);     // First dim size
    const unsigned y = INTEGER_VALUE(Ry);     // Second dim size
    const unsigned nc = INTEGER_VALUE(Rnc);   // Number of clusters
    const double lambda = NUMERIC_VALUE(Rl);  // Alpha
    unsigned ns = INTEGER_VALUE(Rns);         // Nb of execution to find the best result
    const bool t = (bool)LOGICAL_VALUE(Rt);   // Trace
    const unsigned im = INTEGER_VALUE(Rim);   // Nb of maximum iteration

    // Pre generate centers
    if (Rcent != NULL_USER_OBJECT) {
        ns = 1;
    }

    // Transform the data vector to a 2D array
    double *const *data_matrix = new_matrix_double(x, y);
    if (!data_matrix) error("Memory allocation failed !");

    for (i = 0; i < x; i++) {
        for (j = 0; j < y; j++) {
            data_matrix[i][j] = data_vec[i + j * x];
        }
    }
    const double *const *elements = (const double *const *)data_matrix;

    double **centers = new_matrix_double(nc, y);
    bool **asso = new_matrix_bool(x, nc);
    double *withinss = new_array_double(x);

    if (!centers || !asso || !withinss) {
        delete_matrix((void ***)&data_matrix, x);
        if (centers != NULL) delete_matrix((void ***)&centers, x);
        if (asso != NULL) delete_matrix((void ***)&asso, x);
        if (withinss != NULL) delete_array((void **)&withinss);
        error("Memory allocation failed !");
    }

    double tot = 0;
    double totwss = INFINITY;
    unsigned iteration = 0;

    // Execute r2okm
    for (j = 0; j < ns; j++) {
        double **c = new_matrix_double(nc, y);
        bool **a = new_matrix_bool(x, nc);
        double *w = new_array_double(x);
        double to, tw;
        unsigned it;

        PRINT_START(t, "r2okm", j);

        if (Rcent != NULL_USER_OBJECT) {
            const double *centers_vec = NUMERIC_POINTER(Rcent);
            for (i = 0; i < nc; i++) {
                for (j = 0; j < y; j++) {
                    c[i][j] = centers_vec[i + j * nc];
                }
            }

        } else {
            initVectorClusters(elements, c, x, nc, y);
        }

        r2okm(elements, c, a, x, nc, y, lambda, t, im, w, &to, &tw, &it);

        if (tw < totwss) {
            copy_matrix(c, centers, nc, y);
            copy_matrix(a, asso, x, nc);
            copy_array(w, withinss, x);
            tot = to;
            totwss = tw;
            iteration = it;

            if (tw == 0) break;
        }

        delete_matrix((void ***)&c, nc);
        delete_matrix((void ***)&a, x);
        delete_array((void **)&w);
    }

    // Results
    PROTECT(Rcluster = allocMatrix(LGLSXP, x, nc));
    for (i = 0; i < x; i++)
        for (j = 0; j < nc; j++) LOGICAL(Rcluster)[i + j * x] = asso[i][j];

    PROTECT(Rcenters = allocMatrix(REALSXP, nc, y));
    for (i = 0; i < nc; i++)
        for (j = 0; j < y; j++) REAL(Rcenters)[i + j * nc] = centers[i][j];

    PROTECT(Rtot = ScalarReal(tot));

    PROTECT(Rwss = NEW_NUMERIC(nc));
    for (i = 0; i < nc; i++) REAL(Rwss)[i] = withinss[i];

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

    delete_matrix((void ***)&elements, x);
    delete_matrix((void ***)&centers, nc);
    delete_matrix((void ***)&asso, x);
    delete_array((void **)&withinss);

    return Rlist;
}
