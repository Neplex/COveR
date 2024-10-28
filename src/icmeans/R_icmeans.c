#include "R_icmeans.h"

#include "helpers.h"
#include "icmeans.h"

SEXP R_icmeans(SEXP Rdata, SEXP Rx, SEXP Ry, SEXP Rz, SEXP Rnc, SEXP Rm, SEXP Rns, SEXP Rd, SEXP Rt,
               SEXP Rim, SEXP Rcent) {
    unsigned i, j;
    SEXP Rcluster, Rcenters, Rtot, Rwss, Rtotwss, Riter, Rlist;

    // R types to C types
    const double *data_vec = NUMERIC_POINTER(Rdata);  // Data vector (elements)
    // const unsigned n = GET_LENGTH(Rdata);             // Length of data vector
    const unsigned x = INTEGER_VALUE(Rx);  // First dim size
    // const unsigned y = INTEGER_VALUE(Ry);             // Second dim size
    const unsigned z = INTEGER_VALUE(Rz);    // Third dim size
    const unsigned nc = INTEGER_VALUE(Rnc);  // Number of clusters
    const double m = NUMERIC_VALUE(Rm);      // Degree of fuzzification
    unsigned ns = INTEGER_VALUE(Rns);        // Nb of execution to find the best result
    const unsigned d = INTEGER_VALUE(Rd);    // Distance to use
    const bool t = LOGICAL_VALUE(Rt);        // Trace
    const unsigned im = INTEGER_VALUE(Rim);  // Nb of maximum iteration

    if (Rcent != NULL_USER_OBJECT) {
        ns = 1;
    }

    // Transform the data vector to an Interval 2D array
    Interval *const *intervals = new_matrix_Interval(x, z);
    if (!intervals) error("Memory allocation failed !");

    for (i = 0; i < x; i++) {
        for (j = 0; j < z; j++) {
            intervals[i][j].min = data_vec[i + j * 2 * x];
            intervals[i][j].max = data_vec[i + j * 2 * x + x];
        }
    }
    const Interval *const *elements = (const Interval *const *)intervals;

    Interval **centers = new_matrix_Interval(nc, z);
    double **asso = new_matrix_double(x, nc);
    double *withinss = new_array_double(nc);

    if (!centers || !asso || !withinss) {
        delete_matrix((void ***)&intervals, x);
        if (centers != NULL) delete_matrix((void ***)&centers, x);
        if (asso != NULL) delete_matrix((void ***)&asso, x);
        if (withinss != NULL) delete_array((void **)&withinss);
        error("Memory allocation failed !");
    }

    double tot = 0;
    double totwss = INFINITY;
    unsigned iteration = 0;

    // Execute icmeans
    for (j = 0; j < ns; j++) {
        Interval **c = new_matrix_Interval(nc, z);
        double **a = new_matrix_double(x, nc);
        double *w = new_array_double(nc);
        double to, tw;
        unsigned it;

        PRINT_START(t, "icmeans", j);

        if (Rcent != NULL_USER_OBJECT) {
            const double *centers_vec = NUMERIC_POINTER(Rcent);
            for (i = 0; i < nc; i++) {
                for (j = 0; j < z; j++) {
                    c[i][j].min = centers_vec[i + j * 2 * nc];
                    c[i][j].max = centers_vec[i + j * 2 * nc + nc];
                }
            }

        } else {
            initClusters(elements, c, x, nc, z);
        }

        icmeans(elements, c, a, x, nc, z, m, d, t, im, w, &to, &tw, &it);

        if (tw < totwss) {
            copy_matrix(c, centers, nc, z);
            copy_matrix(a, asso, x, nc);
            copy_array(w, withinss, nc);
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
    PROTECT(Rcluster = allocMatrix(REALSXP, x, nc));
    for (i = 0; i < x; i++)
        for (j = 0; j < nc; j++) REAL(Rcluster)[i + j * x] = asso[i][j];

    PROTECT(Rcenters = alloc3DArray(REALSXP, nc, 2, z));
    for (i = 0; i < nc; i++) {
        for (j = 0; j < z; j++) {
            REAL(Rcenters)[i + j * 2 * nc] = centers[i][j].min;
            REAL(Rcenters)[i + j * 2 * nc + nc] = centers[i][j].max;
        }
    }

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
