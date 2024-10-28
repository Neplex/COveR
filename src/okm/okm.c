/*
Overlapping K-Means for R
2011  Guillaume Cleuziou

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the author.
guillaume.cleuziou@univ-orleans.fr
*/

// must be compiled with R CMD SHLIB okm.c

#include <R.h>
#include <math.h>
#define DBG printf("%s : %d\n", __FILE__, __LINE__);

// return the smallest elements' index of vector v1 of dimension dim
int indmin(const double *v1, const int dim, const int index) {
    int min = -1;
    const double base = index == -1 ? -1 : v1[index];
    for (int i = 0; i < dim; i++) {
        if (min == -1)
            if (v1[i] >= base && i != index)
                min = i;
            else
                ;
        else if (v1[i] >= base && i != index && v1[i] < v1[min])
            min = i;
    }
    return min;
}

// return the Euclidean square distance between vectors v1 and v2 of dimension dim
double okm_square_distance(const double *v1, const double *v2, const int dim) {
    double dd = 0.0;
    for (int i = 0; i < dim; i++) {
        double tmp = v1[i] - v2[i];
        dd += tmp * tmp;
    }
    return dd;
}

// return the manhattan distance between vectors v1 and v2 of dimension dim
double man_distance(const double *v1, const double *v2, const int dim) {
    double d = 0.0;
    for (int i = 0; i < dim; i++) {
        const double tmp = v1[i] - v2[i];
        d += tmp < 0 ? -tmp : tmp;
    }
    return d;
}

// write in res the product of the vector v1 of dimension dim with the number
// fac.
void multiply_vn(double *res, const double *v1, const double fac, const int dim) {
    for (int i = 0; i < dim; i++) res[i] = fac * v1[i];
}

// init a turn of the main loop with the computation of the square distance
// between each Xi and each centers.
void compute_sq_distances(const double *x, double *dist, const double *cen, int *ca, int *cl, int n,
                          int p, int k) {
    for (int i = 0; i < n; i++) {
        for (int c = 0; c < k; c++) {
            if (ca) ca[c + i * k] = cl[c + i * k];
            cl[c + i * k] = 0;
            dist[c + i * k] = okm_square_distance(&x[i * p], &cen[c * p], p);
        }
    }
}

// init a turn of the main loop with the computation of the square distance
// between each Xi and each centers.
void compute_man_distances(const double *x, double *dist, const double *cen, int *ca, int *cl,
                           int n, int p, int k) {
    for (int i = 0; i < n; i++) {
        for (int c = 0; c < k; c++) {
            if (ca) ca[c + i * k] = cl[c + i * k];
            cl[c + i * k] = 0;
            dist[c + i * k] = man_distance(&x[i * p], &cen[c * p], p);
        }
    }
}

// compute the mean of the two vector v1 and v2 of dimension dim
void mean_2(double *res, const double *v1, const double *v2, const int dim, const int c) {
    for (int i = 0; i < dim; i++) res[i] = (v2[i] * (c) + v1[i]) / (double)(c + 1);
}

// process to the attribution of clusters by using manhattan distances for each
// Xi and return the convergence criterion.
double man_attribution(const double *x, const double *cen, int *cl, int *ca, const int n,
                       const int p, const int k, int *ai) {
    int j, it;
    double dist[n * k], res[p], res2[p], dd2;
    double ret = 0.0;

    compute_man_distances(x, dist, cen, ca, cl, n, p, k);

    // for each Xi, find his closest center(s)
    for (int i = 0; i < n; i++) {
        int idm = indmin(&dist[i * k], k, -1);
        cl[i * k + idm]++;
        double dd1 = dist[i * k + idm];
        // printf("i = %d, idm = %d, dd1 = %f\n", i, idm, dd1);
        ai[i] = 1;
        for (j = 0; j < p; j++) res[j] = cen[p * idm + j];

        for (int c = 1; c < k; c++) {
            idm = indmin(&dist[i * k], k, idm);
            mean_2(res, &cen[p * idm], res, p, c);
            dd2 = man_distance(res, &x[i * p], p);
            // printf("--->idm = %d, dd1 = %f, dd2 = %f\n", idm, dd1, dd2);

            if (dd2 < dd1) {
                cl[i * k + idm]++;
                ai[i]++;
                dd1 = dd2;
                // printf("--->toto\n");
            } else
                break;
        }

        if (ca) {
            for (j = 0; j < p; j++) res2[j] = 0.0;
            int count = 0;
            for (it = 0; it < k; it++) {
                if (ca[i * k + it]) {
                    for (j = 0; j < p; j++) res2[j] += cen[it * p + j];
                    count++;
                }
            }
            for (j = 0; j < p; j++) {
                res2[j] = res2[j] / (double)count;
            }

            dd2 = man_distance(res2, &x[i * p], p);

            if (dd2 < dd1) {
                for (it = 0; it < k; it++) {
                    cl[i * k + it] = ca[i * k + it];
                }
                ai[i] = count;
                ret += dd2;
            } else {
                ret += dd1;
            }
        } else
            ret += dd1;
        // printf("%f\n", ret);
    }
    /*
       for(i = 0; i < n; i++){
         for(j = 0; j < k; j++)
             printf("%f ", dist[i*k+j]);
         for(j = 0; j < k; j++)
             printf("%d ", cl[i*k+j]);
         printf("\n");}
     //*/
    return ret;
}

// proccess to the attribution of clusters by using euclidian distances for
// each Xi and return the convergence criterion.
double euc_attribution(const double *x, const double *cen, int *cl, int *ca, int n, int p, int k) {
    double dd2;
    int it, j;
    double res[p], res2[p], dist[n * k];
    double ret = 0.0, ret2 = 0.0;

    compute_sq_distances(x, dist, cen, ca, cl, n, p, k);

    // for each Xi, we attribute his closest center(s)
    for (int i = 0; i < n; i++) {
        // nearest cluster selection
        int idm = indmin(&dist[i * k], k, -1);
        cl[i * k + idm]++;
        // must we add other clusters ?
        double dd1 = dist[i * k + idm];
        for (j = 0; j < p; j++) res[j] = cen[p * idm + j];

        for (int c = 1; c < k; c++) {
            // next nearest cluster selection
            idm = indmin(&dist[i * k], k, idm);
            // if the square distance between the mean of the clusters already
            // associated to Xi plus the next nearest cluster
            // is lower than the mean of the clusters already associated to
            // Xi then the current cluster is associated else all the other
            // clusters are not associated, and we exit the loop
            mean_2(res, &cen[idm * p], res, p, c);
            dd2 = okm_square_distance(res, &x[i * p], p);
            if (dd2 < dd1) {
                cl[i * k + idm]++;
                dd1 = dd2;
            } else
                break;
        }

        // if the previous affectation is better (i.e. the square distance
        // of the element and his old centers is lower than the new ones),
        // we keep the previous affectation.
        if (ca) {
            for (j = 0; j < p; j++) res2[j] = 0.0;
            int count = 0;
            for (it = 0; it < k; it++) {
                if (ca[i * k + it]) {
                    for (j = 0; j < p; j++) res2[j] += cen[it * p + j];
                    count++;
                }
            }
            for (j = 0; j < p; j++) {
                res2[j] = res2[j] / (double)count;
            }
            dd2 = okm_square_distance(res2, &x[i * p], p);
            ret2 += dd2;
            if (dd2 < dd1) {
                for (it = 0; it < k; it++) {
                    cl[i * k + it] = ca[i * k + it];
                }
                ret += dd2;
            } else {
                ret += dd1;
            }
        } else
            ret += dd1;
    }
    return ret;
}

void compute_di_pi(int *di, double *pi, const int *cl, const int n, const int k) {
    for (int i = 0; i < n; i++) {
        int count = 0;
        for (int c = 0; c < k; c++) {
            if (cl[i * k + c]) count++;
        }
        di[i] = count;
        pi[i] = 1.0 / (double)(count * count);
    }
}

double man_sum_vector(const double *v, const int dim) {
    double res = 0.0;

    for (int i = 0; i < dim; i++) res += fabs(v[i]);

    return res;
}

void man_compute_sum(const double *cen, const int *ai, double *res, const int *cl, const int p,
                     const int k, const int num, const int id) {
    int j;
    for (j = 0; j < p; j++) res[j] = 0;
    for (int c = 0; c < k; c++) {
        if (cl[id * k + c] && c != num)
            for (j = 0; j < p; j++) res[j] += cen[c * p + j];
    }
    for (j = 0; j < p; j++) res[j] /= (double)ai[id];
}

int compute_cluster_cardinal(const int *cl, const int num, const int n, const int k) {
    int res = 0;
    for (int i = 0; i < n; i++)
        if (cl[i * k + num]) res++;

    return res;
}

void man_compute_center(double *cen, const int num, const double *x, const int *ai, const int *cl,
                        const int n, const int p, const int k) {
    int i, j, c;
    int u = 0;
    const int count = compute_cluster_cardinal(cl, num, n, k);
    double bi[count * p], tbi[p * count], yi[count * p], zi[count], zii[count];

    for (i = 0; i < n; i++) {
        if (cl[i * k + num]) {
            double res[p];
            zi[u] = 1 / (double)ai[i];
            man_compute_sum(cen, ai, res, cl, p, k, num, i);
            for (j = 0; j < p; j++) yi[u * p + j] = x[i * p + j] - res[j];
            u++;
        }
    }

    // printf("u = %d count = %d\n", u, count);

    for (i = 0; i < count; i++)
        for (j = 0; j < p; j++) bi[i * p + j] = yi[i * p + j] / zi[i];

    for (i = 0; i < p; i++)
        for (c = 0; c < count; c++)
            // tbi[i*count+c] = (bi[c*p+i] < 0) ? -bi[c*p+i] : bi[c*p+i];
            tbi[i * count + c] = bi[c * p + i];

    // for each col
    for (i = 0; i < p; i++) {
        for (j = 0; j < count; j++) zii[j] = zi[j];

        // T = man_sum_vector(&tbi[i*count], count);
        double T = man_sum_vector(zi, count);
        // printf("T=%f\n",T);
        double bisum = 0.0;
        c = -1;
        while (bisum < T / 2 && c < count) {
            c++;
            int r = i * count + c;
            int jj = c;
            for (j = c; j < count; j++) {
                if (tbi[i * count + j] <= tbi[r] && i * count + j != r) {
                    r = i * count + j;
                    jj = j;
                };
            }
            double itmp = tbi[r];
            double ztmp = zii[jj];
            tbi[r] = tbi[i * count + c];
            zii[jj] = zii[c];
            tbi[i * count + c] = itmp;
            zii[c] = ztmp;
            bisum += fabs(ztmp);  // fabs(tbi[i*count+c]);
            // printf("Sum1=%f - Suim2=%f = Diff=%f\n",bisum,T-bisum,2*bisum-T);
        }

        // if(bisum==T/2) cen[num*p+i]=(tbi[i*count+c]+tbi[i*count+c-1])/2;
        // else
        cen[num * p + i] = tbi[i * count + c];
    }
    /*
    printf("\n-------center %d-------\n", num);
    for(i = 0; i < p; i++)
        printf("%f ",cen[num*p+i]);
        //*/
}

// start new centers calculation
// find all the elements of a cluster
// compute the vectors Di and Pi for these elements
// for each cluster
// for each Xi, compute Yi the centers off all the clusters Xi is linked except
// the current cluster
// compute Z as
// Z = (the number of cluster of each element of the current cluster * the
// description of the element) -
//(the number of cluster of each element of the current cluster -1) * Yi
// le nouveau centre sera une moyenne pondérée par Pi de Z, pour chaque colonne
// de la matrice.
// the new centers will be a mean weighted by Pi on each column of the Z
// matrix.
// compute the new centers
void euc_compute_center(double *cen, const int num, const double *x, const int *di,
                        const double *pi, const int *cl, const int n, const int p, const int k) {
    int j;
    double tmp = 0.0;
    double res[p], z[p];
    // select all the xi in the cluster num and
    // compute the mean of all their other clusters
    for (j = 0; j < p; j++) z[j] = 0.0;
    for (int i = 0; i < n; i++) {
        if (cl[i * k + num]) {
            tmp += pi[i];
            for (j = 0; j < p; j++) res[j] = 0.0;
            for (int c = 0; c < k; c++) {
                if (cl[i * k + c] && c != num)
                    for (j = 0; j < p; j++) res[j] += cen[c * p + j];
            }

            // res is the mean of all the Xis cluster except the current
            if (di[i] == 1)
                for (j = 0; j < p; j++) z[j] += x[i * p + j];
            else {
                double res2[p];
                multiply_vn(res2, &x[i * p], di[i], p);
                for (j = 0; j < p; j++) z[j] += (res2[j] - res[j]) * pi[i];
            }
        }
    }
    if (tmp != 0.0)
        for (j = 0; j < p; j++) cen[num * p + j] = z[j] / tmp;
}

// return 1 if the vectors v1 and v2 are identical else 0
int identical(const int *v1, const int *v2, const int dim) {
    int i = 0, res = 1;
    while (res && i < dim) {
        if (v1[i] != v2[i]) res = 0;
        i++;
    }
    return res;
}

double debug_compute_wss(const double *x, const double *cen, const int *cl, const int n,
                         const int p, const int k) {
    double ret = 0.0;
    int j;

    double res[p];

    for (int i = 0; i < n; i++) {
        for (j = 0; j < p; j++) res[j] = 0.0;
        int count = 0;
        for (int c = 0; c < k; c++) {
            if (cl[i * k + c]) {
                for (j = 0; j < p; j++) res[j] += cen[c * p + j];
                count++;
            }
        }

        for (j = 0; j < p; j++) res[j] /= count;

        ret += man_distance(&x[i * p], res, p);
    }

    return ret;
}

// process to the overlapping k-means algorithm on a matrix
// double * x the matrix to clustered
// double * cen the centers matrix
// int maxIter : the maximum number of iterations
// int k : the number of clusters
// int p : the number of columns in the x matrix
// int n : the number of rows in the matrix x
// int * cl : the matrix which associate for each element of X their cluster(s)
// (result)
// double wss : the convergence criterion(result)
// double over : the mean number of clusters for each element of x (result)
// int visu : if true show the convergence criterion after each attribution
// int save : should the intermediate result be conserved ?
// int * swss : the convergence criterion after each turn (if (save) result)
// int * scl : the clusters attribution after each turn (if (save) result)
// int * scen : the centers matrix after each turn (if (save) result)
// int * ex : the reached iteration (result)
void R_okm(const double *x, double *cen, const int *pmax, const int *pk, const int *pp,
           const int *pn, int *cl, double *pwss, double *pover, const int *pvisu, const int *psave,
           double *swss, int *scl, double *scen, int *ex, const int *pmet) {
    int iterMax = *pmax;
    int k = *pk, p = *pp, n = *pn, nk = n * k, visu = *pvisu;
    int it, save = *psave;
    int ai[n];
    const int met = *pmet;
    char method = 0;

    switch (met) {
        case 2:
            method = 'm';
            break;
        case 1:
        default:
            method = 'e';
            break;
    }

    // to be saved : cluster attribution for each step, wss vector, centers matrix
    double wss = method == 'e' ? euc_attribution(x, cen, cl, NULL, n, p, k)
                               : man_attribution(x, cen, cl, NULL, n, p, k, ai);

    if (save) swss[0] = wss;
    if (visu) Rprintf("WSS at first turn : %f\n", wss);
    for (int iter = 0; iter < iterMax; iter++) {
        int ca[n * k];
        // DBG
        if (method == 'e') {
            int di[n];
            double pi[n * p];
            compute_di_pi(di, pi, cl, n, k);
            for (it = 0; it < k; it++) {
                euc_compute_center(cen, it, x, di, pi, cl, n, p, k);
            }

            wss = euc_attribution(x, cen, cl, ca, n, p, k);
        }

        if (method == 'm') {
            for (it = 0; it < k; it++) {
                man_compute_center(cen, it, x, ai, cl, n, p, k);
            }

            wss = man_attribution(x, cen, cl, ca, n, p, k, ai);
        }
        if (visu) Rprintf("WSS at turn %d : %f\n", iter, wss);

        if (save) {
            swss[iter + 1] = wss;

            // centers saving
            for (it = 0; it < k * p; it++) scen[iter * k * p + it] = cen[it];

            // clusters attribution saving
            for (it = 0; it < n * k; it++) scl[iter * n * k + it] = cl[it];
            *ex = iter;
        }
        if (identical(ca, cl, nk)) break;
    }

    double tmp = 0.0;
    for (int i = 0; i < n; i++) {
        int count = 0;
        for (it = 0; it < k; it++)
            if (cl[i * k + it]) count++;
        tmp += (double)count;
    }
    *pover = tmp / (double)n;
    *pwss = wss;
}
