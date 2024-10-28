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

#ifndef OKM_H
#define OKM_H

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
           double *swss, int *scl, double *scen, int *ex, const int *pmet);

#endif
