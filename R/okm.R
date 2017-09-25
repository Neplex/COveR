# Overlappig K-Means 2011 Guillaume Cleuziou

# This program is free software; you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free Software
# Foundation; either version 2 of the License, or (at your option) any later
# version.

# This program is distributed in the hope that it will be useful, but WITHOUT ANY
# WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
# PARTICULAR PURPOSE.  See the GNU General Public License for more details.

# You should have received a copy of the GNU General Public License along with
# this program; if not, write to the author. guillaume.cleuziou@univ-orleans.fr


#' OKM clustering.
#'
#' Overlapping Kmeans algorithm.
#' @useDynLib COveR, .registration = TRUE
#'
#' @param X A data.
#' @param centers A number or matrix, number of cluster for clustering or pre init centers.
#' @param nstart A number, number of execution to find the best result.
#' @param method A string ('euclid': Euclidian distance, 'manhattan': Manhattan distance).
#' @param trace A boolean, tracing information on the progress of the algorithm is produced.
#' @param iter.max the maximum number of iterations allowed.
#'
#' @export
#'
#' @examples
#' okm(iris[,-5], 3)
okm <- function(X, centers, iter.max = 10, nstart = 1, trace = FALSE, method = "euclid") {
  Z <- X
  X <- as.matrix(Z)
  if (is.null(dim(X)))
    stop("X must be a matrix.")
  use.nstart <- FALSE
  if (is.null(dim(centers))) {
    use.nstart <- TRUE
    k <- centers
    cen <- X[sample(1:nrow(X), k), ]
  } else {
    cen <- as.matrix(centers)
    k <- nrow(cen)
    if (ncol(cen) != ncol(X))
      stop("X and centers must have the same number of columns.")
    if (any(duplicated(cen)))
      stop("Centers must be distincts.")
    nstart <- 1
  }
  if (iter.max < 0) {
    stop("iter.max must be a null or a positive number.")
  }
  if (nrow(X) < k)
    stop("More cluster than datas in X.")

  if (method == "euclid")
    method <- 1 else if (method == "manhattan")
    method <- 2 else stop("Unknown distance computation method.")
  U <- .C("_okm", as.double(t(X)), centers = as.double(t(cen)), as.integer(iter.max),
    as.integer(k), as.integer(ncol(X)), as.integer(nrow(X)), clusters = integer(nrow(X) *
      k), wss = as.double(k), over = as.double(k), as.integer(trace), 0, 0,
    0, 0, 0, as.integer(method))
  if (use.nstart && nstart > 1) {
    for (i in 2:nstart) {
      cen <- X[sample(1:nrow(X), k), ]
      UU <- .C("_okm", as.double(t(X)), centers = as.double(t(cen)), as.integer(iter.max),
        as.integer(k), as.integer(ncol(X)), as.integer(nrow(X)), clusters = integer(nrow(X) *
          k), wss = as.double(k), over = as.double(k), as.integer(trace),
        0, 0, 0, 0, 0, as.integer(method))
      if (UU$wss < U$wss)
        U <- UU
    }
  }
  clust <- matrix(U$clusters, ncol = k, byrow = TRUE)
  centers <- matrix(U$centers, ncol = ncol(X), nrow = k, byrow = TRUE)
  if (!is.null(colnames(Z)))
    colnames(centers) <- colnames(Z)

  # Result
  structure(list(cluster = clust, centers = centers, tot.withinss = U$wss, overlaps = U$over),
    class = "okm")
}

#' OKM print
#'
#' Print override for OKM
#'
#' @param x An OKM object.
#' @param ... Other options from print.
#'
#' @export
print.okm <- function(x, ...) {
  cat("OKM clustering with ", length(x$size), " clusters of sizes ", paste(x$size,
    collapse = ", "), "\n", sep = "")
  cat("\nCluster means:\n")
  print(x$centers, ...)
  cat("\nClustering matrix:\n")
  print(x$cluster, ...)
  cat("\nWithin cluster sum of squares by elements:\n")
  print(x$withinss, ...)
  cat(sprintf(" (between_SS / total_SS = %5.1f %%)\n", 100 * x$betweenss/x$totss),
    "Available components:\n", sep = "\n")
  print(names(x))
  invisible(x)
}
