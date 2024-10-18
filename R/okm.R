# Overlappig K-Means 2011 Guillaume Cleuziou

# This program is free software; you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free Software
# Foundation; either version 2 of the License, or (at your option) any later
# version.

# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
# details.

# You should have received a copy of the GNU General Public License along with
# this program; if not, write to the author. guillaume.cleuziou@univ-orleans.fr


#' Clusters data using the OKM (Overlapping K-Means) clustering algorithm.
#'
#' @param x A numeric data matrix or data frame containing the data to be
#' clustered.
#' @param centers Either a positive integer indicating the number of clusters to
#' create or a matrix of pre-initialized cluster centers.
#' @param iter.max Maximum number of iterations allowed for the clustering
#' algorithm (default is 10).
#' @param nstart Number of random initializations to find the best result
#' (default is 1).
#' @param trace Logical value indicating whether to display the progress of the
#' algorithm (default is `FALSE`).
#' @param method A string specifying the distance metric to use; options are
#' 'euclid' (Euclidean distance) or 'manhattan' (Manhattan distance)
#' (default is "euclid").
#' @return A list containing the clustering results, including:
#'   - `cluster`: Matrix indicating the cluster assignments for each data point.
#'   - `centers`: The final cluster centers.
#'   - `tot.withinss`: Total within-cluster sum of squares.
#'   - `overlaps`: The measure of overlap among clusters.
#' @useDynLib COveR, .registration = TRUE
#' @export
#' @examples
#' okm(iris[, -5], 3)
okm <- function(  # nolint cyclocomp_linter
  x, centers,
  iter.max = 10,  # nolint object_name_linter
  nstart = 1,
  trace = FALSE,
  method = "euclid"
) {

  # Check input validity
  stopifnot(
    "Data must be a numeric matrix or data frame." = is.data.frame(x) ||
      is.matrix(x) ||
      is.numeric(x),
    "'x' must be a matrix or data frame" = is.numeric(dim(x)),
    "'nstart' must be > 0" = is.numeric(nstart) && nstart > 0,
    "'trace' must be logical" = is.logical(trace),
    "'iter.max' must be > 0" = is.numeric(iter.max) && iter.max > 0
  )

  x <- as.matrix(x)

  use_nstart <- FALSE
  if (is.null(dim(centers))) {
    use_nstart <- TRUE
    k <- centers
    cen <- x[sample(seq_len(nrow(x)), k), ]
  } else {
    cen <- as.matrix(centers)
    k <- nrow(cen)
    if (ncol(cen) != ncol(x)) {
      stop("'x' and 'centers' must have the same number of columns.")
    }
    if (any(duplicated(cen))) stop("Centers must be distinct.")
    nstart <- 1
  }

  if (nrow(x) < k) stop("More cluster than datas in 'x'.")

  # Distance method selection
  dist <- switch(
    method,
    "euclid" = 1,
    "manhattan" = 2,
    stop("Unknown distance type. Use 'euclid' or 'manhattan'.")
  )

  # Call the C function for OKM clustering
  result <- .C(
    "_okm", as.double(t(x)),
    centers = as.double(t(cen)),
    as.integer(iter.max),
    as.integer(k),
    as.integer(ncol(x)),
    as.integer(nrow(x)),
    clusters = integer(nrow(x) * k),
    wss = as.double(k),
    over = as.double(k),
    as.integer(trace),
    0, 0, 0, 0, 0,
    as.integer(dist)
  )

  # Execute multiple starts if specified
  if (use_nstart && nstart > 1) {
    for (i in 2:nstart) {
      cen <- x[sample(seq_len(nrow(x)), k), ]
      new_result <- .C(
        "_okm",
        as.double(t(x)),
        centers = as.double(t(cen)),
        as.integer(iter.max),
        as.integer(k),
        as.integer(ncol(x)),
        as.integer(nrow(x)),
        clusters = integer(nrow(x) * k),
        wss = as.double(k),
        over = as.double(k),
        as.integer(trace),
        0, 0, 0, 0, 0,
        dist
      )
      if (new_result$wss < result$wss) result <- new_result
    }
  }

  # Process results
  cluster <- matrix(result$clusters, ncol = k, byrow = TRUE)
  centers <- matrix(result$centers, ncol = ncol(x), nrow = k, byrow = TRUE)
  if (!is.null(colnames(x))) colnames(centers) <- colnames(x)

  # Return the clustering results as a structured list
  structure(list(
    cluster = cluster,
    centers = centers,
    tot.withinss = result$wss,
    overlaps = result$over
  ), class = "okm")
}

#' Displays the results of OKM clustering in a readable format.
#'
#' @param x An OKM object resulting from the `okm` function.
#' @param ... Additional arguments passed to print().
#' @return No return value, it prints the clustering results to the console.
#' @export
print.okm <- function(x, ...) {
  cat("OKM clustering with", length(x$size), "clusters of sizes:",
      paste(x$size, collapse = ", "), "\n")
  cat("\nCluster centers:\n")
  print(x$centers, ...)
  cat("\nClustering matrix:\n")
  print(x$cluster, ...)
  cat("\nTotal within-cluster sum of squares:\n")
  print(x$tot.withinss, ...)
  cat("\nOverlapping measure:\n")
  print(x$overlaps, ...)
  cat("Available components:\n")
  print(names(x))
  invisible(x)
}
