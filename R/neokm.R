#' Clusters data using the NEOKM (Non-Exhaustive Overlapping K-means) algorithm.
#'
#' @param x A numeric matrix or data frame containing the data to be clustered.
#' @param centers Either the number of clusters to create or a set of
#' pre-initialized cluster centers.
#' If a number is provided, it indicates how many clusters to create.
#' @param alpha A numeric value representing the degree of overlap allowed
#' between clusters (default is 0.3).
#' @param beta A numeric value representing non-exhaustiveness, which affects
#' the cluster formation (default is 0.05).
#' @param nstart The number of times to run the NEOKM algorithm with different
#' starting values to find the best result (default is 10).
#' @param trace Logical value indicating whether to show progress of the
#' algorithm (default is `FALSE`).
#' @param iter.max Maximum number of iterations allowed for the NEOKM algorithm
#' (default is 20).
#' @return A list of clustering results, including:
#'   - `cluster`: Matrix indicating the cluster assignment for each data point.
#'   - `centers`: The final cluster centers.
#'   - `totss`: Total sum of squares.
#'   - `withinss`: Within-cluster sum of squares by elements.
#'   - `tot.withinss`: Total within-cluster sum of squares.
#'   - `betweenss`: Between-cluster sum of squares.
#'   - `size`: The number of points in each cluster.
#'   - `iter`: The number of iterations the algorithm executed.
#'   - `overlaps`: The average overlap across clusters.
#' @useDynLib COveR, .registration = TRUE
#' @export
#' @examples
#' neokm(iris[, -5], 3)
#' neokm(iris[, -5], iris[, -5], 1, 2)
neokm <- function(  # nolint cyclocomp_linter
  x, centers,
  alpha = 0.3,
  beta = 0.05,
  nstart = 10,
  trace = FALSE,
  iter.max = 20  # nolint object_name_linter
) {

  # Check input validity
  stopifnot(
    "Data must be a numeric matrix or data frame" = is.data.frame(x) ||
      is.matrix(x) ||
      is.numeric(x),
    "'alpha' must be numeric" = is.numeric(alpha),
    "'beta' must be numeric" = is.numeric(beta),
    "'nstart' must be > 0" = is.numeric(nstart) && nstart > 0,
    "'trace' must be logical" = is.logical(trace),
    "'iter.max' must be > 0" = is.numeric(iter.max) && iter.max > 0
  )

  # Handle centers input
  if (length(centers) == 1) {
    if (centers > 0 && centers <= nrow(x)) {
      nc <- centers
      c <- NULL
    } else {
      stop("The number of clusters must be between 1 and the number of rows.")
    }
  } else if (is.numeric(centers) || is.data.frame(centers) ||
               is.matrix(centers) || is.vector(centers)) {
    centers <- as.matrix(data.matrix(centers))
    if (ncol(centers) != ncol(x)) {
      stop("'x' and 'centers' must have the same number of dimensions.")
    }
    nc <- nrow(centers)
    c <- as.numeric(as.vector(centers))
  } else {
    stop("'centers' must be a number, vector, or matrix.")
  }

  # Call the underlying C function for NEOKM clustering
  v <- as.numeric(unlist(x))
  c <- .Call(
    "_neokm", v, nrow(x), ncol(x),
    nc, alpha, beta, nstart, trace, iter.max, c
  )

  cluster <- data.matrix(c[[1]])
  centers <- data.matrix(c[[2]])
  totss <- c[[3]]
  wss <- c[[4]]
  totwss <- c[[5]]
  bss <- totss - totwss
  size <- colSums(cluster)
  iter <- c[[6]]
  over <- mean(rowSums(cluster))

  # Return the clustering results as a structured list
  structure(list(
    cluster = cluster,
    centers = centers,
    totss = totss,
    withinss = wss,
    tot.withinss = totwss,
    betweenss = bss,
    size = size,
    iter = iter,
    overlaps = over
  ), class = "neokm")
}

#' Displays the results of NEOKM clustering in a user-friendly format.
#'
#' @param x A `neokm` object resulting from the `neokm` function.
#' @param ... Additional arguments passed to print().
#' @return No return value, it prints the clustering results to the console.
#' @export
print.neokm <- function(x, ...) {
  cat("NEOKM clustering with", length(x$size), "clusters of sizes:",
      paste(x$size, collapse = ", "), "\n")
  cat("\nCluster centers:\n")
  print(x$centers, ...)
  cat("\nClustering matrix:\n")
  print(x$cluster, ...)
  cat("\nWithin-cluster sum of squares by cluster:\n")
  print(x$withinss, ...)
  cat(sprintf(" (Between_SS / Total_SS = %5.1f%%)\n",
              100 * x$betweenss / x$totss))
  cat("Available components:\n")
  print(names(x))
  invisible(x)
}
