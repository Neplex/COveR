#' Clusters interval data using the OKM (Overlapping K-means) algorithm.
#'
#' @param x A 3D interval array representing the data to be clustered.
#' @param centers Either the number of clusters to create or a set of
#' pre-initialized cluster centers. If a number is provided, it indicates how
#' many clusters to create.
#' @param nstart The number of times to run the OKM algorithm with different
#' starting values to find the best result (default is 10).
#' @param distance A string specifying the distance metric to use: 'euclid' for
#' Euclidean distance or 'hausdorff' for Hausdorff distance (default is
#' 'euclid').
#' @param algorithm A string specifying the algorithm type to use: 'std' for the
#' standard algorithm or 'matrix' for matrix-based algorithm (default is 'std').
#' @param update A string specifying the update method for cluster centers.
#' Either: 'mean', 'sum', 'join' or 'meet' (default is 'mean').
#' @param trace Logical value indicating whether to show progress of the
#' algorithm (default is `FALSE`).
#' @param iter.max Maximum number of iterations allowed for the OKM algorithm
#' (default is 20).
#' @param secure Logical value indicating whether to ensure that the minimum is
#' less than or equal to the maximum in intervals (default is `FALSE`).
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
#' iokm(iaggregate(iris, col = 5), 2)
#' iokm(iaggregate(iris, col = 5), iaggregate(iris, col = 5))
iokm <- function(  # nolint cyclocomp_linter
  x, centers,
  nstart = 10,
  distance = "euclid",
  algorithm = "std",
  update = "mean",
  trace = FALSE,
  iter.max = 20,  # nolint object_name_linter
  secure = FALSE
) {

  # Check input validity
  stopifnot(
    "Data must be interval" = is.interval(x),
    "'nstart' must be > 0" = is.numeric(nstart) && nstart > 0,
    "'trace' must be logical" = is.logical(trace),
    "'iter.max' must be > 0" = is.numeric(iter.max) && iter.max > 0,
    "'secure' must be logical" = is.logical(secure)
  )

  # Set the distance measure
  dist <- switch(
    distance,
    "euclid" = 0,
    "hausdorff" = 1,
    stop("Unknown distance type. Use 'euclid' or 'hausdorff'.")
  )

  # Set the algorithm type
  algo <- switch(
    algorithm,
    "std" = 0,
    "matrix" = 1,
    stop("Unknown algorithm type. Use 'std' or 'matrix'.")
  )

  # Set the update type
  up <- switch(
    update,
    "mean" = 0,
    "sum" = 1,
    "join" = 2,
    "meet" = 3,
    stop("Unknown update type. Use 'mean', 'sum', 'join' or 'meet'.")
  )

  # Handle centers input
  if (is.numeric(centers)) {
    if (centers > 0 && centers <= nrow(x$inter)) {
      nc <- centers
      c <- NULL
    } else {
      stop("The number of clusters must be between 1 and the number of rows.")
    }
  } else if (is.interval(centers) || is.matrix(centers) ||
               is.vector(centers) || is.array(centers)) {
    centers <- as.interval(centers)
    if (dim(centers$inter)[3] != dim(x$inter)[3]) {
      stop("'x' and 'centers' must have the same number of intervals.")
    }
    nc <- dim(centers$inter)[1]
    c <- as.numeric(as.vector(centers$inter))
  } else {
    stop("'centers' must be a number, interval, vector, or matrix.")
  }

  # Call the underlying C function for OKM clustering
  d <- dim(x$inter)
  n <- dimnames(x$inter)
  v <- as.numeric(as.vector(x$inter))
  c <- .Call(
    "_iokm", v, d[1], d[2], d[3],
    nc, nstart, dist, algo, up, trace, iter.max, secure, c
  )

  # Naming
  colnames(c[[1]]) <- 1:nc
  row.names(c[[1]]) <- row.names(x)
  dimnames(c[[2]]) <- list(1:nc, n[[2]], n[[3]])

  # Remove empty clusters
  cluster <- data.matrix(c[[1]])
  cluster <- cluster[, colSums(cluster) != 0]
  centers <- c[[2]][!rowSums(!is.finite(c[[2]])), , ]

  # Ensure 3D array format if there is only one cluster
  if (dim(centers)[1] == 1 && length(dim(centers)) < 3) {
    cluster <- matrix(as.vector(cluster), ncol = 1)
    centers <- array(centers, dim = list(1, 2, d[3]))
  }

  centers <- as.interval(centers)
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
  ), class = "iokm")
}

#' Displays the results of IOKM clustering in a user-friendly format.
#'
#' @param x An `iokm` object resulting from the `iokm` function.
#' @param ... Additional arguments passed to print().
#' @return No return value, it prints the clustering results to the console.
#' @export
print.iokm <- function(x, ...) {
  cat("IOKM clustering with", length(x$size), "clusters of sizes:",
      paste(x$size, collapse = ", "), "\n")
  cat("\nCluster centers:\n")
  print(x$centers, ...)
  cat("\nClustering matrix:\n")
  print(x$cluster, ...)
  cat("\nWithin-cluster sum of squares by elements:\n")
  print(x$withinss, ...)
  cat(sprintf(" (Between_SS / Total_SS = %5.1f%%)\n",
              100 * x$betweenss / x$totss))
  cat("Available components:\n")
  print(names(x))
  invisible(x)
}
