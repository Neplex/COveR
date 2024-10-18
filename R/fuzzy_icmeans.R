#' Performs fuzzy c-means clustering on interval data, allowing for soft
#' clustering of data points into multiple clusters.
#'
#' @param x A 3D interval array representing the data to be clustered.
#' @param centers Either the number of clusters or a set of pre-initialized
#' cluster centers. If a number is provided, it specifies how many clusters to
#' create.
#' @param m A number greater than 1 that controls the degree of fuzziness in the
#' clustering process (default is 2).
#' @param nstart Number of times to run the clustering algorithm with different
#' starting values to find the best solution (default is 2).
#' @param distance A string specifying the distance metric to use, either
#' 'euclid' for Euclidean distance or 'hausdorff' for Hausdorff distance
#' (default is 'euclid').
#' @param trace Logical, if `TRUE`, tracing information on the progress of the
#' algorithm is displayed (default is `FALSE`).
#' @param iter.max Maximum number of iterations allowed for the clustering
#' algorithm (default is 40).
#' @return A list of clustering results, including:
#'   - `cluster`: The membership matrix indicating the degree of belonging of
#'                each data point to each cluster.
#'   - `centers`: The final cluster centers.
#'   - `totss`: Total sum of squares.
#'   - `withinss`: Within-cluster sum of squares by cluster.
#'   - `tot.withinss`: Total within-cluster sum of squares.
#'   - `betweenss`: Between-cluster sum of squares.
#'   - `size`: Sizes of each cluster.
#'   - `iter`: Number of iterations run by the algorithm.
#'   - `overlaps`: The average overlap among clusters.
#' @useDynLib COveR, .registration = TRUE
#' @export
#' @examples
#' fuzzy_icmeans(iaggregate(iris, col = 5), 2)
#' fuzzy_icmeans(iaggregate(iris, col = 5), iaggregate(iris, col = 5))
fuzzy_icmeans <- function(  # nolint cyclocomp_linter
  x, centers,
  m = 2,
  nstart = 2,
  distance = "euclid",
  trace = FALSE,
  iter.max = 40  # nolint object_name_linter
) {
  # Check input validity
  stopifnot(
    "Data must be interval" = is.interval(x),
    "'m' must be numeric > 1" = is.numeric(m) && m > 1,
    "'nstart' must be > 0" = is.numeric(nstart) && nstart > 0,
    "'trace' must be logical" = is.logical(trace),
    "'iter.max' must be > 0" = is.numeric(iter.max) && iter.max > 0
  )

  # Set the distance measure
  dist <- switch(
    distance,
    "euclid" = 0,
    "hausdorff" = 1,
    stop("Unknown distance type. Use 'euclid' or 'hausdorff'.")
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

  # Call the underlying C function for fuzzy clustering
  d <- dim(x$inter)
  n <- dimnames(x$inter)
  v <- as.numeric(as.vector(x$inter))
  c <- .Call(
    "_icmeans", v, d[1], d[2], d[3],
    nc, m, nstart, dist, trace, iter.max, c
  )

  # Naming
  dimnames(c[[2]]) <- list(1:nc, n[[2]], n[[3]])

  # Remove empty cluster
  centers <- c[[2]][!rowSums(!is.finite(c[[2]])), , ]

  # Ensure 3D array format if there is only one cluster
  if (dim(centers)[1] == 1 && length(dim(centers)) < 3) {
    centers <- array(as.vector(centers), dim = list(1, 2, d[3]))
  }

  cluster <- round(c[[1]], 2)
  centers <- as.interval(centers)
  totss <- c[[3]]
  wss <- c[[4]]
  totwss <- c[[5]]
  bss <- totss - totwss
  size <- colSums(cluster != 0)
  iter <- c[[6]]
  overlaps <- mean(rowSums(cluster != 0))

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
    overlaps = overlaps
  ), class = "icmeans")
}

#' Displays the results of fuzzy icmeans clustering in a readable format.
#'
#' @param x An `icmeans` object resulting from the `fuzzy_icmeans` function.
#' @param ... Additional arguments passed to print().
#' @return No return value, it prints the clustering results to the console.
#' @export
print.icmeans <- function(x, ...) {
  cat("Fuzzy Icmeans clustering with", length(x$size), "clusters of sizes:",
      paste(x$size, collapse = ", "), "\n")
  cat("\nCluster centers:\n")
  print(x$centers, ...)
  cat("\nClustering vector:\n")
  print(x$cluster, ...)
  cat("\nWithin-cluster sum of squares by cluster:\n")
  print(x$withinss, ...)
  cat(sprintf(" (Between_SS / Total_SS = %5.1f%%)\n",
              100 * x$betweenss / x$totss))
  cat("Available components:\n")
  print(names(x))
  invisible(x)
}
