#' Interval K-Means Clustering
#'
#' Cluster interval data using the k-means algorithm.
#' @useDynLib COveR, .registration = TRUE
#' @param x A 3D interval array.
#' @param centers Either the number of clusters or pre-initialized centers.
#' @param nstart Number of executions to find the best result.
#' @param distance Distance metric ('euclid' or 'hausdorff').
#' @param trace Logical, if TRUE, trace progress of the algorithm.
#' @param iter.max Maximum number of iterations allowed.
#' @return A list representing the clustering results.
#' @export
#' @examples
#' ikmeans(iaggregate(iris, col = 5), 2)
#' ikmeans(iaggregate(iris, col = 5), iaggregate(iris, col = 5))
ikmeans <- function(  # nolint cyclocomp_linter
  x, centers,
  nstart = 10,
  distance = "euclid",
  trace = FALSE,
  iter.max = 20  # nolint object_name_linter
) {

  # Check input validity
  stopifnot(
    "Data must be interval" = is.interval(x),
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

  # Call the underlying C function for k-means clustering
  d <- dim(x$inter)
  n <- dimnames(x$inter)
  v <- as.numeric(as.vector(x$inter))
  c <- .Call(
    "_ikmeans", v, d[1], d[2], d[3],
    nc, nstart, dist, trace, iter.max, c
  )

  # Naming
  dimnames(c[[2]]) <- list(1:nc, n[[2]], n[[3]])

  # Remove empty cluster
  centers <- c[[2]][!rowSums(!is.finite(c[[2]])), , ]

  # Ensure 3D array format if there is only one cluster
  if (dim(centers)[1] == 1 && length(dim(centers)) < 3) {
    centers <- array(as.vector(centers), dim = list(1, 2, d[3]))
  }

  cluster <- c[[1]]
  centers <- as.interval(centers)
  totss <- c[[3]]
  wss <- c[[4]]
  totwss <- c[[5]]
  bss <- totss - totwss
  size <- as.vector(table(cluster))
  iter <- c[[6]]

  # Return the clustering results as a structured list
  structure(list(
    cluster = cluster,
    centers = centers,
    totss = totss,
    withinss = wss,
    tot.withinss = totwss,
    betweenss = bss,
    size = size,
    iter = iter
  ), class = "ikmeans")
}

#' Print Method for Ikmeans Clustering
#'
#' Print method for displaying results of ikmeans clustering.
#' @param x An ikmeans object.
#' @param ... Additional arguments passed to the print method.
#' @export
print.ikmeans <- function(x, ...) {
  cat("Ikmeans clustering with", length(x$size), "clusters of sizes:",
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
