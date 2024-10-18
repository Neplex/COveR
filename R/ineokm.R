#' Performs clustering on interval data using the Neo-KM algorithm, which allows
#' for overlapping and non-exhaustive cluster membership.
#'
#' @param x A 3D interval array representing the data to be clustered.
#' @param centers Either the number of clusters to create or a set of
#' pre-initialized cluster centers. If a number is provided, it specifies how
#' many clusters to create.
#' @param alpha A numeric value that controls the degree of overlap between
#' clusters (default is 0.3).
#' @param beta A numeric value that controls the non-exhaustiveness of clusters
#' (default is 0.05).
#' @param nstart The number of times to run the Neo-KM algorithm with different
#' starting values in order to find the best solution (default is 10).
#' @param trace Logical value indicating whether to show the progress of the
#' algorithm (default is `FALSE`).
#' @param iter.max Maximum number of iterations allowed for the Neo-KM algorithm
#' (default is 20).
#' @return A list of clustering results, including:
#'   - `cluster`: A vector indicating the cluster assignment of each data point.
#'   - `centers`: The final cluster centers.
#'   - `totss`: Total sum of squares.
#'   - `withinss`: Within-cluster sum of squares by cluster.
#'   - `tot.withinss`: Total within-cluster sum of squares.
#'   - `betweenss`: Between-cluster sum of squares.
#'   - `size`: The number of points in each cluster.
#'   - `iter`: Number of iterations the algorithm executed.
#' @useDynLib COveR, .registration = TRUE
#' @export
#' @examples
#' ineokm(iaggregate(iris, col = 5), 3)
#' ineokm(iaggregate(iris, col = 5), iaggregate(iris, col = 5), 1, 2)
ineokm <- function(  # nolint cyclocomp_linter
  x, centers,
  alpha = 0.3,
  beta = 0.05,
  nstart = 10,
  trace = FALSE,
  iter.max = 20  # nolint object_name_linter
) {

  # Check input validity
  stopifnot(
    "Data must be interval" = is.interval(x),
    "'alpha' must be numeric" = is.numeric(alpha),
    "'beta' must be numeric" = is.numeric(beta),
    "'nstart' must be > 0" = is.numeric(nstart) && nstart > 0,
    "'trace' must be logical" = is.logical(trace),
    "'iter.max' must be > 0" = is.numeric(iter.max) && iter.max > 0
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

  # Call the underlying C function for Neo-KM clustering
  d <- dim(x$inter)
  n <- dimnames(x$inter)
  v <- as.numeric(as.vector(x$inter))
  c <- .Call(
    "_ineokm", v, d[1], d[2], d[3],
    nc, alpha, beta, nstart, trace, iter.max, c
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
  ), class = "ineokm")
}

#' Displays the results of Neo-KM clustering in a user-friendly format.
#'
#' @param x An `ineokm` object resulting from the `ineokm` function.
#' @param ... Additional arguments passed to print().
#' @return No return value, it prints the clustering results to the console.
#' @export
print.ineokm <- function(x, ...) {
  cat("Ineokm clustering with", length(x$size), "clusters of sizes:",
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
