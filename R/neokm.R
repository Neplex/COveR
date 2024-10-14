#' NEOKM Clustering
#'
#' Cluster data using the NEOKM algorithm.
#' @useDynLib COveR, .registration = TRUE
#' @param x A data matrix.
#' @param centers The number of clusters or pre-initialized centers.
#' @param alpha A number representing overlap.
#' @param beta A number representing non-exhaustiveness.
#' @param nstart Number of executions to find the best result.
#' @param trace Logical, if TRUE, trace progress of the algorithm.
#' @param iter.max Maximum number of iterations allowed.
#' @return A list representing the clustering results.
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

#' Print Method for NEOKM Clustering
#'
#' Print method for displaying results of NEOKM clustering.
#' @param x A NEOKM object.
#' @param ... Additional arguments passed to the print method.
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
