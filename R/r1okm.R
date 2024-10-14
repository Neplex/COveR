#' R1-OKM Clustering
#'
#' Cluster data using the R1OKM algorithm.
#' @useDynLib COveR, .registration = TRUE
#' @param x A data matrix.
#' @param centers The number of clusters or a matrix of initial centers.
#' @param alpha A numeric parameter controlling clustering behavior.
#' @param nstart Number of executions to find the best result.
#' @param trace Logical, if TRUE, provides progress information.
#' @param iter.max Maximum number of iterations allowed.
#' @return A list representing the clustering results.
#' @export
#' @examples
#' r1okm(iris[, -5], 3)
#' r1okm(iris[, -5], 3, alpha = -0.5)
#' r1okm(iris[, -5], iris[, -5], alpha = 1)
r1okm <- function(  # nolint cyclocomp_linter
  x, centers,
  alpha = 0,
  nstart = 10,
  trace = FALSE,
  iter.max = 20  # nolint object_name_linter
) {
  nc <- 0
  c <- NULL

  # Check input validity
  stopifnot(
    "Data must be a numeric matrix or data frame." = is.data.frame(x) ||
      is.matrix(x) ||
      is.numeric(x),
    "'alpha' must be numeric" = is.numeric(alpha),
    "'nstart' must be a positive integer" = is.numeric(nstart) && nstart > 0,
    "'trace' must be logical" = is.logical(trace),
    "'iter.max' must be a positive" = is.numeric(iter.max) && iter.max > 0
  )

  x <- as.matrix(x)  # Ensure x is in matrix form

  # Handling centers input (number of clusters or matrix of initial centers)
  if (length(centers) == 1) {
    if (centers > 0 && centers <= nrow(x)) {
      nc <- centers
    } else {
      stop("The number of clusters must be between 1 and the number of rows.")
    }
  } else if (is.numeric(centers) || is.matrix(centers) ||
               is.data.frame(centers) || is.vector(centers)) {
    centers <- as.matrix(data.matrix(centers))
    nc <- nrow(centers)
    c <- as.numeric(as.vector(centers))

    if (ncol(centers) != ncol(x)) {
      stop("'x' and 'centers' must have the same number of dimensions.")
    }
  } else {
    stop("'centers' must be a number, vector, or matrix.")
  }

  # Call to the underlying C function for R1OKM clustering
  v <- as.numeric(unlist(x))
  clustering_result <- .Call(
    "_r1okm", v, nrow(x), ncol(x),
    nc, alpha, nstart, trace, iter.max, c
  )

  cluster <- data.matrix(clustering_result[[1]])
  centers <- data.matrix(clustering_result[[2]])
  totss <- clustering_result[[3]]
  wss <- clustering_result[[4]]
  totwss <- clustering_result[[5]]
  bss <- totss - totwss
  size <- colSums(cluster)
  iter <- clustering_result[[6]]
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
  ), class = "r1okm")
}

#' Print Method for R1-OKM Clustering
#'
#' Custom print method for displaying R1OKM clustering results.
#' @param x An R1OKM object.
#' @param ... Additional arguments passed to the print method.
#' @export
print.r1okm <- function(x, ...) {
  cat("R1OKM clustering with", length(x$size), "clusters of sizes:",
      paste(x$size, collapse = ", "), "\n")
  cat("Cluster centers:\n")
  print(x$centers, ...)
  cat("\nClustering matrix:\n")
  print(x$cluster, ...)
  cat("\nWithin-cluster sum of squares by cluster:\n")
  print(x$withinss, ...)
  cat(sprintf(
    "\n(between_SS / total_SS = %5.1f%%)\n",
    100 * x$betweenss / x$totss
  ))
  cat("\nAvailable components:\n")
  print(names(x))
  invisible(x)
}
