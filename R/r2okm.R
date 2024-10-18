#' Cluster data using the R2-OKM algorithm.
#'
#' @param x A numeric data matrix or data frame containing the data to be
#' clustered.
#' @param centers Either a positive integer specifying the number of clusters
#' to create or a matrix of initial cluster centers.
#' @param lambda A numeric parameter that controls the clustering behavior,
#' influencing the shape and separation of clusters (default is 0).
#' @param nstart Number of random initializations to find the best clustering
#' result (default is 10).
#' @param trace Logical value indicating whether to display progress information
#' during execution (default is `FALSE`).
#' @param iter.max Maximum number of iterations allowed for the clustering
#' algorithm (default is 20).
#' @return A list containing the clustering results, which includes:
#'   - `cluster`: Matrix indicating the cluster assignments for each data point.
#'   - `centers`: The final cluster centers.
#'   - `totss`: Total sum of squares.
#'   - `withinss`: Within-cluster sum of squares for each cluster.
#'   - `tot.withinss`: Total within-cluster sum of squares.
#'   - `betweenss`: Between-cluster sum of squares.
#'   - `size`: Number of data points in each cluster.
#'   - `iter`: Number of iterations performed.
#'   - `overlaps`: Average number of clusters that each point overlaps with.
#' @useDynLib COveR, .registration = TRUE
#' @export
#' @examples
#' r2okm(iris[, -5], 3)
#' r2okm(iris[, -5], 3, lambda = 0.3)
#' r2okm(iris[, -5], iris[, -5], lambda = 1)
r2okm <- function(  # nolint cyclocomp_linter
  x, centers,
  lambda = 0,
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
    "'lambda' must be non-negative" = is.numeric(lambda) && lambda >= 0,
    "'nstart' must be a positive integer" = is.numeric(nstart) && nstart > 0,
    "'trace' must be logical" = is.logical(trace),
    "'iter.max' must be positive" = is.numeric(iter.max) && iter.max > 0
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

  # Call to the underlying C function for R2OKM clustering
  v <- as.numeric(unlist(x))
  clustering_result <- .Call(
    "_r2okm", v, nrow(x), ncol(x),
    nc, lambda, nstart, trace, iter.max, c
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
  ), class = "r2okm")
}

#' Displays the results of R2-OKM clustering in a readable format.
#'
#' @param x An R2-OKM object resulting from the `r2okm` function.
#' @param ... Additional arguments passed to print().
#' @return No return value, it prints the clustering results to the console.
#' @export
print.r2okm <- function(x, ...) {
  cat("R2OKM clustering with", length(x$size), "clusters of sizes:",
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
