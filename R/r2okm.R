#' r2okm clustering.
#'
#' Culster data with r2okm algorithm.
#' @useDynLib COveR, .registration = TRUE
#'
#' @param x An data matrix.
#' @param centers A number, number of cluster for clustering or pre init centers.
#' @param lambda A number.
#' @param nstart A number, number of execution to find the best result.
#' @param trace A boolean, tracing information on the progress of the algorithm is produced.
#' @param iter.max the maximum number of iterations allowed.
#'
#' @export
#'
#' @examples
#' r2okm(iris[,-5], 3)
#' r2okm(iris[,-5], 3, .3)
#' r2okm(iris[,-5], iris[,-5], 1)
r2okm <- function(x, centers, lambda = 0, nstart = 10, trace = FALSE, iter.max = 20) {

  nc <- 0
  c <- NULL

  # Arguments check
  if (!is.data.frame(x) && !is.matrix(x) && !is.numeric(x))
    stop("Data must be numeric matrix")

  if (length(centers) == 1) {
    if (centers > 0 && centers <= nrow(x)) {
      nc <- centers
    } else {
      stop("The number of clusters must be between 1 and number of row")
    }

  } else if (is.numeric(centers) || is.data.frame(centers) || is.matrix(centers) ||
    is.vector(centers)) {
    centers <- as.matrix(data.matrix(centers))
    nc <- nrow(centers)
    c <- as.numeric(as.vector(centers))
    print(c)
    if (ncol(centers) != ncol(x))
      stop("x and centers must have the same number of dimensions")

  } else stop("centers must be double, vector or matrix")

  if (!is.numeric(lambda))
    stop("lambda must be numeric")
  if (lambda < 0)
    stop("lambda must be positive or null")

  if (!is.numeric(nstart))
    stop("nstart must be numeric")
  if (nstart <= 0)
    stop("nstart must be positive")

  if (!is.logical(trace))
    stop("trace must be logical")

  if (!is.numeric(iter.max))
    stop("iter.max must be numeric")
  if (iter.max <= 0)
    stop("iter.max must be positive")


  # Call
  v <- as.numeric(as.vector(data.matrix(x)))
  c <- .Call("_r2okm", v, nrow(x), ncol(x), nc, lambda, nstart, trace, iter.max,
    c)


  cluster <- data.matrix(c[[1]])
  centers <- data.matrix(c[[2]])
  totss <- c[[3]]
  wss <- c[[4]]
  totwss <- c[[5]]
  bss <- totss - totwss
  size <- colSums(cluster)
  iter <- c[[6]]
  over <- mean(rowSums(cluster))

  # Result
  structure(list(cluster = cluster, centers = centers, totss = totss, withinss = wss,
    tot.withinss = totwss, betweenss = bss, size = size, iter = iter, overlaps = over),
    class = "r2okm")
}

#' R2-OKM print
#'
#' Print override for R2-OKM
#'
#' @param x An NEOKM object.
#' @param ... Other options from print.
#'
#' @export
print.r2okm <- function(x, ...) {
  cat("R2OKM clustering with ", length(x$size), " clusters of sizes ", paste(x$size,
    collapse = ", "), "\n", sep = "")
  cat("\nCluster means:\n")
  print(x$centers, ...)
  cat("\nClustering matrix:\n")
  print(x$cluster, ...)
  cat("\nWithin cluster sum of squares by cluster:\n")
  print(x$withinss, ...)
  cat(sprintf(" (between_SS / total_SS = %5.1f %%)\n", 100 * x$betweenss/x$totss),
    "Available components:\n", sep = "\n")
  print(names(x))
  invisible(x)
}
