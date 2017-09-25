#' Interval okm clustering.
#'
#' Culster interval data with okm algorithm.
#' @useDynLib COveR, .registration = TRUE
#'
#' @param x An 3D interval array.
#' @param centers A number or interval, number of cluster for clustering or pre init centers.
#' @param nstart A number, number of execution to find the best result.
#' @param distance A string ('euclid': Euclidian distance, 'hausdorff': Hausdorff distance).
#' @param algorithm A string ('std': Standard algorithm, 'matrix': Matrix algorithm).
#' @param update A string ('mean': Mean center, 'sum': Sum center, 'join': Union center, 'meet': Intersect center).
#' @param trace A boolean, tracing information on the progress of the algorithm is produced.
#' @param iter.max the maximum number of iterations allowed.
#' @param secure A boolean (secure interval or not : min <= max).
#'
#' @export
#'
#' @examples
#' iokm(iaggregate(iris, col=5), 2)
#' iokm(iaggregate(iris, col=5), iaggregate(iris, col=5))
iokm <- function(x, centers, nstart = 10, distance = "euclid", algorithm = "std",
  update = "mean", trace = FALSE, iter.max = 20, secure = FALSE) {

  nc <- 0
  c <- NULL

  # Arguments check
  if (!is.interval(x))
    stop("Data must be interval")

  if (is.double(centers)) {
    if (centers > 0 && centers <= nrow(x$inter)) {
      nc <- centers
    } else stop("The number of clusters must be between 1 and number of row")

  } else if (is.interval(centers) || is.matrix(centers) || is.vector(centers) ||
    is.array(centers)) {
    centers <- as.interval(centers)
    d <- dim(centers$inter)
    nc <- d[1]
    c <- as.numeric(as.vector(centers$inter))
    if (d[3] != dim(x$inter)[3])
      stop("x and centers must have the same number of intervals")

  } else stop("centers must be double, interval, vector or matrix")

  if (!is.numeric(nstart))
    stop("nstart must be numeric")
  if (nstart <= 0)
    stop("nstart must be positive")

  if (!is.character(distance))
    stop("distance must be character")

  if (!is.character(algorithm))
    stop("algorithm must be character")

  if (!is.character(update))
    stop("update must be character")

  if (!is.logical(trace))
    stop("trace must be logical")

  if (!is.numeric(iter.max))
    stop("iter.max must be numeric")
  if (iter.max <= 0)
    stop("iter.max must be positive")

  if (!is.logical(secure))
    stop("secure must be logical")


  # Distance
  dist <- match(distance, c("euclid", "hausdorff")) - 1
  # Algorithm
  algo <- match(algorithm, c("std", "matrix")) - 1
  # Update
  up <- match(update, c("mean", "sum", "join", "meet")) - 1

  # Call
  d <- dim(x$inter)
  n <- dimnames(x$inter)
  v <- as.numeric(as.vector(x$inter))
  c <- .Call("_iokm", v, d[1], d[2], d[3], nc, nstart, dist, algo, up, trace, iter.max,
    secure, c)


  # Naming
  colnames(c[[1]]) <- 1:nc
  row.names(c[[1]]) <- row.names(x)
  dimnames(c[[2]]) <- list(1:nc, n[[2]], n[[3]])


  # Remove empty cluster
  cluster <- data.matrix(c[[1]])
  cluster <- cluster[, colSums(cluster) != 0]
  centers <- c[[2]][!rowSums(!is.finite(c[[2]])), , ]

  # Recreate 3D array in case of 1 cluster
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

  # Result
  structure(list(cluster = cluster, centers = centers, totss = totss, withinss = wss,
    tot.withinss = totwss, betweenss = bss, size = size, iter = iter, overlaps = over),
    class = "iokm")
}

#' IOKM print
#'
#' Print override for IOKM
#'
#' @param x An IOKM object.
#' @param ... Other options from print.
#'
#' @export
print.iokm <- function(x, ...) {
  cat("IOKM clustering with ", length(x$size), " clusters of sizes ", paste(x$size,
    collapse = ", "), "\n", sep = "")
  cat("\nCluster means:\n")
  print(x$centers, ...)
  cat("\nClustering matrix:\n")
  print(x$cluster, ...)
  cat("\nWithin cluster sum of squares by elements:\n")
  print(x$withinss, ...)
  cat(sprintf(" (between_SS / total_SS = %5.1f %%)\n", 100 * x$betweenss/x$totss),
    "Available components:\n", sep = "\n")
  print(names(x))
  invisible(x)
}
