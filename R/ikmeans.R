#' Interval kmeans clustering.
#'
#' Culster interval data with kmeans algorithm.
#' @useDynLib COveR, .registration = TRUE
#'
#' @param x An 3D interval array.
#' @param centers A number or interval, number of cluster for clustering or pre init centers.
#' @param nstart A number, number of execution to find the best result.
#' @param distance A string ('euclid': Euclidian distance, 'hausdorff': Hausdorff distance).
#' @param trace A boolean, tracing information on the progress of the algorithm is produced.
#' @param iter.max the maximum number of iterations allowed.
#'
#' @export
#'
#' @examples
#' ikmeans(iaggregate(iris, col=5), 2)
#' ikmeans(iaggregate(iris, col=5), iaggregate(iris, col=5))
ikmeans <- function(x, centers, nstart = 10, distance = "euclid", trace = FALSE,
  iter.max = 20) {
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

  if (!is.logical(trace))
    stop("trace must be logical")

  if (!is.numeric(iter.max))
    stop("iter.max must be numeric")
  if (iter.max <= 0)
    stop("iter.max must be positive")

  # Distance
  if (distance == "euclid") {
    dist <- 0
  } else if (distance == "hausdorff") {
    dist <- 1
  } else {
    stop(paste("Unknow distance:", distance, ". Try 'euclid' or 'hausdorff'"))
  }

  # Call
  d <- dim(x$inter)
  n <- dimnames(x$inter)
  v <- as.numeric(as.vector(x$inter))
  c <- .Call("_ikmeans", v, d[1], d[2], d[3], nc, nstart, dist, trace, iter.max,
    c)

  # Naming
  dimnames(c[[2]]) <- list(1:nc, n[[2]], n[[3]])

  # Remove empty cluster
  centers <- c[[2]][!rowSums(!is.finite(c[[2]])), , ]

  # Recreate 3D array in case of 1 cluster
  if (dim(centers)[1] == 1 && length(dim(centers)) < 3)
    centers <- array(as.vector(centers), dim = list(1, 2, d[3]))

  cluster <- c[[1]]
  centers <- as.interval(centers)
  totss <- c[[3]]
  wss <- c[[4]]
  totwss <- c[[5]]
  bss <- totss - totwss
  size <- as.vector(table(cluster))
  iter <- c[[6]]

  # Result
  structure(list(cluster = cluster, centers = centers, totss = totss, withinss = wss,
    tot.withinss = totwss, betweenss = bss, size = size, iter = iter), class = "ikmeans")
}

#' Ikmeans print
#'
#' Print override for ikmeans
#'
#' @param x An IKmeans object.
#' @param ... Other options from print.
#'
#' @export
print.ikmeans <- function(x, ...) {
  cat("Ikmeans clustering with ", length(x$size), " clusters of sizes ", paste(x$size,
    collapse = ", "), "\n", sep = "")
  cat("\nCluster means:\n")
  print(x$centers, ...)
  cat("\nClustering vector:\n")
  print(x$cluster, ...)
  cat("\nWithin cluster sum of squares by cluster:\n")
  print(x$withinss, ...)
  cat(sprintf(" (between_SS / total_SS = %5.1f %%)\n", 100 * x$betweenss/x$totss),
    "Available components:\n", sep = "\n")
  print(names(x))
  invisible(x)
}
