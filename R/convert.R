.onUnload <- function(libpath) {
  library.dynam.unload("COveR", libpath)
}

# Interval to R object =========================================================

#' Interval to vector
#' @param x an interval.
#' @param ... additional arguments to be passed to or from methods.
#'
#' @export
#'
#' @examples
#' as.vector(inter_city)
as.vector.interval <- function(x, ...) as.vector(x$inter, ...)

#' Interval to matrix
#' @param x an interval.
#' @param ... additional arguments to be passed to or from methods.
#'
#' @export
#'
#' @examples
#' as.matrix(inter_city)
as.matrix.interval <- function(x, ...) {
  matrix(as.vector(x$inter, ...), ncol = dim(x$inter)[3] * 2)
}

#' Interval to array
#' @param x an interval.
#' @param ... additional arguments to be passed to or from methods.
#'
#' @export
#'
#' @examples
#' as.array(inter_city)
as.array.interval <- function(x, ...) as.array(x$inter, ...)

#' Interval to data.frame
#' @param x an interval.
#' @param ... additional arguments to be passed to or from methods.
#'
#' @export
#'
#' @examples
#' as.data.frame(inter_city)
as.data.frame.interval <- function(x, ...) as.data.frame(x$inter, ...)


# R object to interval =========================================================

#' default to interval
#' @param x an interval.
#'
#' @export
as.interval.default <- function(x) NULL

#' interval to interval
#' @param x an interval.
#'
#' @export
as.interval.interval <- function(x) x

#' numeric to interval
#' @param x an interval.
#'
#' @export
#'
#' @examples
#' as.interval(1:6)
as.interval.numeric <- function(x) {
  names <- names <- list(NULL, c("min", "max"), names(x))
  d <- array(x, dim = list(1, 2, length(x)/2), dimnames = names)
  return(structure(list(inter = d, class = vector()), class = "interval"))
}

#' matrix to interval
#' @param x an interval.
#'
#' @export
#'
#' @examples
#' as.interval(matrix(1:12, 3, 4))
as.interval.matrix <- function(x) {
  names <- names <- list(row.names(x), c("min", "max"), NULL)
  d <- array(x, dim = list(nrow(x), 2, ncol(x)/2), dimnames = names)
  return(structure(list(inter = d, class = vector()), class = "interval"))
}

#' array to interval
#' @param x an interval.
#'
#' @export
as.interval.array <- function(x) {
  if (is.array(x) && length(dim(x)) == 3 && dim(x)[2] == 2) {
    return(structure(list(inter = x, class = vector()), class = "interval"))
  } else {
    return(as.interval(as.matrix(x)))
  }
}

#' Interval data convertor
#'
#' Convert data to interval
#' @param x an R object.
#'
#' @export
as.interval <- function(x) UseMethod("as.interval")
