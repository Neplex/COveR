# Unload the package when the library is detached
.onUnload <- function(libpath) {
  library.dynam.unload("COveR", libpath)
}

# Interval to R object =========================================================

#' Converts an interval object to its vector representation.
#'
#' @param x An interval object to be converted.
#' @param ... Additional arguments to be passed to as.vector().
#' @return A numeric vector where each consecutive pair of values represents
#' an interval.
#' @export
#' @examples
#' as.vector(inter_city)
as.vector.interval <- function(x, ...) {
  as.vector(x$inter, ...)
}

#' Converts an interval object to a matrix representation.
#'
#' Each interval is expanded into its minimum and maximum bounds.
#'
#' @param x An interval object to be converted.
#' @param ... Additional arguments to be passed to as.vector().
#' @return A matrix representation of the interval, with two columns for each
#' interval's minimum and maximum values.
#' @export
#' @examples
#' as.matrix(inter_city)
as.matrix.interval <- function(x, ...) {
  matrix(as.vector(x$inter, ...), ncol = dim(x$inter)[3] * 2)
}

#' Converts an interval object to an array representation.
#'
#' @param x An interval object to be converted.
#' @param ... Additional arguments to be passed to as.array().
#' @return An array representation of the interval.
#' @export
#' @examples
#' as.array(inter_city)
as.array.interval <- function(x, ...) {
  as.array(x$inter, ...)
}

#' Converts an interval object to a data frame representation.
#'
#' @param x An interval object to be converted.
#' @param ... dditional arguments to be passed to as.data.frame().
#' @return A data frame representation of the interval.
#' @export
#' @examples
#' as.data.frame(inter_city)
as.data.frame.interval <- function(x, ...) {
  as.data.frame(x$inter, ...)
}

# R object to interval =========================================================

#' Provides a default method for converting unsupported data types to interval.
#'
#' @param x An object that does not have a supported conversion method.
#' @return `NULL`, indicating no conversion is possible.
#' @export
as.interval.default <- function(x) {
  NULL
}

#' Identity Conversion for Interval
#'
#' @param x An interval object.
#' @return The input interval object, without modification.
#' @export
#' @examples
#' as.interval(inter_city)
as.interval.interval <- function(x) {
  x
}

#' Converts a numeric vector to an interval object.
#'
#' The length of the numeric vector must be even, representing pairs of minimum
#' and maximum values.
#'
#' @param x A numeric vector where each consecutive pair of values represents
#' an interval.
#' @return An interval object constructed from the numeric vector.
#' @export
#' @examples
#' as.interval(1:6)
as.interval.numeric <- function(x) {
  if (length(x) %% 2 != 0) {
    stop("The length of numeric vector must be even to convert to an interval.")
  }

  dim_names <- list(NULL, c("min", "max"), names(x))
  d <- array(x, dim = c(1, 2, length(x) / 2), dimnames = dim_names)

  structure(list(inter = d, class = vector()), class = "interval")
}

#' Converts a matrix to an interval object.
#'
#' The number of columns in the matrix must be even, representing pairs of
#' minimum and maximum values.
#'
#' @param x A matrix where each pair of columns represents the minimum and
#' maximum bounds of intervals.
#' @return An interval object constructed from the matrix.
#' @export
#' @examples
#' as.interval(matrix(1:12, 3, 4))
as.interval.matrix <- function(x) {
  if (ncol(x) %% 2 != 0) {
    stop("Number of columns in the matrix must be even.")
  }

  dim_names <- list(rownames(x), c("min", "max"), NULL)
  d <- array(x, dim = c(nrow(x), 2, ncol(x) / 2), dimnames = dim_names)

  structure(list(inter = d, class = vector()), class = "interval")
}

#' Converts an array to an interval object.
#'
#' The array must have three
#' dimensions, with the second dimension of size 2, representing the minimum
#' and maximum values.
#'
#' @param x An array to be converted to an interval object.
#' @return An interval object constructed from the array if it meets the
#' requirements, otherwise attempts to convert it to a matrix first.
#' @export
#' @examples
#' as.interval(array(1:12, dim = c(2, 2, 3)))
as.interval.array <- function(x) {
  if (is.array(x) &&
        length(dim(x)) == 3 &&
        dim(x)[2] == 2) {
    structure(list(inter = x, class = vector()), class = "interval")
  } else {
    as.interval(as.matrix(x))
  }
}

#' A generic function to convert various R objects into interval objects.
#'
#' @param x An R object to be converted to an interval.
#' @return An interval object constructed from the R object or NULL if the type
#' is not supported.
#' @export
as.interval <- function(x) {  # nolint object_name_linter
  UseMethod("as.interval")
}
