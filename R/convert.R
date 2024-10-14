# Unload the package when the library is detached
.onUnload <- function(libpath) {
  library.dynam.unload("COveR", libpath)
}

# Interval to R object =========================================================

#' Convert Interval to Vector
#'
#' @param x An interval object.
#' @param ... Additional arguments to be passed to or from methods.
#' @return A vector representation of the interval.
#' @export
#' @examples
#' as.vector(inter_city)
as.vector.interval <- function(x, ...) {
  as.vector(x$inter, ...)
}

#' Convert Interval to Matrix
#'
#' @param x An interval object.
#' @param ... Additional arguments to be passed to or from methods.
#' @return A matrix representation of the interval.
#' @export
#' @examples
#' as.matrix(inter_city)
as.matrix.interval <- function(x, ...) {
  matrix(as.vector(x$inter, ...), ncol = dim(x$inter)[3] * 2)
}

#' Convert Interval to Array
#'
#' @param x An interval object.
#' @param ... Additional arguments to be passed to or from methods.
#' @return An array representation of the interval.
#' @export
#' @examples
#' as.array(inter_city)
as.array.interval <- function(x, ...) {
  as.array(x$inter, ...)
}

#' Convert Interval to Data Frame
#'
#' @param x An interval object.
#' @param ... Additional arguments to be passed to or from methods.
#' @return A data frame representation of the interval.
#' @export
#' @examples
#' as.data.frame(inter_city)
as.data.frame.interval <- function(x, ...) {
  as.data.frame(x$inter, ...)
}

# R object to interval =========================================================

#' Default Method for Interval Conversion
#'
#' @param x An object to be converted.
#' @return `NULL` as default behavior for non-supported types.
#' @export
as.interval.default <- function(x) {
  NULL
}

#' Identity Conversion for Interval
#'
#' @param x An interval object.
#' @return The interval object itself.
#' @export
as.interval.interval <- function(x) {
  x
}

#' Convert Numeric to Interval
#'
#' @param x A numeric vector.
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

#' Convert Matrix to Interval
#'
#' @param x A matrix object.
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

#' Convert Array to Interval
#'
#' @param x An array object.
#' @return An interval object constructed from the array or attempts conversion.
#' @export
as.interval.array <- function(x) {
  if (is.array(x) &&
        length(dim(x)) == 3 &&
        dim(x)[2] == 2) {
    structure(list(inter = x, class = vector()), class = "interval")
  } else {
    as.interval(as.matrix(x))
  }
}

#' Interval Data Converter
#'
#' Generic function to convert different data types to interval.
#' @param x An R object.
#' @export
as.interval <- function(x) {  # nolint object_name_linter
  UseMethod("as.interval")
}
