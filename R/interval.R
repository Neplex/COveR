#' Determines if an object is a strictly valid interval object.
#'
#' @param x An R object to be tested.
#' @return A logical value indicating whether the object is a valid interval.
#' @export
#' @examples
#' is.interval(inter_city)
#' is.interval(1:4)
is.interval <- function(x) {  # nolint object_name_linter
  inherits(x, "interval") &&
    is.array(x$inter) &&
    length(dim(x$inter)) == 3 &&
    dim(x$inter)[2] == 2
}

#' Custom print method for displaying interval objects in a readable format.
#'
#' @param x An interval object to be printed.
#' @param ... Additional arguments passed to the underlying print() function.
#' @return No return value, it prints the interval to the console.
#' @export
#' @examples
#' print(inter_city)
print.interval <- function(x, ...) {
  print(x$inter, ...)
  cat("Available components:\n")
  print(names(x), ...)
  invisible(x)
}

#' Reads a CSV file and converts the data into a 3D interval array.
#'
#' @param row.names Logical indicating if the first column contains row names.
#' @param class The column index of class labels (set to `NULL` if not present).
#' @param ... Additional arguments passed to read.csv().
#' @return A structured interval object representing the data from the CSV file.
#' @importFrom utils read.csv
#' @export
read.interval <- function(  # nolint object_name_linter
  ...,
  row.names = FALSE,  # nolint object_name_linter
  class = NULL
) {
  args <- list(...)
  stopifnot(is.logical(row.names))

  frame <- read.csv(...)  # Load CSV

  if (row.names) {
    row_names <- frame[, 1]
    frame <- frame[, -1]
  } else {
    row_names <- NULL
  }

  if (!is.null(class)) {
    stopifnot(is.numeric(class))
    classes <- frame[, class]
    frame <- frame[, -class]
  } else {
    classes <- vector()
  }

  stopifnot("Not a valid interval CSV format" = ncol(frame) %% 2 == 0)

  if (!is.null(args$header) && args$header) {
    col_names <- unique(substr(colnames(frame), 5, 100))
  } else {
    col_names <- NULL
  }

  data <- array(
    as.numeric(unlist(frame)),
    dim = c(nrow(frame), 2, ncol(frame) / 2),
    dimnames = list(row_names, c("min", "max"), col_names)
  )

  # Warning if data contain unvalid interval
  if (any(data[, 1, ] > data[, 2, ])) {
    warning("Data contains invalid intervals")
  }

  structure(list(inter = data, class = classes), class = "interval")
}

#' Writes an interval object to a CSV file.
#'
#' @param x An interval object to be saved.
#' @param class Logical indicating whether to add the class column in the CSV.
#' @param ... Additional arguments passed to write.csv().
#' @return No return value, it saves the interval to the given CSV file.
#' @importFrom utils write.csv
#' @export
write.interval <- function(x, ..., class = FALSE) {  # nolint object_name_linter
  stopifnot(
    "x must be an interval object" = is.interval(x),
    "class must be logical" = is.logical(class)
  )

  frame <- as.data.frame(x$inter)
  if (class && length(x$class) == nrow(frame)) {
    frame <- cbind(frame, class = x$class)
  }

  write.csv(frame, ...)
}

#' Generates a visual representation of interval data as rectangles on a plot.
#'
#' @param x An interval object to be plotted.
#' @param ... Additional graphical parameters such as `col` and `add`.
#' @return No return value, it plot the interval.
#' @importFrom graphics plot rect
#' @export
#' @examples
#' plot(iaggregate(iris, 5))
#' plot(iaggregate(iris, 5), col = 4)
#' plot(iaggregate(iris, 5), add = TRUE)
plot.interval <- function(x, ...) {
  stopifnot(is.interval(x))

  args <- list(...)
  dx <- x$inter[, , 1]
  dy <- if (dim(x$inter)[3] > 1) x$inter[, , 2] else matrix(c(1, 2), ncol = 2)

  if (is.null(args$add) || !args$add) {
    plot(range(dx), range(dy), type = "n", ...)
  }

  rect(dx[, 1], dy[, 1], dx[, 2], dy[, 2], lwd = 2, border = args$col)
}

#' Aggregates data into a 3D interval array based on a specified column.
#'
#' @param data The data frame to aggregate.
#' @param col The index of the column to aggregate by.
#' @return A structured interval object representing the aggregated data.
#' @importFrom stats aggregate
#' @export
#' @examples
#' iaggregate(iris, col = 5)
#' iaggregate(rock, col = 4)
#' iaggregate(cars, col = 1)
iaggregate <- function(data, col = 1) {
  if (col <= 0 || col > ncol(data)) {
    stop("col must be between 1 and number of column")
  }

  aggregated <- aggregate(
    x = data[, -col],
    by = list(data[, col]),
    FUN = function(v) range(v, na.rm = TRUE)
  )

  classes <- aggregated[, 1]
  as.interval(array(
    unlist(aggregated[, -1]),
    dim = c(length(classes), 2, ncol(data) - 1),
    dimnames = list(classes, c("min", "max"), colnames(data)[-col])
  ))
}

#' Combines multiple interval objects into a single interval object.
#'
#' @param ... Interval objects to bind together.
#' @param class Logical value indicating whether to assign a new class label to
#' each interval object when binding. If `TRUE`, each set of intervals will have
#' a distinct class label.
#' @return A new interval object containing the combined intervals from the
#' input objects.
#' @export
#' @examples
#' ibind(iaggregate(iris, 5), iaggregate(iris, 5))
#' ibind(iaggregate(iris, 5), iaggregate(iris, 5), iaggregate(iris, 5),
#' class = TRUE)
ibind <- function(..., class = FALSE) {
  inters <- list(...)

  inter <- as.matrix(inters[[1]])
  row_names <- dimnames(inters[[1]]$inter)[[1]]
  dim_names <- dimnames(inters[[1]]$inter)[[3]]

  if (class) {
    classes <- rep(1, nrow(inter))
  } else {
    classes <- inters[[1]]$class
  }

  if (length(inters) > 1) {
    for (i in 2:length(inters)) {
      it <- as.matrix(inters[[i]])
      dn <- dimnames(inters[[i]]$inter)[[1]]

      inter <- rbind(inter, it)
      row_names <- c(row_names, dn)

      if (class) {
        classes <- c(classes, rep(i, nrow(it)))
      } else {
        classes <- c(classes, inters[[i]]$class)
      }
    }
  }

  data <- as.interval(inter)
  if (!is.null(row_names)) dimnames(data$inter)[[1]] <- row_names
  if (!is.null(dim_names)) dimnames(data$inter)[[3]] <- dim_names
  data$class <- classes

  data
}

#' Creates intervals from Normal Distribution using specified mean and standard
#' deviation values for both the center and half-size of the intervals.
#'
#' @param n Number of intervals to generate.
#' @param ... Vectors representing parameters for generating intervals: each
#' vector should contain four values (`center mean`, `center sd`,
#' `half-size mean`, `half-size sd`).
#' @return An interval object containing the generated intervals.
#' @importFrom stats rnorm
#' @export
#' @examples
#' igenerate(1, c(0, 1, 2, 1))
#' igenerate(1, c(0, 1, 2, 1), c(100, 1, 2, 1))
igenerate <- function(n, ...) {
  gen <- list(...)
  data <- NULL

  if (length(gen)) {
    for (i in seq_along(gen)) {
      x <- rnorm(n, gen[[i]][1], gen[[i]][2])
      y <- rnorm(n, gen[[i]][3], gen[[i]][4])

      data <- cbind(data, x - y)
      data <- cbind(data, x + y)
    }
  }

  as.interval(data)
}

#' Plots the overlap of membership degrees in a matrix as a function of a
#' threshold.
#'
#' @param x A matrix of membership degrees.
#' @param min Minimum threshold value for the plot (default is 0).
#' @param max Maximum threshold value for the plot (default is 1).
#' @param step Step size for the threshold values (default is 0.1).
#' @return No return value, it plot the overlap as a function of the threshold.
#' @importFrom graphics plot
#' @export
#' @examples
#' membership_matrix <- matrix(runif(20), nrow = 5)
#' measure(membership_matrix, min = 0, max = 1, step = 0.2)
measure <- function(x, min = 0, max = 1, step = 0.1) {
  stopifnot(is.matrix(x))

  thresholds <- seq(min, max, step)
  overlap <- sapply(thresholds, function(th) mean(x >= th))
  plot(thresholds, overlap, type = "l", xlab = "Threshold", ylab = "Overlap")
}

#' Transforms a matrix of membership degrees into a logical matrix based on a
#' specified threshold.
#'
#' @param x A matrix of membership degrees.
#' @param t Threshold value for converting the degrees to logical values. By
#' default, it uses the minimum of the maximum values in each row.
#' @return A logical matrix where each element is `TRUE` if it meets or exceeds
#' the threshold, and `FALSE` otherwise.
#' @export
#' @examples
#' degrees <- matrix(runif(9), nrow = 3)
#' degree2logical(degrees, t = 0.5)
degree2logical <- function(x, t = min(apply(x, 1, max))) {
  x >= t
}
