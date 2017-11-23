#' Interval data test
#'
#' Test if its argument is a (strict) interval.
#' @param x an R object.
#'
#' @export
#'
#' @examples
#' is.interval(inter_city)
is.interval <- function(x) {
  class(x) == "interval" && length(dim(x$inter)) == 3 && dim(x$inter)[2] == 2
}

#' Interval data print
#'
#' Print override for interval
#' @param x an interval.
#' @param ... Other options from print.
#'
#' @export
#'
#' @examples
#' print(inter_city)
print.interval <- function(x, ...) {
  print(x$inter, ...)
  cat("Available components:\n")
  print(names(x), ...)
  invisible(x)
}

#' Interval loading.
#'
#' Load a 3D interval array from csv.
#' @param row.names Row have column names ?
#' @param class The column of class (empty if not).
#' @param ... Other options from read.
#'
#' @importFrom utils read.csv
#'
#' @export
read.interval <- function(..., row.names = FALSE, class = NULL) {
  args <- list(...)

  if (!is.logical(row.names))
    stop("row.names must be logical")

  frame <- read.csv(...)  # Load csv

  if (row.names) {
    names <- frame[, 1]
    frame <- frame[, -1]
  } else {
    names <- NULL
  }

  if (!is.null(class)) {
    if (!is.numeric(class))
      stop("class must be numeric")
    classes <- as.vector(frame[, class])
    frame <- frame[, -class]
  } else {
    classes <- vector()
  }

  # Stop if is not valid interval csv
  if ((ncol(frame)%%2) != 0)
    stop("Not a valid interval csv")

  colnames <- NULL
  if (!is.null(args$header) && args$header)
    colnames <- unique(substr(colnames(frame), 5, 100))
  names <- list(names, c("min", "max"), colnames)
  d <- as.numeric(as.vector(as.matrix(frame)))
  # Create 3D array (persons * 2 * intervals)
  data <- array(d, dim = c(nrow(frame), 2, ncol(frame)/2), dimnames = names)

  # Warning if data contain unvalid interval
  if (any(data[, 1, ] > data[, 2, ]))
    warning("Data contain unvalid interval")

  data <- structure(list(inter = data, class = classes), class = "interval")

  if (!is.interval(data))
    stop("Internal error, function not return a valid interval")

  data
}

#' Interval saving.
#'
#' Save 3D interval array to csv to open later with iload.
#'
#' @param x An interval.
#' @param class Write the class column at the end ?.
#' @param ... Other options from write.
#'
#' @importFrom utils write.csv
#'
#' @export
write.interval <- function(x, ..., class = FALSE) {
  if (!is.interval(x))
    stop("x must be interval")

  if (!is.logical(class))
    stop("class must be logical")

  frame <- as.data.frame(x)

  if (class) {
    if (length(class) != nrow(frame)) {
      warnings("The length of interval class must be equal to number of row")
    } else {
      frame <- cbind(frame, class = x$class)
    }
  }

  write.csv(frame, ...)
}

#' Interval plotting.
#'
#' plot for interval.
#'
#' @param x An interval.
#' @param ... :
#'   - 'col' Colors of rectangles.
#'   - 'add' Add to exsiting plot or not.
#'   - Other options from plot.
#'
#' @importFrom graphics plot rect
#'
#' @export
#'
#' @examples
#' plot(iaggregate(iris, 5))
#' plot(iaggregate(iris, 5), col=4)
#' plot(iaggregate(iris, 5), add=TRUE)
plot.interval <- function(x, ...) {
  if (!is.interval(x))
    stop("x must be interval")

  args <- list(...)
  dx <- matrix(c(x$inter[, , 1]), ncol = 2)
  dy <- matrix(c(1, 2), ncol = 2)

  if (dim(x$inter)[3] > 1) {
    dy <- matrix(c(x$inter[, , 2]), ncol = 2)
  }

  # Draw plot
  if (is.null(args$add) || !args$add)
    plot(c(min(dx), max(dx)), c(min(dy), max(dy)), type = "n", ...)

  # Draw interval rect
  rect(dx[, 1], dy[, 1], dx[, 2], dy[, 2], lwd = 2, border = args$col)
}

#' Interval aggregation.
#'
#' Aggregate data to 3D interval array.
#'
#' @param data Some data to aggregate.
#' @param col A number (the column index to aggregate on).
#'
#' @importFrom stats aggregate rnorm
#'
#' @export
#'
#' @examples
#' iaggregate(iris, col=5)
#' iaggregate(rock, col=4)
#' iaggregate(cars, col=1)
iaggregate <- function(data, col = 1) {
  if (col <= 0 || col > ncol(data))
    stop("col must be between 1 and number of column")

  d <- aggregate(x = data[, -col], by = list(data[, col]), FUN = function(v) {
    c(min(as.numeric(v), na.rm = TRUE), max(as.numeric(v), na.rm = TRUE))
  })

  classes <- d[, 1]
  v <- as.vector(as.matrix(d[, -1]))
  names <- list(classes, c("min", "max"), colnames(data)[-col])
  dim <- c(length(classes), 2, ncol(data) - 1)
  inter <- array(v, dim = dim, dimnames = names)

  inter <- as.interval(inter)

  if (!is.interval(inter))
    stop("Internal error, function not return a valid interval")

  inter
}

#' Interval binding.
#'
#' Bind intervals.
#'
#' @param ... All intervals to bind.
#' @param class set to true to create class by bind interval, false bind class attribute.
#'
#' @export
#'
#' @examples
#' ibind(iaggregate(iris,5), iaggregate(iris,5))
#' ibind(iaggregate(iris,5), iaggregate(iris,5), iaggregate(iris,5), class=TRUE)
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
  if (!is.null(row_names))
    dimnames(data$inter)[[1]] <- row_names
  if (!is.null(dim_names))
    dimnames(data$inter)[[3]] <- dim_names
  data$class <- classes

  if (!is.interval(data))
    stop("Internal error, function not return a valid interval")

  data
}

#' Interval generate.
#'
#' Generate intervals from normal law.
#'
#' @param n the number of elements to generate.
#' @param ... Vectors (center mean, center sd, half size mean, half size sd).
#'
#' @export
#'
#' @examples
#' igenerate(1, c(0,1,2,1))
#' igenerate(1, c(0,1,2,1), c(100,1,2,1))
igenerate <- function(n, ...) {
  gen <- list(...)
  data <- NULL

  if (length(gen))
    for (i in 1:length(gen)) {
      x <- rnorm(n, gen[[i]][1], gen[[i]][2])
      y <- rnorm(n, gen[[i]][3], gen[[i]][4])

      data <- cbind(data, x - y)
      data <- cbind(data, x + y)
    }

  as.interval(data)
}

# TODO: naming

#' Visualisation of overlap by seuil.
#'
#' Generate plot of overlap by seuil, used to help to defined the best seuil .
#'
#' @param x A matrix of membership degree.
#' @param min The minimum value of degree.
#' @param max The maximum value of degree.
#' @param step The step of degree.
#'
#' @importFrom graphics plot
#'
#' @export
measure <- function(x, min = 0, max = 1, step = 0.1) {
  if (!is.matrix(x))
    stop("x must be a numeric matrix")

  t <- seq(min, max, step)
  overlap <- sapply(t, function(y) sum(x >= y)/nrow(x))

  plot(t, overlap, type = "l")
}

#' Degree matrix to logical
#'
#' Transform a membership degree matrix into logical matrix by threshold.
#'
#' @param x A matrix of membership degree.
#' @param t The threshold to assign over.
#'
#' @export
degree2logical <- function(x, t = min(apply(x, 1, max))) {
  x >= t
}
