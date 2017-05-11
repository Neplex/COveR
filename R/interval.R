require(methods)

.onUnload <- function(libpath) {
    library.dynam.unload("icluster", libpath)
}

#' Interval data check
#' @desc Check if object is a good interval data object
#' @param x an R object.
is.interval <- function(x) {
    class(x) == "interval" && length(dim(x$inter)) == 3 && dim(x$inter)[2] == 2 && 
        is.vector(x$class)
}

# Convert interval to R type
as.vector.interval <- function(x, ...) as.vector(x$inter, ...)
as.matrix.interval <- function(x, ...) {
    matrix(as.vector(x$inter, ...), ncol = dim(x$inter)[3] * 2)
}
as.array.interval <- function(x, ...) as.array(x$inter, ...)
as.data.frame.interval <- function(x, ...) as.data.frame(x$inter, ...)

# Convert R type to interval
as.interval.default <- function(x) NULL
as.interval.interval <- function(x) x
as.interval.numeric <- function(x) {
    names <- names <- list(NULL, c("min", "max"), names(x))
    d <- array(x, dim = list(1, 2, length(x)/2), dimnames = names)
    return(structure(list(inter = d, class = vector()), class = "interval"))
}
as.interval.matrix <- function(x) {
    names <- names <- list(row.names(x), c("min", "max"), NULL)
    d <- array(x, dim = list(nrow(x), 2, ncol(x)/2), dimnames = names)
    return(structure(list(inter = d, class = vector()), class = "interval"))
}
as.interval.array <- function(x) {
    if (is.array(x) && length(dim(x)) == 3 && dim(x)[2] == 2) {
        return(structure(list(inter = x, class = vector()), class = "interval"))
    } else {
        return(as.interval(as.matrix(x)))
    }
}

#' Interval data convertor
#' @desc Convert data to interval
#' @param x an R object.
as.interval <- function(x) UseMethod("as.interval")

#' Interval data print
#' @desc Print override for interval
#' @param x an interval.
#' @param ... Other options from print.
print.interval <- function(x, ...) {
    print(x$inter, ...)
    cat("Available components:\n")
    print(names(x), ...)
    invisible(x)
}

#' Interval loading.
#' @desc Load a 3D interval array from csv.
#' @param row.names Row have column names ?
#' @param class The column of class (empty if not).
#' @param ... Other options from read.
read.interval <- function(..., row.names = FALSE, class) {
    if (!is.logical(row.names)) 
        stop("row.names must be logical")
    
    frame <- read.csv(...)  # Load csv
    
    if (row.names) {
        names <- frame[, 1]
        frame <- frame[, -1]
    } else {
        names <- NULL
    }
    
    if (hasArg(class)) {
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
    
    colnames <- unique(substr(colnames(frame), 5, 100))
    names <- list(names, c("min", "max"), colnames)
    d <- as.vector(as.matrix(frame))
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
#' @desc Save 3D interval array to csv to open later with iload.
#'
#' @param x An interval.
#' @param class Write the class column at the end ?.
#' @param ... Other options from write.
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
#' @desc plot for interval.
#'
#' @param X An object (array, number, vector, matrix, ...).
#' @param Y An object (array, number, vector, matrix, ...).
#' @param col Colors of rectangles.
#' @param type Type of plot (only rectangle: r).
#' @param add Add to exsiting plot or not.
#' @param ... Other options from plot.
#'
#' @examples
#' plot(iaggregate(iris, col=5))
#' plot.interval(1)
#' plot.interval(c(1,3,2,4))
#' plot.interval(c(1,3,2,4), c(1,2,4,3))
plot.interval <- function(X, Y, col = 1, type = "r", add = FALSE, ...) {
    y <- matrix(c(1, 2), ncol = 2)
    
    if (is.interval(X)) {
        x <- matrix(c(X$inter[, , 1]), ncol = 2)
        
        if (dim(X$inter)[3] > 1) 
            y <- matrix(c(X$inter[, , 2]), ncol = 2)
        
    } else if (is.numeric(X)) {
        # number, vector or matrix
        x <- matrix(X, ncol = 2)
        
        if (hasArg(Y)) 
            y <- matrix(Y, ncol = 2)
        
    } else {
        stop("X and Y must be Interval or numeric value")
    }
    
    # Draw plot
    if (!add) 
        plot(c(min(x), max(x)), c(min(y), max(y)), type = "n", ...)
    
    # Draw interval rect
    rect(x[, 1], y[, 1], x[, 2], y[, 2], lwd = 2, border = col)
}

#' Interval aggregation.
#' @desc Aggregate data to 3D interval array.
#'
#' @param data Some data to aggregate.
#' @param col A number (the column index to aggregate on).
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
#' @desc Bind intervals.
#'
#' @param ... All intervals to bind.
#' @param class set to true to create class by bind interval, false bind class attribute.
#'
#' @examples
#' ibind(iaggregate(iris,5), iaggregate(iris,5))
#' ibind(iaggregate(iris,5), iaggregate(iris,5), iaggregate(iris,5), class=TRUE)
ibind <- function(..., class = FALSE) {
    inters <- list(...)
    inter <- as.matrix(inters[[1]])
    if (class) {
        classes <- rep(1, nrow(inter))
    } else {
        classes <- inters[[1]]$class
    }
    
    for (i in 2:length(inters)) {
        it <- as.matrix(inters[[i]])
        inter <- rbind(inter, it)
        if (class) {
            classes <- c(classes, rep(i, nrow(it)))
        } else {
            classes <- c(classes, inters[[i]]$class)
        }
    }
    
    data <- as.interval(inter)
    data$class <- classes
    
    if (!is.interval(data)) 
        stop("Internal error, function not return a valid interval")
    
    data
}

#' Interval generate.
#' @desc Generate intervals from normal law.
#'
#' @param n the number of elements to generate
#' @param ... Vectors (center mean, center sd, half size mean, half size sd)
#'
#' @examples
#' igenerate(1, c(0,1,2,1))
#' igenerate(1, c(0,1,2,1), , c(100,1,2,1))
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
