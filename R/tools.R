#' Clustering color for plotting.
#'
#' Get color from clustering cluster for data plotting.
#'
#' @param x A cluster.
#'
#' @importFrom grDevices col2rgb rainbow rgb
#' @importFrom stats model.matrix
#'
#' @export
#'
#' @examples
#' plot(iris[,1:2], col=cluster_color(neokm(iris, 2, .2, .05)$cluster))
cluster_color <- function(x) {
  nb_elem <- ifelse(is.matrix(x), nrow(x), length(x))
  if (!is.matrix(x))
    x <- matrix(as.logical(model.matrix(~factor(x) - 1)), nrow = nb_elem)
  x <- t(apply(x, 1, function(x) x/sum(x)))  # normalize
  x[is.na(x)] <- 0  # remove NA (no class -> black color)
  c <- col2rgb(rainbow(ncol(x)))
  apply(x, 1, function(x) {
    r <- sum(c[1, ] * x)
    v <- sum(c[2, ] * x)
    b <- sum(c[3, ] * x)
    rgb(r, v, b, maxColorValue = 255)
  })
}
