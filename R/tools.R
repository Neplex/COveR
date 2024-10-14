#' Clustering Color for Plotting
#'
#' Generate colors for data points based on their clustering assignments.
#'
#' @param x A clustering vector or matrix.
#' @return A vector of colors corresponding to the clustering assignments.
#' @importFrom grDevices col2rgb rainbow rgb
#' @importFrom stats model.matrix
#' @export
#' @examples
#' plot(iris[, 1:2], col = cluster_color(neokm(iris, 2, 0.2, 0.05)$cluster))
cluster_color <- function(x) {
  # Determine the number of elements in the cluster
  nb_elem <- if (is.matrix(x)) nrow(x) else length(x)

  # Convert clustering vector to matrix form if necessary
  if (!is.matrix(x)) {
    x <- matrix(as.logical(model.matrix(~ factor(x) - 1)), nrow = nb_elem)
  }

  # Generate rainbow colors for each cluster
  cluster_colors <- col2rgb(rainbow(ncol(x)))

  # Normalize matrix rows and compute the colors
  apply(x, 1, function(row) {
    row_sum <- sum(row)
    if (row_sum > 0) {
      row <- row / row_sum  # Normalize the row
    } else {
      row[] <- 0  # Handle cases where the sum is zero (no class -> black color)
    }

    # Compute the RGB values based on normalized row values
    r <- sum(cluster_colors[1, ] * row)
    g <- sum(cluster_colors[2, ] * row)
    b <- sum(cluster_colors[3, ] * row)
    rgb(r, g, b, maxColorValue = 255)
  })
}
