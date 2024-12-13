% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ineokm.R
\name{ineokm}
\alias{ineokm}
\title{Performs clustering on interval data using the Neo-KM algorithm, which allows
for overlapping and non-exhaustive cluster membership.}
\usage{
ineokm(
  x,
  centers,
  alpha = 0.3,
  beta = 0.05,
  nstart = 10,
  trace = FALSE,
  iter.max = 20
)
}
\arguments{
\item{x}{A 3D interval array representing the data to be clustered.}

\item{centers}{Either the number of clusters to create or a set of
pre-initialized cluster centers. If a number is provided, it specifies how
many clusters to create.}

\item{alpha}{A numeric value that controls the degree of overlap between
clusters (default is 0.3).}

\item{beta}{A numeric value that controls the non-exhaustiveness of clusters
(default is 0.05).}

\item{nstart}{The number of times to run the Neo-KM algorithm with different
starting values in order to find the best solution (default is 10).}

\item{trace}{Logical value indicating whether to show the progress of the
algorithm (default is `FALSE`).}

\item{iter.max}{Maximum number of iterations allowed for the Neo-KM algorithm
(default is 20).}
}
\value{
A list of clustering results, including:
  - `cluster`: A vector indicating the cluster assignment of each data point.
  - `centers`: The final cluster centers.
  - `totss`: Total sum of squares.
  - `withinss`: Within-cluster sum of squares by cluster.
  - `tot.withinss`: Total within-cluster sum of squares.
  - `betweenss`: Between-cluster sum of squares.
  - `size`: The number of points in each cluster.
  - `iter`: Number of iterations the algorithm executed.
}
\description{
Performs clustering on interval data using the Neo-KM algorithm, which allows
for overlapping and non-exhaustive cluster membership.
}
\examples{
ineokm(iaggregate(iris, col = 5), 3)
ineokm(iaggregate(iris, col = 5), iaggregate(iris, col = 5), 1, 2)
}
