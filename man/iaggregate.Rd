% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/interval.R
\name{iaggregate}
\alias{iaggregate}
\title{Aggregates data into a 3D interval array based on a specified column.}
\usage{
iaggregate(data, col = 1)
}
\arguments{
\item{data}{The data frame to aggregate.}

\item{col}{The index of the column to aggregate by.}
}
\value{
A structured interval object representing the aggregated data.
}
\description{
Aggregates data into a 3D interval array based on a specified column.
}
\examples{
iaggregate(iris, col = 5)
iaggregate(rock, col = 4)
iaggregate(cars, col = 1)
}
