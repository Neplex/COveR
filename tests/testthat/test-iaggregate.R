library(COveR)

context("iaggregate")

v <- c(
  4.3, 4.9, 4.9, 5.8, 7, 7.9, 2.3, 2, 2.2, 4.4, 3.4, 3.8,
  1.0, 3, 4.5, 1.9, 5.1, 6.9, 0.1, 1, 1.4, 0.6, 1.8, 2.5
)

i <- structure(list(
  inter = array(
    v,
    dim = c(3, 2, 4),
    dimnames = list(
      c("setosa", "versicolor", "virginica"),
      c("min", "max"),
      c(
        "Sepal.Length", "Sepal.Width",
        "Petal.Length", "Petal.Width"
      )
    )
  ), class = vector()
), class = "interval")

# -- TESTS ---------------------------------------------------------------------

test_that("iaggregate", {
  expect_equal(i, iaggregate(iris, 5))
})

test_that("iaggregate error", {
  expect_error(iaggregate(iris, -1))
  expect_error(iaggregate(iris, 10))
  expect_error(iaggregate(1:2))
})
