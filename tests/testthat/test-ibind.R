library(COveR)

context("ibind")

v <- c(4.3, 4.9, 4.9, 4.3, 4.9, 4.9, 5.8, 7, 7.9, 5.8, 7, 7.9, 2.3, 2, 2.2, 2.3,
  2, 2.2, 4.4, 3.4, 3.8, 4.4, 3.4, 3.8, 1, 3, 4.5, 1, 3, 4.5, 1.9, 5.1, 6.9, 1.9,
  5.1, 6.9, 0.1, 1, 1.4, 0.1, 1, 1.4, 0.6, 1.8, 2.5, 0.6, 1.8, 2.5)
a <- array(v, dim = c(6, 2, 4), dimnames = list(c("setosa", "versicolor", "virginica",
  "setosa", "versicolor", "virginica"), c("min", "max"), c("Sepal.Length", "Sepal.Width",
  "Petal.Length", "Petal.Width")))
i <- structure(list(inter = a, class = vector()), class = "interval")
j <- structure(list(inter = a, class = c(1, 1, 1, 2, 2, 2)), class = "interval")
iiris <- iaggregate(iris, 5)

# -- TESTS ---------------------------------------------------------------------

test_that("ibind", {
  expect_equal(iiris, ibind(iiris))
  expect_equal(i, ibind(iiris, iiris))
  expect_equal(j, ibind(iiris, iiris, class = TRUE))
})
