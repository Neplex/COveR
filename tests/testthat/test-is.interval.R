library(COveR)

context("is.interval")

i1 <- structure(list(
  inter = array(
    1:12,
    dim = c(3, 2, 2),
    dimnames = list(NULL, c("min", "max"), NULL)
  ), class = vector()
), class = "interval")
i2 <- structure(list(
  inter = array(
    1:4,
    dim = c(1, 2, 2),
    dimnames = list(NULL, c("min", "max"), NULL)
  ),
  class = vector()
), class = "interval")
i3 <- structure(list(
  inter = array(
    1:2,
    dim = c(1, 2, 1),
    dimnames = list(NULL, c("min", "max"), NULL)
  ), class = vector()
), class = "interval")
i4 <- structure(list(
  inter = array(1:3, dim = c(1, 3, 1)),
  class = vector()
), class = "interval")

# -- TESTS ---------------------------------------------------------------------

test_that("interval", {
  expect_true(is.interval(i1))
  expect_true(is.interval(i2))
  expect_true(is.interval(i3))
})

test_that("no interval", {
  expect_false(is.interval(i4))
  expect_false(is.interval("interval"))
  expect_false(is.interval(NULL))
  expect_false(is.interval(NA))
  expect_false(is.interval(TRUE))
  expect_false(is.interval(FALSE))
  expect_false(is.interval(1:4))
  expect_false(is.interval(matrix(1:4)))
  expect_false(is.interval(array(1:4)))
  expect_false(is.interval(iris))
})
