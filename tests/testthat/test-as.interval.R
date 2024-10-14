library(COveR)

context("as.interval")

m1 <- matrix(1:12, 3, 4)
m2 <- matrix(1:4, 1, 4)

v1 <- 1:2
v2 <- 1:4

ii1 <- iaggregate(iris, 5)
ii2 <- iaggregate(rock, 4)
im1 <- structure(list(
  inter = array(
    1:12,
    dim = c(3, 2, 2),
    dimnames = list(NULL, c("min", "max"), NULL)
  ),
  class = vector()
), class = "interval")
im2 <- structure(list(
  inter = array(
    1:4,
    dim = c(1, 2, 2),
    dimnames = list(NULL, c("min", "max"), NULL)
  ),
  class = vector()
), class = "interval")
iv1 <- structure(list(
  inter = array(
    1:2,
    dim = c(1, 2, 1),
    dimnames = list(NULL, c("min", "max"), NULL)
  ),
  class = vector()
), class = "interval")
iv2 <- structure(list(
  inter = array(
    1:4,
    dim = c(1, 2, 2),
    dimnames = list(NULL, c("min", "max"), NULL)
  ),
  class = vector()
), class = "interval")

# -- TESTS ---------------------------------------------------------------------

test_that("as.interval interval", {
  expect_equal(ii1, as.interval(ii1))
  expect_equal(ii2, as.interval(ii2))
})

test_that("as.interval array", {
})

test_that("as.interval matrix", {
  expect_equal(im1, as.interval(m1))
  expect_equal(im2, as.interval(m2))
})

test_that("as.interval numeric", {
  expect_equal(iv1, as.interval(v1))
  expect_equal(iv2, as.interval(v2))
})

test_that("as.interval default", {
  expect_equal(NULL, as.interval("NULL"))
  expect_equal(NULL, as.interval(NULL))
  expect_equal(NULL, as.interval(NA))
})
