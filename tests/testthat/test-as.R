library(COveR)

context("as R object")

m1 <- matrix(1:12, 3, 4)
m2 <- matrix(1:4, 1, 4)

v1 <- 1:2
v2 <- 1:4

im1 <- structure(list(
  inter = array(
    1:12,
    dim = c(3, 2, 2),
    dimnames = list(NULL, c("min", "max"), NULL)
  ), class = vector()
), class = "interval")
im2 <- structure(list(
  inter = array(
    1:4,
    dim = c(1, 2, 2),
    dimnames = list(NULL, c("min", "max"), NULL)
  ), class = vector()
), class = "interval")
iv1 <- structure(list(
  inter = array(
    1:2,
    dim = c(1, 2, 1),
    dimnames = list(NULL, c("min", "max"), NULL)
  ), class = vector()
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

test_that("as.data.frame", {
})

test_that("as.array", {
})

test_that("as.matrix", {
  expect_equal(m1, as.matrix(im1))
  expect_equal(m2, as.matrix(im2))
})

test_that("as.numeric", {
  expect_equal(v1, as.vector(iv1))
  expect_equal(v2, as.vector(iv2))
})
