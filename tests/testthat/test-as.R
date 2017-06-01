library(COveR)

context("as R object")

M1 <- matrix(1:12, 3, 4)
M2 <- matrix(1:4, 1, 4)

V1 <- 1:2
V2 <- 1:4

IM1 <- structure(list(inter = array(1:12, dim = c(3, 2, 2), dimnames = list(NULL,
  c("min", "max"), NULL)), class = vector()), class = "interval")
IM2 <- structure(list(inter = array(1:4, dim = c(1, 2, 2), dimnames = list(NULL,
  c("min", "max"), NULL)), class = vector()), class = "interval")
IV1 <- structure(list(inter = array(1:2, dim = c(1, 2, 1), dimnames = list(NULL,
  c("min", "max"), NULL)), class = vector()), class = "interval")
IV2 <- structure(list(inter = array(1:4, dim = c(1, 2, 2), dimnames = list(NULL,
  c("min", "max"), NULL)), class = vector()), class = "interval")

# -- TESTS ---------------------------------------------------------------------

test_that("as.data.frame", {
})

test_that("as.array", {
})

test_that("as.matrix", {
  expect_equal(M1, as.matrix(IM1))
  expect_equal(M2, as.matrix(IM2))
})

test_that("as.numeric", {
  expect_equal(V1, as.vector(IV1))
  expect_equal(V2, as.vector(IV2))
})
