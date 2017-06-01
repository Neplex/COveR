library(COveR)

context("as.interval")

M1 <- matrix(1:12, 3, 4)
M2 <- matrix(1:4, 1, 4)

V1 <- 1:2
V2 <- 1:4

II1 <- iaggregate(iris, 5)
II2 <- iaggregate(rock, 4)
IM1 <- structure(list(inter = array(1:12, dim = c(3, 2, 2), dimnames = list(NULL,
  c("min", "max"), NULL)), class = vector()), class = "interval")
IM2 <- structure(list(inter = array(1:4, dim = c(1, 2, 2), dimnames = list(NULL,
  c("min", "max"), NULL)), class = vector()), class = "interval")
IV1 <- structure(list(inter = array(1:2, dim = c(1, 2, 1), dimnames = list(NULL,
  c("min", "max"), NULL)), class = vector()), class = "interval")
IV2 <- structure(list(inter = array(1:4, dim = c(1, 2, 2), dimnames = list(NULL,
  c("min", "max"), NULL)), class = vector()), class = "interval")

# -- TESTS ---------------------------------------------------------------------

test_that("as.interval interval", {
  expect_equal(II1, as.interval(II1))
  expect_equal(II2, as.interval(II2))
})

test_that("as.interval array", {
})

test_that("as.interval matrix", {
  expect_equal(IM1, as.interval(M1))
  expect_equal(IM2, as.interval(M2))
})

test_that("as.interval numeric", {
  expect_equal(IV1, as.interval(V1))
  expect_equal(IV2, as.interval(V2))
})

test_that("as.interval default", {
  expect_equal(NULL, as.interval("NULL"))
  expect_equal(NULL, as.interval(NULL))
  expect_equal(NULL, as.interval(NA))
})
