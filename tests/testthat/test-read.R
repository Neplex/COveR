library(COveR)

context("read.interval")

f1 <- system.file("extdata", "test.read.1.csv", package = "COveR")
f2 <- system.file("extdata", "test.read.2.csv", package = "COveR")
f3 <- system.file("extdata", "test.read.3.csv", package = "COveR")
f4 <- system.file("extdata", "test.read.4.csv", package = "COveR")
f5 <- system.file("extdata", "test.read.5.csv", package = "COveR")
f6 <- system.file("extdata", "test.read.6.csv", package = "COveR")
f7 <- system.file("extdata", "test.read.7.csv", package = "COveR")

d <- as.interval(matrix(
  c(0, 50, 0, 50, 100, 200, 100, 200, 100, 200),
  nrow = 5,
  byrow = TRUE
))
h <- c("temp")
r <- c("Paris", "Orleans", "Lille", "Strasbourg", "Marseille")
c <- c("N", "N", "N", "E", "S")

i1 <- d
i2 <- d
dimnames(i2$inter)[[3]] <- h
i3 <- d
dimnames(i3$inter)[[1]] <- r
i4 <- i2
dimnames(i4$inter)[[1]] <- r
i5 <- i4
i5$class <- c

# -- TESTS ---------------------------------------------------------------------

test_that("read.interval", {
  expect_equal(i1, read.interval(f1, row.names = FALSE, header = FALSE))
  expect_equal(i2, read.interval(f2, row.names = FALSE, header = TRUE))
  expect_equal(i3, read.interval(f3, row.names = TRUE, header = FALSE))
  expect_equal(i4, read.interval(f4, row.names = TRUE, header = TRUE))
  expect_equal(i5, read.interval(f5,
    row.names = TRUE, header = TRUE,
    class = 3
  ))
})

test_that("read.interval warnings", {
  expect_warning(read.interval(f6, row.names = FALSE, header = FALSE))
})

test_that("read.interval errors", {
  expect_error(read.interval(f7, row.names = FALSE, header = FALSE))
})
