library(testthat)
library(vartest)

test_that("adjusted.taha.test works", {
  result <- adjusted.taha.test(Sepal.Length ~ Species, data = iris)
  expect_s3_class(result, "vht")
  expect_true(is.numeric(result$p.value))
  expect_true(result$p.value >= 0 && result$p.value <= 1)
  expect_true(is.numeric(result$statistic))
  expect_s3_class(result$data, "data.frame")
  expect_true(inherits(result$formula, "formula"))
})

test_that("bartletts.test works", {
  result <- bartletts.test(Sepal.Length ~ Species, data = iris)
  expect_s3_class(result, "vht")
  expect_true(is.numeric(result$p.value))
  expect_true(result$p.value >= 0 && result$p.value <= 1)
  expect_true(is.numeric(result$statistic))
  expect_s3_class(result$data, "data.frame")
  expect_true(inherits(result$formula, "formula"))
})

test_that("ansari.test works", {
  result <- ansari.test(Sepal.Length ~ Species, data = iris)
  expect_s3_class(result, "vht")
  expect_true(is.numeric(result$p.value))
  expect_true(result$p.value >= 0 && result$p.value <= 1)
  expect_true(is.numeric(result$statistic))
  expect_s3_class(result$data, "data.frame")
  expect_true(inherits(result$formula, "formula"))
})

test_that("capon.test works", {
  result <- capon.test(Sepal.Length ~ Species, data = iris)
  expect_s3_class(result, "vht")
  expect_true(is.numeric(result$p.value))
  expect_true(result$p.value >= 0 && result$p.value <= 1)
  expect_true(is.numeric(result$statistic))
  expect_s3_class(result$data, "data.frame")
  expect_true(inherits(result$formula, "formula"))
})

test_that("david.barton.test works", {
  result <- david.barton.test(Sepal.Length ~ Species, data = iris)
  expect_s3_class(result, "vht")
  expect_true(is.numeric(result$p.value))
  expect_true(result$p.value >= 0 && result$p.value <= 1)
  expect_true(is.numeric(result$statistic))
  expect_s3_class(result$data, "data.frame")
  expect_true(inherits(result$formula, "formula"))
})

test_that("duran.test works", {
  result <- duran.test(Sepal.Length ~ Species, data = iris)
  expect_s3_class(result, "vht")
  expect_true(is.numeric(result$p.value))
  expect_true(result$p.value >= 0 && result$p.value <= 1)
  expect_true(is.numeric(result$statistic))
  expect_s3_class(result$data, "data.frame")
  expect_true(inherits(result$formula, "formula"))
})

test_that("adjusted.taha.test works", {
  result <- adjusted.taha.test(Sepal.Length ~ Species, data = iris)
  expect_s3_class(result, "vht")
  expect_true(is.numeric(result$p.value))
  expect_true(result$p.value >= 0 && result$p.value <= 1)
  expect_true(is.numeric(result$statistic))
  expect_s3_class(result$data, "data.frame")
  expect_true(inherits(result$formula, "formula"))
})

test_that("fk.test works", {
  result <- fk.test(Sepal.Length ~ Species, data = iris)
  expect_s3_class(result, "vht")
  expect_true(is.numeric(result$p.value))
  expect_true(result$p.value >= 0 && result$p.value <= 1)
  expect_true(is.numeric(result$statistic))
  expect_s3_class(result$data, "data.frame")
  expect_true(inherits(result$formula, "formula"))
})

test_that("klotz.test works", {
  result <- klotz.test(Sepal.Length ~ Species, data = iris)
  expect_s3_class(result, "vht")
  expect_true(is.numeric(result$p.value))
  expect_true(result$p.value >= 0 && result$p.value <= 1)
  expect_true(is.numeric(result$statistic))
  expect_s3_class(result$data, "data.frame")
  expect_true(inherits(result$formula, "formula"))
})

test_that("mood.test works", {
  result <- mood.test(Sepal.Length ~ Species, data = iris)
  expect_s3_class(result, "vht")
  expect_true(is.numeric(result$p.value))
  expect_true(result$p.value >= 0 && result$p.value <= 1)
  expect_true(is.numeric(result$statistic))
  expect_s3_class(result$data, "data.frame")
  expect_true(inherits(result$formula, "formula"))
})

test_that("siegel.tukey.test works", {
  result <- siegel.tukey.test(Sepal.Length ~ Species, data = iris)
  expect_s3_class(result, "vht")
  expect_true(is.numeric(result$p.value))
  expect_true(result$p.value >= 0 && result$p.value <= 1)
  expect_true(is.numeric(result$statistic))
  expect_s3_class(result$data, "data.frame")
  expect_true(inherits(result$formula, "formula"))
})

test_that("taha.test works", {
  result <- taha.test(Sepal.Length ~ Species, data = iris)
  expect_s3_class(result, "vht")
  expect_true(is.numeric(result$p.value))
  expect_true(result$p.value >= 0 && result$p.value <= 1)
  expect_true(is.numeric(result$statistic))
  expect_s3_class(result$data, "data.frame")
  expect_true(inherits(result$formula, "formula"))
})

test_that("talwar.gentle.test works", {
  result <- talwar.gentle.test(Sepal.Length ~ Species, data = iris)
  expect_s3_class(result, "vht")
  expect_true(is.numeric(result$p.value))
  expect_true(result$p.value >= 0 && result$p.value <= 1)
  expect_true(is.numeric(result$statistic))
  expect_s3_class(result$data, "data.frame")
  expect_true(inherits(result$formula, "formula"))
})

test_that("cochrans.test works", {
  result <- cochrans.test(Sepal.Length ~ Species, data = iris)
  expect_s3_class(result, "vht")
  expect_true(is.numeric(result$p.value))
  expect_true(result$p.value >= 0 && result$p.value <= 1)
  expect_true(is.numeric(result$statistic))
  expect_s3_class(result$data, "data.frame")
  expect_true(inherits(result$formula, "formula"))
})

test_that("mzv.test works", {
  result <- mzv.test(Sepal.Length ~ Species, data = iris)
  expect_s3_class(result, "vht")
  expect_true(is.numeric(result$p.value))
  expect_true(result$p.value >= 0 && result$p.value <= 1)
  expect_true(is.numeric(result$statistic))
  expect_s3_class(result$data, "data.frame")
  expect_true(inherits(result$formula, "formula"))
})

test_that("f.test works", {
  result <- f.test(Sepal.Length ~ Species, data = iris)
  expect_s3_class(result, "vht")
  expect_true(is.numeric(result$p.value))
  expect_true(result$p.value >= 0 && result$p.value <= 1)
  expect_true(is.numeric(result$statistic))
  expect_s3_class(result$data, "data.frame")
  expect_true(inherits(result$formula, "formula"))
})

test_that("g.test works", {
  result <- g.test(Sepal.Length ~ Species, data = iris)
  expect_s3_class(result, "vht")
  expect_true(is.numeric(result$p.value))
  expect_true(result$p.value >= 0 && result$p.value <= 1)
  expect_true(is.numeric(result$statistic))
  expect_s3_class(result$data, "data.frame")
  expect_true(inherits(result$formula, "formula"))
})


test_that("hartley.test works", {
  result <- hartley.test(Sepal.Length ~ Species, data = iris)
  expect_s3_class(result, "vht")
  expect_true(is.numeric(result$p.value))
  expect_true(result$p.value >= 0 && result$p.value <= 1)
  expect_true(is.numeric(result$statistic))
  expect_s3_class(result$data, "data.frame")
  expect_true(inherits(result$formula, "formula"))
})

test_that("levene.test works", {
  result <- levene.test(Sepal.Length ~ Species, data = iris)
  expect_s3_class(result, "vht")
  expect_true(is.numeric(result$p.value))
  expect_true(result$p.value >= 0 && result$p.value <= 1)
  expect_true(is.numeric(result$statistic))
  expect_s3_class(result$data, "data.frame")
  expect_true(inherits(result$formula, "formula"))
})

test_that("obrien.test works", {
  result <- obrien.test(Sepal.Length ~ Species, data = iris)
  expect_s3_class(result, "vht")
  expect_true(is.numeric(result$p.value))
  expect_true(result$p.value >= 0 && result$p.value <= 1)
  expect_true(is.numeric(result$statistic))
  expect_s3_class(result$data, "data.frame")
  expect_true(inherits(result$formula, "formula"))
})

test_that("zv.test works", {
  result <- zv.test(Sepal.Length ~ Species, data = iris)
  expect_s3_class(result, "vht")
  expect_true(is.numeric(result$p.value))
  expect_true(result$p.value >= 0 && result$p.value <= 1)
  expect_true(is.numeric(result$statistic))
  expect_s3_class(result$data, "data.frame")
  expect_true(inherits(result$formula, "formula"))
})