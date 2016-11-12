context("Alphabet frequency")

f <- system.file("files/example_1.fastq.gz", package = "ngstools")
d <- checkAlphabetDistribution(f)

test_that("result is correct type and dimensions", {
  expect_is(d, "data.frame")
  expect_true(nrow(d) == 1800L)
  expect_true(ncol(d) == 4L)
})

test_that("result contains correct values", {
  expect_true(d[1, 1] == "A")
  expect_true(d[1, 2] == 1)
  expect_equal(d[1, 3], 0.090)
})
