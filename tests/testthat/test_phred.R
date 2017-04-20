context("Phred score")

f <- system.file("files/example_1.fastq.gz", package = "ngstools")
d <- get_phred(f)

test_that("result is correct", {
  expect_is(d, "data.frame")
  expect_true(nrow(d) == 1L)
  expect_identical(d[, "phred"], "phred33")
})
