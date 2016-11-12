context("Fragment size")

f <- system.file("files/example.bam", package = "ngstools")
tmp <- readGAlignmentPairs(f)
fs <- fsize(tmp)

test_that("fragment size is correct class and value", {
  expect_is(fs, "integer")
  expect_true(is.na(fs[1]))
  expect_identical(fs[2], 104L)
})
