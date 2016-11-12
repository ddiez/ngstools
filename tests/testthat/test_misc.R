f <- system.file("files/example.bam", package = "ngstools")
tmp <- readGAlignmentPairs(f)
fs <- fsize(tmp)

context("Fragment size")
test_that("Compute fragment size", {
  expect_true(class(tmp) == "GAlignmentPairs")
  expect_type(fs, "integer")
  expect_true(is.na(fs[1]))
  expect_identical(fs[2], 104L)
})
