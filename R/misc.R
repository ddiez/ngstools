#' checkAlphabetDistribution
#'
#' Compute the distribution of letters in a Fastq file.
#'
#' @param files character vector with the name of the files to process.
#'
#' @return a data.frame with the letter distribution.
#' @export
#'
#' @examples
#' NULL
checkAlphabetDistribution <- function(files) {
  l <- lapply(files, function(f) {
    s <- readFastq(f)
    m <- alphabetByCycle(sread(s))
    m
    m <- t(t(m) / colSums(m))

    d <- melt(m)
    d$file <- f
    d
  })
  bind_rows(l)
  #  d <- d %>% mutate(read = factor(sub(".*_(.).*", "\\1", d$file)))
  #  d <- d %>% filter(alphabet %in% c("A", "C", "G", "T", "N"))
  # d
}

#' fsize
#'
#' Computes the fragment size.
#'
#' @param x GenomicAlignmentPair object
#' @param check whether to check consistency of pair's position.
#'
#' @note Adapted from Herve Pages's implementation (TODO: add link to original).
#'
#' @return an integer vector with fragment size.
#' @export
#'
#' @examples
#' NULL
fsize <- function(x, check = TRUE)
{
  lgal <- first(x)
  rgal <- last(x)
  lend <- start(lgal)
  rstart <- end(rgal)
  #ans <- qwidth(lgal) + qwidth(rgal) + rstart - lend - 1L
  ans <- rstart - lend - 1L

  ## Set to NA when the first and last ends are not in an
  ## upstream/downstream configuration:
  if (check) {
    not_upstream_downstream <- rstart <= lend
    ans[not_upstream_downstream] <- NA_integer_
  }

  ans
}
