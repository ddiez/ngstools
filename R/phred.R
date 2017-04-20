#' check_phred_range
#'
#' checks the Phred encoding for the quality scores in a fastq file.
#'
#' @param filename name of fastq file to check.
#' @param nlines number of quality lines to check.
check_phred_range <- function(filename, nlines = 1000) {
  str <- readLines(filename, n = nlines * 4)
  sel <- rep(c(FALSE, FALSE, FALSE, TRUE), nlines)
  str <- str[sel]

  asc <- function(x) {
    strtoi(charToRaw(x), 16L)
  }
  asc <- Vectorize(asc, SIMPLIFY = FALSE, USE.NAMES = FALSE)

  r <- range(asc(str))
  data.frame(
    file = filename,
    start = r[1],
    end = r[2],
    start33 = r[1] - 33L,
    end33 = r[2] - 33L,
    start64 = r[1] - 64L,
    end64 = r[2] - 64L,
    stringsAsFactors = FALSE
  )
}

#' check_phred
#'
#' checks the Phred encoding for the quality scores in fastq files.
#'
#' @param filenames name of fastq file(s) to check.
#' @param nlines number of quality lines to check.
check_phred <- function(filenames, nlines = 1000) {
  tmp <- lapply(filenames, check_phred_range, nlines = nlines)
  do.call(rbind, tmp)
}

#' get_phred
#'
#' returns the phred type for a set of FASTQ files.
#'
#' @param filenames character vector with the names of the files.
#' @param nlines number of files in the FASTQ file used to guess the phred.
#'
#' @export
get_phred <- function(filenames, nlines = 1000) {
  d <- check_phred(filenames, nlines)
  p <- apply(d, 1, function(x) {
    s0 <- as.numeric(x["start"])
    s1 <- as.numeric(x["end"])
    if (s0 >= 33 & s1 <= 74)
      return("phred33")
    if (s0 >= 64 & s1 <= 105)
      return("phred64")
  })
  data.frame(phred = p, filename = filenames, stringsAsFactors = FALSE)
}

#' get_bowtie2_phred
#'
#' @param filenames character vector with the names of the files.
#' @param nlines number of files in the FASTQ file used to guess the phred.
#'
#' @export
get_bowtie2_phred <- function(filenames, nlines = 1000) {
  d <- get_phred(filenames, nlines)
  d[d[, "phred"] == "phred33", "phred"] <- "--phred33"
  d[d[, "phred"] == "phred64", "phred"] <- "--phred64"
  d
}
