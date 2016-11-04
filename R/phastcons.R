#' processPhastCons
#'
#' Process a BigWig file with phastConst scores.
#'
#' @param object a BigWig file object.
#'
#' @return an RleList with the scores by chromosome.
#' @export
#'
#' @examples
#' NULL
processPhastCons <- function(object) {
  org <- organism(object)

  seqstyle <- GenomeInfoDb:::.guessSpeciesStyle(seqlevels(object))
  seqstyle <- seqstyle$style[seqstyle$species == sub(" ", "_", org)]

  sname <- seqlevels(object)
  stdname <- genomeStyles(species = org)[[seqstyle]]
  sname <- sname[sname %in% stdname]

  pcrlelist <- RleList(compress = FALSE)
  for (s in sname) {
    pcrlelist[[s]] <- readRDS(file.path(object@data_dirpath, sprintf("%s.%s.rds", object@data_pkgname, s)))
  }
  pcrlelist
}

#' getPhastconsScores
#'
#' Return phastCons scores.
#'
#' @param db the database of phastCons scores.
#' @param chr the chromosome.
#' @param start the starting point.
#' @param end the end point.
#'
#' @return an integer vector of phastCons scores.
#' @export
#'
#' @examples
#' NULL
getPhastconsScores <- function(db, chr, start, end) {
  tmp <- db[[chr]][start:end]
  rep(as.integer(tmp@values), tmp@lengths)/10 # prob. was x10 and rounded.
}
