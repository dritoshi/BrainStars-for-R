#' BrainStars API client for R
#' 
#' This R package can search and get gene expression data and plots
#' from BrainStars (or B*, \url{http://brainstars.org/}).
#' 
#' BrainStars is a quantitative expression database of the adult
#' mouse brain. The database has genome-wide expression profile at 51
#' adult mouse CNS regions.
#' 
#' For 51 CNS regions, slices (0.5-mm thick) of mouse brain were cut on a
#' Mouse Brain Matrix, frozen, and the specific regions were punched out
#' bilaterally with a microdissecting needle (gauge 0.5 mm) under a
#' stereomicroscope. For each region, we took samples every 4 hours,
#' starting at ZT0 (Zeitgaber time 0; the time of lights on), for 24
#' hours (6 time-point samples for each region), and we pooled the
#' samples from the different time points. We independently sampled each
#' region twice (n=2).
#' 
#' These samples were purified their RNA, and measured with Affymetrix
#' GeneChip Mouse Genome 430 2.0 arrays. Expression values were then
#' summarized with the RMA method. After several analysis with the
#' expression data, the data and analysis results were stored in the
#' BrainStars database.
#' 
#' BrainStars database has a REST API to query gene expression data and
#' some kind of figures written by Dr. Takeya Kasukawa. This package is
#' wrapper for BrainStars REST API in R. BrainStars data, images and
#' texts (excluding ABA data and images) are licensed under a Creative
#' Commons Attribution 2.1 Japan License.
#' 
#' @name brainstars
#' @docType package
#' @aliases brainstars package-brainstars
#' @references Takeya Kasukawa*, Koh-hei Masumoto*, Itoshi Nikaido*,
#' Mamoru Nagano, Kenichiro D. Uno, Kaori Tsujino, Carina Hanashima,
#' Yasufumi Shigeyoshi, and Hiroki R. Ueda: Quantitative Expression
#' Profile of Distinct Functional Regions in the Adult Mouse Brain,
#' PLoS ONE 6(8), e23228, 2011. (doi:10.1371/journal.pone.0023228)
#' *equally contributed
#'
NULL

#' Get raw expression profile by one gene
#'
#' This function get a gene expression profile from BrainStars
#'
#' @usage getBrainStarsExpression(id)
#' @param id Affymetrix GeneChip ID (Mouse Genome 430 2.0 array)
#' @keywords bio
#' @export
#' @examples
#' my.eset <- getBrainStarsExpression("1439627_at")
getBrainStarsExpression <- function(id) {
  
  base.url    <- "http://brainstars.org/probeset/"
  url.options <- "/exprvalues.csv"
  url         <- paste(base.url, id, url.options, sep="")

  expr <- read.csv(url, row.names=1)
  eset <- new("ExpressionSet", expr = as.matrix(expr) )
  return(eset)
}

#' Get barplot of gene expression profile by one gene
#'
#' This function get a barplotof gene expression profile from BrainStars
#'
#' @usage getBrainStarsFigure(id, fig.type)
#' @param id Affymetrix GeneChip ID (Mouse Genome 430 2.0 array)
#' @param fig.type Figure type
#' @keywords bio
#' @export
#' @examples
#' getBrainStarsFigure("1439627_at", "exprgraph")
#' getBrainStarsFigure("1439627_at", "exprmap")
#' getBrainStarsFigure("1439627_at", "switchgraph")
#' getBrainStarsFigure("1439627_at", "switchhist")
#' getBrainStarsFigure("1439627_at", "switchmap")
getBrainStarsFigure <- function(id, fig.type) {
  
  base.url    <- "http://brainstars.org/probeset/"
  url.option.list <- c("exprgraph", "exprmap", "switchgraph", "switchhist", "switchmap")

  if (sum(url.option.list == fig.type) != 1) {
    error.meg <- "Specify figure type."
    error.meg <- paste(error.meg, paste(url.option.list, collapse=", "))
    stop(warn=error.meg)
  }

  cat("Downloading...\n")
  url         <- paste(base.url, id, "/", fig.type, ".png", sep="")
  file.name   <- paste(id, ".", fig.type, ".png", sep="")
  plot <- getBinaryURL(url)
  cat("Done.\n")
  
  writeBin(plot, file.name)
  invisible(plot)
}

#' Keyword search
#'
#' This function search by keyword in BrainStars
#'
#' @usage getBrainStarsSearch(keyword)
#' @param keyword keyword
#' @keywords bio
#' @export
#' @examples
#' recep.list  <- getBrainStarsSearch("receptor/10,5")
#' recep.count <- getBrainStarsSearch("receptor/count")
getBrainStarsSearch <- function(keyword) {
  
  base.url    <- "http://brainstars.org/search/"

#  if (sum(keyword) != 1) {
#    error.meg <- "Input keyword type."
#    error.meg <- paste(error.meg, paste(keyword, collapse=", "))
#    stop(warn=error.meg)
#  }

  cat("Downloading...\n")
  url         <- paste(base.url, keyword, sep="")
  cat("Done.\n")
  results <- getURL(url)
  results <- unlist(strsplit(results, "\n"))
  return(results)
  
#  writeBin(plot, file.name)
#  invisible(plot)
}

#' Get brain region specific markers
#'
#' This function get brain region specific markers in BrainStars
#'
#' @usage getBrainStarsMarker(keyword)
#' @param keyword keyword
#' @keywords bio
#' @export
#' @examples
#' my.genes.json <- getBrainStarsMarker("high/LS/count")
getBrainStarsMarker <- function(keyword) {
  
  base.url    <- "http://brainstars.org/marker/"
  json.option <- "?content-type=application/json"
  
  cat("Downloading...\n")
  url         <- paste(base.url, keyword, json.option, sep="")
  cat(url, "\n")
  cat("Done.\n")
  results <- getURL(url)
  #results <- unlist(strsplit(results, "\n"))
  return(results)
}
