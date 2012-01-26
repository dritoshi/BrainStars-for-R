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

setClass("BrainStars",
  representation(
    query    = "character",
    response = "character",
    base.url = "character",    
    api.name = "character"
  ),
  prototype(
    query    = "receptor/1,5",
    response = "",
    base.url = "http://brainstars.org/",
    api.name = "search"
  ),
)

setMethod("initialize", "BrainStars",
  function(.Object, query, api.name) {
    .Object@query    <- query
    .Object@api.name <- api.name
    .Object@response <- .getBrainStars(.Object)
    .Object
  }
)

.getBrainStars <- function(object) {
  url.option <- "?content-type=application/json"
  url <- paste(
    object@base.url,
    object@api.name,  "/",
    object@query,
    url.option,
    sep=""
  )
  #cat("Download data from", url, "\n")
  response = getURL(url)

  # check error
  if (length(grep("\\*\\*\\*ERROR\\*\\*\\*", response)) > 0) {
    response <- paste('{ "error":"', "No hit.\"\n", '"details":', response, '" }', sep="")
    stop(warn = response)
  }
  return(response)
}
