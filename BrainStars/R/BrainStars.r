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
#' @name BrainStars
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

#' Get various information in BrainStars
#'
#' This function queries various information from BrainStars
#' The function is a wrapper of All BrainStars API.
#'
#' @usage getBrainStars(query, type, base.url)
#' @param query keyword
#' @param type BrainStars API name. (search, probeset, marker, multistate, onestate, ntnh and genefamily)
#' @param base.url URL of Brainstars database.
#' @return A character vector of Search API response in JSON.
#' @details Brain API is for keyword search and is based on Tokyo Manifesto and
#' TogoWS REST interface. Keyword for retrieving a list of hit entries:
#' (query+string)[/(offset),(limit)].
#'
#'   output:
#'   If the result has at least one hit entries, a list of entry IDs is returned
#'   in RJSONIN format. If not, 404 Not found error code is returned. "offset,limit"
#'   can be used to retrieve a part of hit entries. If "offset,limit" is not given,
#'   all hits are returned.
#'
#'   Keyword for retrieving the count of hit entries:
#'  (query+string)/count.
#'
#'    output: The count of hit entries is returned in RJSONIO format
#' @export
#' @examples
#' my.search   <- getBrainStars(query = "receptor",   type = "search")
#' my.probeset <- getBrainStars(query = "1439627_at", type = "probeset")
#' my.tf       <- getBrainStars(query = "tf",         type = "genefamily")
getBrainStars <- function(
	base.url = "http://brainstars.org/",
	type = "search",   # API name
	query = "receptor/1,5") {
  url.option <- "?content-type=application/json"
  url <- paste(
    base.url,
    type,  "/",
    query,
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
