#' Extract Probeset ID from JSON Format
#'
#' This function is extraction probeset id from result of List or Search API
#'
#' @usage probeSetIDs(rjsonio)
#' @param rjsonio result of List or Search API
#' @export
#' @examples
#' mk.genes <- getBrainStarsMarker("low/SCN/all")
#' mk.ids   <- probeSetIDs(mk.genes)
probeSetIDs <- function(rjsonio) {
  if ( length(fromJSON(rjsonio)$entries ) > 0) {
    unlist(lapply(fromJSON(rjsonio)$entries, function(x) x$pbsetid))
  } else if (length(fromJSON(rjsonio)$result) > 0) {
    unlist(lapply(fromJSON(rjsonio)$result, function(x) x$pbsetid))
  } else {
    msg <-  "Input result of List or Search API."
    stop(warn=msg)
  }    
}  
#' Extract GeneSymbol from JSON Format
#'
#' This function is extraction GeneSymbol from result of List or Search API
#'
#' @usage geneSymbols(rjsonio)
#' @param rjsonio result of List or Search API
#' @export
#' @examples
#' mk.genes <- getBrainStarsMarker("low/SCN/all")
#' mk.genesymbols   <- geneSymbols(mk.genes)
geneSymbols <- function(rjsonio) {
  if (length(fromJSON(rjsonio)$entries) > 0) {
    unlist(lapply(fromJSON(rjsonio)$entries, function(x) x$symbol))
  } else if (length(fromJSON(rjsonio)$result) > 0) {
    unlist(lapply(fromJSON(rjsonio)$result, function(x) x$symbol))
  } else {
    msg <-  "Input result of List or Search API."
    stop(warn=msg)
  }    
}
#' Extract Gene Names from JSON Format
#'
#' This function is extraction Gene Name from result of List or Search API
#'
#' @usage geneNames(rjsonio)
#' @param rjsonio result of List or Search API
#' @export
#' @examples
#' mk.genes <- getBrainStarsMarker("low/SCN/all")
#' mk.genenames   <- geneNames(mk.genes)
geneNames <- function(rjsonio) {
  if (length(fromJSON(rjsonio)$entries) > 0) {
    unlist(lapply(fromJSON(rjsonio)$entries, function(x) x$name))
  } else if (length(fromJSON(rjsonio)$result) > 0) {
    unlist(lapply(fromJSON(rjsonio)$result, function(x) x$name))
  } else {
    msg <-  "Input result of List or Search API."
    stop(warn=msg)
  }    
}  
