#' Extract Probeset ID from JSON Format
#'
#' This function is extraction probeset id from result of List or Search API
#'
#' @usage probeSetIDs(rjsonio)
#' @param rjsonio result of List or Search API
#' @return A cheracter vector of ProbeSet IDs.
#' @export
#' @importMethodsFrom RJSONIO fromJSON
#' @examples
#' mk.genes <- getBrainStars(query = "low/SCN/all", type = "marker", output = "json")
#' mk.ids   <- probeSetIDs(mk.genes)
probeSetIDs <- function(rjsonio) {
  json <- fromJSON(rjsonio) 
  if ( length(json$entries) > 0) {
    unlist( lapply(json$entries, function(x) x["pbsetid"]) )
  } else if (length(json$result) > 0) {
    unlist(lapply(json$result, function(x) x["pbsetid"]))
  } else {
    msg <-  "Input result of List or Search API."
    stop(warn = msg)
  }    
}  
#' Extract GeneSymbol from JSON Format
#'
#' This function is extraction GeneSymbol from result of List or Search API
#'
#' @usage geneSymbols(rjsonio)
#' @param rjsonio result of List or Search API
#' @return A cheracter vector of Gene Symbols.
#' @export
#' @importMethodsFrom RJSONIO fromJSON 
#' @examples
#' mk.genes <- getBrainStars(query = "low/SCN/all", type = "marker", output = "json")
#' mk.genesymbols <- geneSymbols(mk.genes)
geneSymbols <- function(rjsonio) {
  json <- fromJSON(rjsonio) 
  if (length(json$entries) > 0) {
    unlist(lapply(json$entries, function(x) x["symbol"]))
  } else if (length(json$result) > 0) {
    unlist(lapply(json$result, function(x) x["symbol"]))
  } else {
    msg <- "Input result of List or Search API."
    stop(warn = msg)
  }    
}
#' Extract Gene Names from JSON Format
#'
#' This function is extraction Gene Name from result of List or Search API
#'
#' @usage geneNames(rjsonio)
#' @param rjsonio result of List or Search API
#' @return A cheracter vector of Gene Names.
#' @importMethodsFrom RJSONIO fromJSON 
#' @export
#' @examples
#' mk.genes <- getBrainStars(query = "low/SCN/all", type = "marker", output = "json")
#' mk.genenames <- geneNames(mk.genes)
geneNames <- function(rjsonio) {
  json <- fromJSON(rjsonio) 
  if (length(json$entries) > 0) {
    unlist(lapply(json$entries, function(x) x["name"]))
  } else if (length(json$result) > 0) {
    unlist(lapply(json$result, function(x) x["name"]))
  } else {
    msg <- "Input result of List or Search API."
    stop(warn = msg)
  }    
}

#' Extract a number of hits from JSON Format of Count API
#'
#' This function extract a number of hits from result of List or Search count API
#'
#' @usage count(rjsonio)
#' @param rjsonio result of List or Search API
#' @return A numeric vector of a number of hits
#' @export
#' @importMethodsFrom RJSONIO fromJSON 
#' @examples
#' mk.genes <- getBrainStars(query = "low/SCN/count", type = "marker", output = "json")
#' mk.count <- count(mk.genes)
count <- function(rjsonio) {
  json <- fromJSON(rjsonio) 
  if (json > 0) {
    return(json)
  } else {
    msg <- "Input result of List or Search API."
    stop(warn = msg)
  }    
}

#' Transform results of ntnh's json to matrix
#'
#' This function parses entries of response from ntnh's result of List API
#'
#' @usage ntnh2mat(rjsonio)
#' @param rjsonio result of List or Search API with type "ntnh"
#' @return A matrix
#' @export
#' @importMethodsFrom RJSONIO fromJSON 
#' @examples
#' mk.genes <- getBrainStars(query = "high/SCN/ME/all", type = "ntnh", output = "json")
#' mk.ntnh.mat <- ntnh2mat(mk.genes)
ntnh2mat <- function(rjsonio) {
  json <- fromJSON(rjsonio)	
  num.hits <- length( json$entries )
  if (num.hits > 0) {
    response.mat <- matrix(unlist(json$entries), ncol = 12, byrow = T)
    colnames(response.mat) <- c("receptor_href", "ligand_name", "compound",
      "receptor_pbsetid", "ligand_symbol", "total_highregions", "ligand_href",
      "ligand_pbsetid",   "receptor_name", "ligand_highregions",
      "receptor_symbol", "receptor_highregions")
    return(response.mat)
  }
}