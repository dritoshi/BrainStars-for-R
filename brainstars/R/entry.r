# Entry API

#' Get an entry information in JSON format
#'
#' This function get entry information (annotation, expressions, links)
#' in JSON format from BrainStars using by Entry API.
#'
#' @usage getBrainStarsEntry(query)
#' @param query Affymetrix GeneChip ID (Mouse Genome 430 2.0 array)
#' @return A character vector of entry API response in JSON.
#'
#' @export
#' @examples
#' my.entry <- getBrainStarsEntry("1439627_at")
getBrainStarsEntry <- function(query) {
  api.name = "probeset"
  res <- new("BrainStars", query=query, api.name=api.name)
  return(res@response)
}  
