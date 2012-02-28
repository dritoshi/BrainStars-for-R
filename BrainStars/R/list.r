## List API

#' Gene Marker search in BrainStars
#'
#' This function queries gene marker from BrainStars using by List API.
#'
#' @usage getBrainStarsMarker(query)
#' @param query keyword
#' @return A character vector of list API response in JSON.
#' @details List API is for retrieving a list of gene marker candidates.
#' Keyword search format: {high,low}/(region)/(offset),(limit).
#'
#' Count of entriest search format: {high,low}/(region)/count.
#'
#' "high" is highly expressed regions. "low" is low expressed regions.
#' (region) means CNS region name.
#'
#'   output:
#'   If the result has at least one hit entries, a list of entry IDs is returned
#'   in RJSONIN format. If not, 404 Not found error code is returned. "offset,limit"
#'   can be used to retrieve a part of hit entries. If "offset,limit" is not given,
#'   all hits are returned.
#'
#' @export
#' @examples
#' mk.genes <- getBrainStarsMarker("low/SCN/all")
getBrainStarsMarker <- function(query) {
  api.name = "marker"
  res <- getBrainStars(query = query, type = api.name)
  res
}

# "multistate": multi-state genes
#' Search Multi-state genes search from BrainStars
#'
#' This function queries multi-state genes from BrainStars using by List API.
#'
#' @usage getBrainStarsMultistate(query)
#' @param  query keyword
#' @return A character vector of list API response in JSON.
#' @details List API is for retrieving a list of multi-state gene candidates.
#' Keyword search format: {high,up,low,down}/(region)/(offset),(limit).
#'
#' Count of entries search format: {high,up,low,down}/(region)/count.
#'
#' "high": high state regions,
#' "up": up state regions,
#' "low": low state regions,
#' "down": down state regions.
#'
#' (region) means CNS region name.
#'
#'  output:
#'  If the result has at least one hit entries, a list of entry IDs is returned
#'  in RJSONIN format. If not, 404 Not found error code is returned. "offset,limit"
#'  can be used to retrieve a part of hit entries. If "offset,limit" is not given,
#'  all hits are returned.
#'
#' @export
#' @examples
#' ms.genes <- getBrainStarsMultistate("low/SCN/all")
getBrainStarsMultistate <- function(query) {
  api.name <- "multistate"
  res <- getBrainStars(query = query, type = api.name)
  res
}

#' Search One-state genes search from BrainStars
#'
#' This function queries one-state genes from BrainStars using by List API.
#'
#' @usage getBrainStarsOnestate(query)
#' @param query keyword
#' @return A character vector of list API response in JSON.
#' @details List API is for retrieving a list of one-state gene candidates.
#' Keyword search format: (offset),(limit).
#'
#' Count of entries search format: count.
#'
#'  output:
#'  If the result has at least one hit entries, a list of entry IDs is returned
#'  in RJSONIN format. If not, 404 Not found error code is returned. "offset,limit"
#'  can be used to retrieve a part of hit entries. If "offset,limit" is not given,
#'  all hits are returned.
#'
#' @export
#' @examples
#' os.genes <- getBrainStarsOnestate("count")
getBrainStarsOnestate <- function(query) {
  api.name <- "onestate"
  res <- getBrainStars(query = query, type = api.name)
  res
}

#' Search Inferred connections among CNS regions from BrainStars
#'
#' This function queries inferred connections among CNS regions by
#' neurotransmitter/neurohormone (ntnh) from BrainStars using by List API.
#'
#' @usage getBrainStarsNtNh(query)
#' @param query keyword
#' @return A character vector of list API response in JSON.
#' @details List API is for retrieving inferred connections among CNS
#' regions by neurotransmitter/neurohormone (ntnh).
#'
#' Keyword search format: {high,low}/(ligand-region)/(receptor-region)/(offset),(limit).
#'
#' Count of entries search format: {high,low}/(ligand-region)/(receptor-region)/count.
#' 
#' "high": high state regions, "up": up state regions.
#' 
#' (ligand-region): Ligand CNS region. 
#' (receptor-region): Receptor CNS region.
#'
#'  output:
#'  If the result has at least one hit entries, a list of entry IDs is returned
#'  in RJSONIN format. If not, 404 Not found error code is returned. "offset,limit"
#'  can be used to retrieve a part of hit entries. If "offset,limit" is not given,
#'  all hits are returned.
#'
#' @export
#' @examples
#' os.genes <- getBrainStarsNtNh("high/SCN/ME/all")
getBrainStarsNtNh <- function(query) {
  api.name <- "ntnh"
  res <- getBrainStars(query = query, type = api.name)
  res
}
#getBrainStarsNeurohormone     <- function(query) { getBrainStarsNtNh(query) }
#getBrainStarsNeurotransmitter <- function(query) { getBrainStarsNtNh(query) }

#' Search gene family or categories from BrainStars
#'
#' This function queries a list of genes by gene family or categories name
#' from BrainStars using by List API.
#'
#' @usage getBrainStarsGeneFamCat(query)
#' @param query keyword
#' @return A character vector of list API response in JSON.
#' @details List API is for retrieving genes in specific gene family or categories.
#'
#' Keyword search format: (category)/(keyword)/(offset),(limit)
#'
#' Count of entries search format: (category)/(keyword)/count
#'
#' (category): gene family / category name
#' \itemize{
#'   \item "tf": transcription factors
#'   \item "transmem": transmembrane genes
#'   \item "channel": channel genes
#'   \item "gpcr": GPCR genes
#'   \item "adhesion": cell adhesion genes
#'   \item "excellmat": extracellular matrix genes
#'   \item "structural": structural protein genes
#'   \item "neurogenesis": neurogenesis related genes
#'   \item "hox": homeobox genes
#'   \item "nucrcpt": nuclear receptor genes
#'   \item "ntnh": neurotransmitter/neurohormone genes
#'   \item "axon": axon guidance genes
#'   \item "fox": forkhead genes
#' }
#'
#' (keyword): keyword. If no keyword search required, make this omited or blank.
#'
#'  output:
#'  If the result has at least one hit entries, a list of entry IDs is returned
#'  in RJSONIN format. If not, 404 Not found error code is returned. "offset,limit"
#'  can be used to retrieve a part of hit entries. If "offset,limit" is not given,
#'  all hits are returned.
#'
#' @export
#' @examples
#' gfc.genes1 <- getBrainStarsGeneFamCat("tf//count")
#' gfc.genes2 <- getBrainStarsGeneFamCat("tf/terminal/all")
#' gfc.genes3 <- getBrainStarsGeneFamCat("tf/terminal/count")
getBrainStarsGeneFamCat <- function(query) {
  api.name <- "genefamily"
  res <- getBrainStars(query = query, type = api.name)
  res
}  
#getBrainStarsGeneCategory <- function(query) { getBrainStarsGeneFamily(query) }
