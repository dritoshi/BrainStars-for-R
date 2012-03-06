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
#' @usage getBrainStars(query, type, base.url, json)
#' @param query keyword
#' @param type BrainStars API name. (expression, search, probeset, marker, multistate, onestate, ntnh and genefamily)
#' @param base.url URL of Brainstars database.
#' @param json TRUE is json mode of response. (Defalut is FALSE)
#' @return A matrix, ExpressionSet, list of annotation or character vector of Search API response in JSON.
#'
#' @export
#'
#' @importClassesFrom Biobase ExpressionSet
#' @importMethodsFrom Biobase annotation exprs featureNames pData rowQ varLabels "featureNames<-" "sampleNames<-"
#' @importMethodsFrom RJSONIO fromJSON
#' @details Brain API is for keyword search and is based on Tokyo Manifesto and
#' TogoWS REST interface.
#' 
#' Type: expression
#' (query)
#' "query" is a vector of ProbeSet IDs.
#' 
#' Type: search, probeset and onestate
#' Keyword for retrieving a list of hit entries:
#' (query+string)[/(offset),(limit)].
#' 
#'  Keyword for retrieving the count of hit entries:
#'  (query+string)/count.
#'
#' Type: genefamily
#' Keyword search format: (category)/(keyword)/(offset),(limit)
#'
#' You can indicate gene category name in the following words:
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
#' Type: marker
#' List API is for retrieving a list of gene marker candidates.
#' Keyword search format: {high,low}/(region)/(offset),(limit).
#'
#' Count of entriest search format: {high,low}/(region)/count.
#'
#' "high" is highly expressed regions. "low" is low expressed regions.
#' (region) means CNS region name.
#'
#' Type: ntnh
#' List API is for retrieving inferred connections among CNS
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
#' Type: multistate
#' List API is for retrieving a list of multi-state gene candidates.
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
#'   output:
#'   If json is TRUE, you get response in JSON character. Dafault is matrix or list.
#'   If you chose "expression" type, you can get response in ExpressionSet.
#'   When you indicate "probeset" type, you can get annotations in list.
#'
#'   If the result has at least one hit entries, a matrix of entries is returned.
#'   If not, 404 Not found error code is returned. "offset,limit"
#'   can be used to retrieve a part of hit entries. If "offset,limit" is not given,
#'   all hits are returned.
#'
#'   If "count" was included in "query", the count of hit entries is returned 
#'   in matrix, list, ExpressionSet or JSON format
#'
#' @examples
#' my.search   <- getBrainStars(query = "receptor",   type = "search")
#' my.probeset <- getBrainStars(query = "1439627_at", type = "probeset")
#' my.tf       <- getBrainStars(query = "tf",         type = "genefamily")
#' my.eset     <- getBrainStars(query = "1439627_at", type = "expression")
#'
#' my.probeset.json <- getBrainStars(
#'   query = "1439627_at",
#'   type = "probeset",
#'   json = TRUE
#' )
getBrainStars <- function(
  query    = "receptor/1,5",
  type     = "search",   # API name	
  base.url = "http://brainstars.org/",
  json     = FALSE) {

  url.option <- "?content-type=application/json"
  url <- paste(
    base.url,
    type,  "/",
    query,
    url.option,
    sep = ""
  )
  #cat("Download data from", url, "\n")
  response = getURL(url)

  if (type == "expression") {
    return( getBrainStarsExpression(query) )
  }

  # check error
  if (length(grep("\\*\\*\\*ERROR\\*\\*\\*", response)) > 0) {
    response <- paste('{ "error":"', "No hit.\"\n", '"details":', response, '" }', sep="")
    stop(warn = response)
  }

  if (json == TRUE) {
    return(response)
  } else {
    if (type == "probeset") {  ## Entry API
      return( probeset2list(response) )
    } else {  ## search and list API
	
	  ## count API	
	  if (! identical(grep("count", query), integer(0)) ) {
	    return( count(response) )
	  }
	  ## type = ntnh 
	  if (type == "ntnh") {
		return(ntnh2mat(response))
	  }
	
	  # search and list API (excluding type of "ntnh")
  	  response.mat <- cbind(
	    probeSetIDs(response),
	    geneSymbols(response),
	    geneNames(response)
      )
      rownames(response.mat) <- 1:nrow(response.mat)
      colnames(response.mat) <- c("Probeset ID", "Gene Symbol", "Gene Name")

	  return(response.mat)
	}
  }	
}
