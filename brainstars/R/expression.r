.getBrainStarsExpression <- function(query) {
  api.name = "probeset"
  res = new("BrainStars", query=query, api.name=api.name)
  
  url <- fromJSON(res@response)$file1d$csv["href"]

  expr <- read.csv(url, row.names=1)

  return(as.matrix(expr))
}
#' Retreive a list of expression profile from BrainStars
#'
#' This function queries a list of gene expression profile from BrainStars
#'
#' @usage getBrainStarsExpression(queries)
#' @param queries vector of Affymetrix GeneChip IDs (Mouse Genome 430 2.0 array)
#' @return An object of ExpressionSet is returned.
#' @export
#' @importClassesFrom Biobase ExpressionSet
#' @importMethodsFrom Biobase annotation exprs featureNames pData rowQ varLabels "featureNames<-" "sampleNames<-"
#' @examples
#' mk.genes <- getBrainStarsMarker("low/SCN/all")
#' mk.ids   <- probeSetIDs(mk.genes)
#' mk.eset  <- getBrainStarsExpression(mk.ids)
getBrainStarsExpression <- function(queries) {

  expr <- .getBrainStarsExpression(queries[1])
  num.expr    <- ncol(expr)
  num.entries <- length(queries)
  
  expr.mat <- matrix(nrow=num.entries, ncol=num.expr)

  expr.mat[1,] <- expr
  sample.names <- colnames(expr)

  if (num.entries > 1 ) {
    for (i in 2:num.entries) {
      expr.mat[i,] <- .getBrainStarsExpression(queries[i])
    }
  }
  
  eset <- new("ExpressionSet", expr = expr.mat)
  featureNames(eset) <- queries
  sampleNames(eset)  <- sample.names
  
  return(eset)
} 
