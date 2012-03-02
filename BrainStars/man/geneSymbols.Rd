\name{geneSymbols}
\alias{geneSymbols}
\title{Extract GeneSymbol from JSON Format}
\usage{
  geneSymbols(rjsonio)
}
\arguments{
  \item{rjsonio}{result of List or Search API}
}
\value{
  A cheracter vector of Gene Symbols.
}
\description{
  This function is extraction GeneSymbol from result of
  List or Search API
}
\examples{
mk.genes <- getBrainStars(query = "low/SCN/all", type = "marker", json = TRUE)
mk.genesymbols <- geneSymbols(mk.genes)
}

