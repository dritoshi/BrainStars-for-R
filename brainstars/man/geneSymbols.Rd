\name{geneSymbols}
\alias{geneSymbols}
\title{Extract GeneSymbol from JSON Format}
\usage{
  geneSymbols(rjsonio)
}
\arguments{
  \item{rjsonio}{result of List or Search API}
}
\description{
  This function is extraction GeneSymbol from result of
  List or Search API
}
\examples{
mk.genes <- getBrainStarsMarker("low/SCN/all")
mk.genesymbols   <- geneSymbols(mk.genes)
}

