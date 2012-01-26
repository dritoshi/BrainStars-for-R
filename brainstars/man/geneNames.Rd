\name{geneNames}
\alias{geneNames}
\title{Extract Gene Names from JSON Format}
\usage{
  geneNames(rjsonio)
}
\arguments{
  \item{rjsonio}{result of List or Search API}
}
\description{
  This function is extraction Gene Name from result of List
  or Search API
}
\examples{
mk.genes <- getBrainStarsMarker("low/SCN/all")
mk.genenames   <- geneNames(mk.genes)
}

