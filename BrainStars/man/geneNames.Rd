\name{geneNames}
\alias{geneNames}
\title{Extract Gene Names from JSON Format}
\usage{
  geneNames(rjsonio)
}
\arguments{
  \item{rjsonio}{result of List or Search API}
}
\value{
  A cheracter vector of Gene Names.
}
\description{
  This function is extraction Gene Name from result of List
  or Search API
}
\examples{
mk.genes <- getBrainStars(query = "low/SCN/all", type = "marker", json = TRUE)
mk.genenames <- geneNames(mk.genes)
}

