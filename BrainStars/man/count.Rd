\name{count}
\alias{count}
\title{Extract a number of hits from JSON Format of Count API}
\usage{
  count(rjsonio)
}
\arguments{
  \item{rjsonio}{result of List or Search API}
}
\value{
  A numeric vector of a number of hits
}
\description{
  This function extract a number of hits from result of
  List or Search count API
}
\examples{
mk.genes <- getBrainStars(query = "low/SCN/count", type = "marker", json = TRUE)
mk.count <- count(mk.genes)
}

