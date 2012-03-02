\name{probeset2list}
\alias{probeset2list}
\title{Transform results of probeset's json to list}
\usage{
  probeset2list(rjsonio)
}
\arguments{
  \item{rjsonio}{result of List or Search API with type
  "probeset"}
}
\value{
  A list
}
\description{
  This function parses entries of response from probeset's
  result of List API
}
\examples{
mk.genes <- getBrainStars(query = "1450371_at", type = "probeset", json = TRUE)
mk.ntnh.mat <- probeset2list(mk.genes)
}

