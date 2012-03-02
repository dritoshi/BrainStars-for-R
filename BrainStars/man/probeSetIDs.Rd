\name{probeSetIDs}
\alias{probeSetIDs}
\title{Extract Probeset ID from JSON Format}
\usage{
  probeSetIDs(rjsonio)
}
\arguments{
  \item{rjsonio}{result of List or Search API}
}
\value{
  A cheracter vector of ProbeSet IDs.
}
\description{
  This function is extraction probeset id from result of
  List or Search API
}
\examples{
mk.genes <- getBrainStars(query = "low/SCN/all", type = "marker", json = TRUE)
mk.ids   <- probeSetIDs(mk.genes)
}

