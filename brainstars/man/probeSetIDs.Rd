\name{probeSetIDs}
\alias{probeSetIDs}
\title{Extract Probeset ID from JSON Format}
\usage{
  probeSetIDs(rjsonio)
}
\arguments{
  \item{rjsonio}{result of List or Search API}
}
\description{
  This function is extraction probeset id from result of
  List or Search API
}
\examples{
mk.genes <- getBrainStarsMarker("low/SCN/all")
mk.ids   <- probeSetIDs(mk.genes)
}

