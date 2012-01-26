\name{getBrainStarsMarker}
\alias{getBrainStarsMarker}
\title{Gene Marker search in BrainStars}
\usage{
  getBrainStarsMarker(query)
}
\arguments{
  \item{query}{keyword}
}
\description{
  This function queries gene marker from BrainStars using
  by List API.
}
\details{
  List API is for retrieving a list of gene marker
  candidates. Keyword search format:
  {high,low}/(region)/(offset),(limit).

  Count of entriest search format:
  {high,low}/(region)/count.

  "high" is highly expressed regions. "low" is low
  expressed regions. (region) means CNS region name.

  output: If the result has at least one hit entries, a
  list of entry IDs is returned in RJSONIN format. If not,
  404 Not found error code is returned. "offset,limit" can
  be used to retrieve a part of hit entries. If
  "offset,limit" is not given, all hits are returned.
}
\examples{
mk.genes <- getBrainStarsMarker("low/SCN/all")
}

