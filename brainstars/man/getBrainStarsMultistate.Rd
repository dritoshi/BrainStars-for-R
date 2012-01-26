\name{getBrainStarsMultistate}
\alias{getBrainStarsMultistate}
\title{Search Multi-state genes search from BrainStars}
\usage{
  getBrainStarsMultistate(query)
}
\arguments{
  \item{query}{keyword}
}
\description{
  This function queries multi-state genes from BrainStars
  using by List API.
}
\details{
  List API is for retrieving a list of multi-state gene
  candidates. Keyword search format:
  {high,up,low,down}/(region)/(offset),(limit).

  Count of entries search format:
  {high,up,low,down}/(region)/count.

  "high": high state regions, "up": up state regions,
  "low": low state regions, "down": down state regions.

  (region) means CNS region name.

  output: If the result has at least one hit entries, a
  list of entry IDs is returned in RJSONIN format. If not,
  404 Not found error code is returned. "offset,limit" can
  be used to retrieve a part of hit entries. If
  "offset,limit" is not given, all hits are returned.
}
\examples{
ms.genes <- getBrainStarsMultistate("low/SCN/all")
}

