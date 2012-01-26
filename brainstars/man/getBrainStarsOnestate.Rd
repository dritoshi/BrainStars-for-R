\name{getBrainStarsOnestate}
\alias{getBrainStarsOnestate}
\title{Search One-state genes search from BrainStars}
\usage{
  getBrainStarsOnestate(query)
}
\arguments{
  \item{query}{keyword}
}
\description{
  This function queries one-state genes from BrainStars
  using by List API.
}
\details{
  List API is for retrieving a list of one-state gene
  candidates. Keyword search format: (offset),(limit).

  Count of entries search format: count.

  output: If the result has at least one hit entries, a
  list of entry IDs is returned in RJSONIN format. If not,
  404 Not found error code is returned. "offset,limit" can
  be used to retrieve a part of hit entries. If
  "offset,limit" is not given, all hits are returned.
}
\examples{
os.genes <- getBrainStarsOnestate("count")
}

