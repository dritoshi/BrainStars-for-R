\name{getBrainStars}
\alias{getBrainStars}
\title{Keyword search in BrainStars}
\usage{
  getBrainStars(query)
}
\arguments{
  \item{query}{keyword}
}
\value{
  A character vector of Search API response in JSON.
}
\description{
  This function queries entry information from BrainStars
  using by Search API.
}
\details{
  Search API is for keyword search and is based on Tokyo
  Manifesto and TogoWS REST interface. Keyword for
  retrieving a list of hit entries:
  (query+string)[/(offset),(limit)].

  output: If the result has at least one hit entries, a
  list of entry IDs is returned in RJSONIN format. If not,
  404 Not found error code is returned. "offset,limit" can
  be used to retrieve a part of hit entries. If
  "offset,limit" is not given, all hits are returned.

  Keyword for retrieving the count of hit entries:
  (query+string)/count.

  output: The count of hit entries is returned in RJSONIO
  format
}
\examples{
my.search            <- getBrainStars("receptor")
my.search.offset.num <- getBrainStars("receptor/1,5")
my.search.count      <- getBrainStars("receptor/count")
}

