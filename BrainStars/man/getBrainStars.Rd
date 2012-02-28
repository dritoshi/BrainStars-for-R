\name{getBrainStars}
\alias{getBrainStars}
\title{Get various information in BrainStars}
\usage{
  getBrainStars(query, type, base.url)
}
\arguments{
  \item{query}{keyword}

  \item{type}{BrainStars API name. (search, probeset,
  marker, multistate, onestate, ntnh and genefamily)}

  \item{base.url}{URL of Brainstars database.}
}
\value{
  A character vector of Search API response in JSON.
}
\description{
  This function queries various information from BrainStars
  The function is a wrapper of All BrainStars API.
}
\details{
  Brain API is for keyword search and is based on Tokyo
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
my.search   <- getBrainStars(query = "receptor",   type = "search")
my.probeset <- getBrainStars(query = "1439627_at", type = "probeset")
my.tf       <- getBrainStars(query = "tf",         type = "genefamily")
}

