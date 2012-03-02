\name{ntnh2mat}
\alias{ntnh2mat}
\title{Transform results of ntnh's json to matrix}
\usage{
  ntnh2mat(rjsonio)
}
\arguments{
  \item{rjsonio}{result of List or Search API with type
  "ntnh"}
}
\value{
  A matrix
}
\description{
  This function parses entries of response from ntnh's
  result of List API
}
\examples{
mk.genes <- getBrainStars(query = "high/SCN/ME/all", type = "ntnh", output = "json")
mk.ntnh.mat <- ntnh2mat(mk.genes)
}

