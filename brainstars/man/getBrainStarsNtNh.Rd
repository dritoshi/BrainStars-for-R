\name{getBrainStarsNtNh}
\alias{getBrainStarsNtNh}
\title{Search Inferred connections among CNS regions from BrainStars}
\usage{
  getBrainStarsNtNh(query)
}
\arguments{
  \item{query}{keyword}
}
\description{
  This function queries inferred connections among CNS
  regions by neurotransmitter/neurohormone (ntnh) from
  BrainStars using by List API.
}
\details{
  List API is for retrieving inferred connections among CNS
  regions by neurotransmitter/neurohormone (ntnh).

  Keyword search format:
  {high,low}/(ligand-region)/(receptor-region)/(offset),(limit).

  Count of entries search format:
  {high,low}/(ligand-region)/(receptor-region)/count.

  "high": high state regions, "up": up state regions.

  (ligand-region): Ligand CNS region. (receptor-region):
  Receptor CNS region.

  output: If the result has at least one hit entries, a
  list of entry IDs is returned in RJSONIN format. If not,
  404 Not found error code is returned. "offset,limit" can
  be used to retrieve a part of hit entries. If
  "offset,limit" is not given, all hits are returned.
}
\examples{
os.genes <- getBrainStarsNtNh("high/SCN/ME/all")
}

