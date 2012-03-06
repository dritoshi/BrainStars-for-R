\name{getBrainStars}
\alias{getBrainStars}
\title{Get various information in BrainStars}
\usage{
  getBrainStars(query, type, base.url, json)
}
\arguments{
  \item{query}{keyword}

  \item{type}{BrainStars API name. (expression, search,
  probeset, marker, multistate, onestate, ntnh and
  genefamily)}

  \item{base.url}{URL of Brainstars database.}

  \item{json}{TRUE is json mode of response. (Defalut is
  FALSE)}
}
\value{
  A matrix, ExpressionSet, list of annotation or character
  vector of Search API response in JSON.
}
\description{
  This function queries various information from BrainStars
  The function is a wrapper of All BrainStars API.
}
\details{
  Brain API is for keyword search and is based on Tokyo
  Manifesto and TogoWS REST interface.

  Type: expression (query) "query" is a vector of ProbeSet
  IDs.

  Type: search, probeset and onestate Keyword for
  retrieving a list of hit entries:
  (query+string)[/(offset),(limit)].

  Keyword for retrieving the count of hit entries:
  (query+string)/count.

  Type: genefamily Keyword search format:
  (category)/(keyword)/(offset),(limit)

  You can indicate gene category name in the following
  words: \itemize{ \item "tf": transcription factors \item
  "transmem": transmembrane genes \item "channel": channel
  genes \item "gpcr": GPCR genes \item "adhesion": cell
  adhesion genes \item "excellmat": extracellular matrix
  genes \item "structural": structural protein genes \item
  "neurogenesis": neurogenesis related genes \item "hox":
  homeobox genes \item "nucrcpt": nuclear receptor genes
  \item "ntnh": neurotransmitter/neurohormone genes \item
  "axon": axon guidance genes \item "fox": forkhead genes }

  Type: marker List API is for retrieving a list of gene
  marker candidates. Keyword search format:
  {high,low}/(region)/(offset),(limit).

  Count of entriest search format:
  {high,low}/(region)/count.

  "high" is highly expressed regions. "low" is low
  expressed regions. (region) means CNS region name.

  Type: ntnh List API is for retrieving inferred
  connections among CNS regions by
  neurotransmitter/neurohormone (ntnh).

  Keyword search format:
  {high,low}/(ligand-region)/(receptor-region)/(offset),(limit).

  Count of entries search format:
  {high,low}/(ligand-region)/(receptor-region)/count.

  "high": high state regions, "up": up state regions.

  (ligand-region): Ligand CNS region. (receptor-region):
  Receptor CNS region.

  Type: multistate List API is for retrieving a list of
  multi-state gene candidates. Keyword search format:
  {high,up,low,down}/(region)/(offset),(limit).

  Count of entries search format:
  {high,up,low,down}/(region)/count.

  "high": high state regions, "up": up state regions,
  "low": low state regions, "down": down state regions.

  (region) means CNS region name.

  output: If json is TRUE, you get response in JSON
  character. Dafault is matrix or list.  If you chose
  "expression" type, you can get response in ExpressionSet.
  When you indicate "probeset" type, you can get
  annotations in list.

  If the result has at least one hit entries, a matrix of
  entries is returned.  If not, 404 Not found error code is
  returned. "offset,limit" can be used to retrieve a part
  of hit entries. If "offset,limit" is not given, all hits
  are returned.

  If "count" was included in "query", the count of hit
  entries is returned in matrix, list, ExpressionSet or
  JSON format
}
\examples{
my.search   <- getBrainStars(query = "receptor",   type = "search")
my.probeset <- getBrainStars(query = "1439627_at", type = "probeset")
my.tf       <- getBrainStars(query = "tf",         type = "genefamily")
my.eset     <- getBrainStars(query = "1439627_at", type = "expression")

my.probeset.json <- getBrainStars(
  query = "1439627_at",
  type = "probeset",
  json = TRUE
)
}

