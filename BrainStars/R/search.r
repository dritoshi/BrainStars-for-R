# Search API
#' Keyword search in BrainStars
#'
#' This function queries entry information from BrainStars
#' using by Search API.
#'
#' @usage getBrainStarsSearch(query)
#' @param query keyword
#' @return A character vector of Search API response in JSON.
#' @details Search API is for keyword search and is based on Tokyo Manifesto and
#' TogoWS REST interface. Keyword for retrieving a list of hit entries:
#' (query+string)[/(offset),(limit)].
#'
#'   output:
#'   If the result has at least one hit entries, a list of entry IDs is returned
#'   in RJSONIN format. If not, 404 Not found error code is returned. "offset,limit"
#'   can be used to retrieve a part of hit entries. If "offset,limit" is not given,
#'   all hits are returned.
#'
#'   Keyword for retrieving the count of hit entries:
#'  (query+string)/count.
#'
#'    output: The count of hit entries is returned in RJSONIO format
#' @export
#' @examples
#' my.search            <- getBrainStarsSearch("receptor")
#' my.search.offset.num <- getBrainStarsSearch("receptor/1,5")
#' my.search.count      <- getBrainStarsSearch("receptor/count")
getBrainStarsSearch <- function(query) {
  api.name <- "search"
  res <- getBrainStars(query = query, type = api.name)
  return(res)
}
