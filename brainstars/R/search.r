# Search API
#' Keyword search in BrainStars
#'
#' This function queries entry information from BrainStars
#' using by Search API.
#'
#' @usage getBrainStars(query)
#' @param query keyword
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
#' my.search            <- getBrainStars("receptor")
#' my.search.offset.num <- getBrainStars("receptor/1,5")
#' my.search.count      <- getBrainStars("receptor/count")
getBrainStars <- function(query) {
  api.name = "search"
  res = new("BrainStars", query=query, api.name=api.name)
  return(res@response)
}