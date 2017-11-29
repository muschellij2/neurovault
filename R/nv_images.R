#' Neurovault Images
#'
#' @param id id of the collection
#' @param secure passed to \code{\link{nv_base_url}} for https
#' @param verbose print diagnostic messages
#' @param limit Limit of number of images to return
#' (only relevant if \code{id = NULL})
#' @param offset where to start (only relevant if \code{id = NULL})
#' @param ... additional options to pass to \code{\link{GET}}
#'
#' @note See \url{https://neurovault.org/api-docs}
#'
#' @return List of the result of the \code{\link{GET}} call and
#' the content
#' @export
#'
#' @importFrom httr GET
#' @examples
#' images = nv_images(id = 8719)
#' # same as nv_all_images, but won't loop over all cases
#' images = nv_images(limit = 10, offset = 0)
nv_images = function(
  id = NULL,
  verbose = TRUE,
  secure = TRUE,
  limit = 100,
  offset = 0,
  ...) {

  url = nv_base_url(secure = secure)
  path = "/images"
  path = paste0(path, "/", id)
  url = paste0(url, path)


  query = list()
  if (is.null(id)) {
    query$limit = limit
    query$offset = offset
  }

  L = get_results(
    url, query = query,
    verbose = verbose, ...)

  return(L)
}

