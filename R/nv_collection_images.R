#' Neurovault Collections Images
#'
#' @param id id of the collection
#' @param secure passed to \code{\link{nv_base_url}} for https
#' @param verbose print diagnostic messages
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
#' res = nv_collection_images(id = 77)
#' df = results_to_df(res$content$results)
#' \dontrun{
#' id = nv_collection_id(name = "FeatureX IAPS Test")
#' imgs = nv_collection_images(id = id)
#' }
nv_collection_images = function(
  id,
  verbose = TRUE,
  secure = TRUE,
  ...) {

  url = nv_base_url(secure = secure)
  path = "/collections"
  path = paste0(path, "/", id, "/", "images")
  url = paste0(url, path)
  query = list()

  gr = get_results(url, query = query,
                   verbose = verbose, ...)
  res = gr$response
  cr = gr$content
  rm(list = "gr")

  cr = append_results(content = cr, verbose = verbose,
                      query = query, ...)

  L = list(
    response = res,
    content = cr)
  return(L)
}


