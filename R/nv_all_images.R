#' All Neurovault Images
#'
#' @param secure passed to \code{\link{nv_base_url}} for https
#' @param verbose print diagnostic messages
#' @param max_count Maximum count of records to call, the number of records
#' may be larger than this based on how the limits are set for API calls
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
#' images = nv_all_images(max_count = 100)
#' df = results_to_df(images$content$results)
nv_all_images = function(
  verbose = TRUE,
  secure = TRUE,
  max_count = 300,
  ...) {

  url = nv_base_url(secure = secure)
  path = "/images"
  path = path
  url = paste0(url, path)
  query = list()


  L = get_results(
    url, query = query,
    verbose = verbose, ...)
  res = L$response
  cr = L$content

  cr = append_results(
    content = cr,
    verbose = verbose,
    query = query,
    max_count = max_count,
    ...)
  L = list(
    response = res,
    content = cr)
  return(L)
}

