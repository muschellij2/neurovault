#' @title NIDM Results
#'
#' @description Returns a list of dictionaries with information
#' corresponding to each nidm_results stored in NeuroVault.
#'
#' @param secure passed to \code{\link{nv_base_url}} for https
#' @param verbose print diagnostic messages
#' @param max_count Maximum count of records to call
#' @param ... additional options to pass to \code{\link{GET}}
#'
#' @return List of the result of the \code{\link{GET}} call and
#' the content
#' @export
#'
#' @examples \dontrun{
#' images = nv_nidm_results()
#' }
nv_nidm_results = function(
  verbose = TRUE,
  secure = TRUE,
  max_count = Inf,
  ...) {


  url = nv_base_url(secure = secure)
  path = "/nidm_results"
  path = path
  url = paste0(url, path)
  query = list()


  L = get_results(
    url, query = query,
    verbose = verbose, ...)
  L$content = append_results(
    content = L$content,
    verbose = verbose,
    query = query, ...)

  return(L)
}
