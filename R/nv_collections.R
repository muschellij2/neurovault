#' @title Neurovault Collections
#'
#' @description Returns a  list of dictionaries with information corresponding
#' to each collection stored in NeuroVault.
#' Results can be filtered by specifying the name, DOI or owner of the collection.
#'
#' @param doi Digital Object Identifier (DOI) for the project or collection
#' @param owner owner of the collection
#' @param name name of the collection
#' @param id id of the collection
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
#' @importFrom httr GET stop_for_status warn_for_status
#' @examples
#' res = nv_collection(doi = "10.1016/j.neurobiolaging.2012.11.002")
#' res = nv_collection(name = "21 pain studies (NIDM-Results)")
#' res = nv_collection(id = 77)
#' res = nv_collection(id = 77, doi = "10.1016/j.neurobiolaging.2012.11.002")
#' res = nv_collection(max_count = 100)
#'
nv_collection = function(
  id = NULL,
  doi = NULL,
  owner = NULL,
  name = NULL,
  verbose = TRUE,
  secure = TRUE,
  max_count = Inf,
  ...) {

  query = list()
  query$DOI = doi
  query$owner = owner
  query$name = name

  null_zero = function(x) {
    is.null(x) || length(x) == 0
  }

  null_query = null_zero(name) && null_zero(doi) && null_zero(owner)
  null_id = null_zero(id)

  if (null_id && null_query) {
    message("No doi/owner/name/id have been specified - all collections!")
  }

  if (!null_id & !null_query) {
    warning("ID and doi/owner/name specified, ID will be used, others ignored")
    query = list()
  }

  url = nv_base_url(secure = secure)
  path = "/collections"
  if (!null_id & null_query) {
    path = paste0(path, "/", id)
  }
  url = paste0(url, path)

  query = lapply(query, as.character)

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
  # different output with collections/id
  if (!null_id & null_query) {
    cr = list(results = list(cr))
  }
  L = list(
    response = res,
    content = cr)
  return(L)
}


