#' Retrieve Neurovault Collections ID
#'
#' @param doi Digital Object Identifier (DOI) for the project or collection
#' @param owner owner of the collection
#' @param name name of the collection
#' @param verbose print diagnostic messages
#' @param ... additional options to pass to \code{\link{nv_collection}}
#'
#' @return An ID or \code{NULL} if not found
#' @export
#'
#' @examples
#' id = nv_collection_id(name = "FeatureX IAPS Test")
nv_collection_id = function(
  doi = NULL,
  owner = NULL,
  name = NULL,
  verbose = TRUE,
  ...) {

  res = nv_collection(
    doi = doi, owner = owner, name = name,
    verbose = verbose, ...)
  if (res$content$count > 0) {
    id = res$content$results[[1]]$id
  } else {
    if (verbose) {
      warning("ID Not found")
    }
    id = NULL
  }
  return(id)
}

