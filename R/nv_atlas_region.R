#' @title Neurovault Query Atlas Region
#'
#' @description Searches atlas for specified region and related synonyms.
#' If no matches are found, it will search the NIF ontology
#' recursively for children of the specified region that are
#' found in the atlas. If no matches are found,
#' it will search recursively for parents that are found in the atlas.
#' Returns the corresponding to the X, Y and Z coordinates,
#'  respectively, of the matching voxels. Coordinates are in mm in MNI space.
#'
#' @param region Region to search or query on
#' @param collection_name Collection name of the atlas
#' @param atlas_name Name of the atlas
#' @param secure passed to \code{\link{nv_base_url}} for https
#' @param verbose print diagnostic messages
#' @param ... additional options to pass to \code{\link{GET}}
#'
#'
#' @return List of the result of the \code{\link{GET}} call and
#' the content
#' @export
#'
#' @examples
#' res = nv_atlas_region(
#' region = "middle frontal gyrus",
#' collection_name = "Harvard-Oxford cortical and subcortical structural atlases",
#' atlas_name = "HarvardOxford cort maxprob thr25 1mm")
#' head(res$content$voxels)
nv_atlas_region = function(
  region,
  collection_name,
  atlas_name,
  verbose = TRUE,
  secure = TRUE,
  ...) {

  url = nv_base_url(secure = secure)
  path = "/atlases/atlas_query_region/"
  url = paste0(url, path)

  query = list()
  query$region = region
  query$collection = collection_name
  query$atlas = atlas_name

  query = lapply(query, as.character)

  L = get_results(
    url, query = query,
    verbose = verbose, ...)

  L$content = append_results(
    content = L$content,
    verbose = verbose,
    query = query,
    ...)

  vox = L$content$voxels
  vox = cbind(x = vox[[1]], y = vox[[2]], z = vox[[3]])
  L$content$voxels = vox
  return(L)
}

