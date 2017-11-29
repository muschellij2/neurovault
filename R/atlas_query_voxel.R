#' @title Neurovault Atlas Query Voxel
#'
#' @description Searches atlas and returns
#' the region name that matches the specified coordinates
#' in the specified atlas.
#' Coordinates are in mm in MNI space.
#'
#' @param x x-coordinate to query
#' @param y y-coordinate to query
#' @param z z-coordinate to query
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
#' res = nv_atlas_voxel(x = 30, y= 30, z = 30,
#' collection_name = "Harvard-Oxford cortical and subcortical structural atlases",
#' atlas_name = "HarvardOxford cort maxprob thr25 1mm")
#' head(res$content$voxels)
nv_atlas_voxel = function(
  x,
  y,
  z,
  collection_name,
  atlas_name,
  verbose = TRUE,
  secure = TRUE,
  ...) {

  url = nv_base_url(secure = secure)
  path = "/atlases/atlas_query_voxel/"
  url = paste0(url, path)

  query = list()
  query$x = x
  query$y = y
  query$z = z
  query$collection = collection_name
  query$atlas = atlas_name

  query = lapply(query, as.character)

  L = get_results(
    url, query = query,
    verbose = verbose, ...)
  L$content = list(region = L$content)

  L$content = append_results(
    content = L$content,
    verbose = verbose,
    query = query,
    ...)

  return(L)
}
