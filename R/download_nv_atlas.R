#' @title Neurovault Atlases
#'
#' @description Returns a  list of dictionaries with information corresponding
#' to each atlas stored in NeuroVault.
#'
#' @param id id of the atlas
#' @param secure passed to \code{\link{nv_base_url}} for https
#' @param verbose print diagnostic messages
#' @param ... additional options to pass to \code{\link{GET}}
#'
#'
#' @return List of the result of the \code{\link{GET}} call and
#' the content
#' @export
#'
#' @importFrom httr GET stop_for_status warn_for_status
#' @examples
#' res = nv_atlas()
#' df = results_to_df(res$content$results)
nv_atlas = function(
  id = NULL,
  verbose = TRUE,
  secure = TRUE,
  ...) {

  url = nv_base_url(secure = secure)
  path = "/atlases"
  path = paste0(path, "/", id)
  url = paste0(url, path)

  query = list()

  L = get_results(
    url, query = query,
    verbose = verbose, ...)

  L$content = append_results(
    content = L$content,
    verbose = verbose,
    query = query,
    ...)
  if (!is.null(id)) {
    L$content = list(results = list(L$content))
  }
  return(L)
}

#' @rdname nv_atlas
#' @export
#' @examples
#' nv_atlas_names()
nv_atlas_names = function(...) {
  res = nv_atlas(id = NULL, ...)
  df = results_to_df(res$content$results)
  return(df$name)
}

#' @rdname nv_atlas
#' @export
#' @examples
#' nv_atlas_df()
nv_atlas_df = function(verbose = TRUE, ...) {
  res = nv_atlas(id = NULL, ...)
  df = results_to_df(res$content$results)
  ucollect = unique(df$collection_id)
  con_info = lapply(ucollect, function(id) {
    res = nv_collection(id = id, verbose = FALSE, ...)
    df = results_to_df(res$content$results)
  })
  con_info = bind_list(con_info)
  cn = c("id", "description", "name")
  cn = intersect(cn, colnames(con_info))
  con_info = con_info[, cn, drop = FALSE]
  cn = colnames(con_info)
  cn = paste0("collection_", cn)
  colnames(con_info) = cn

  df = merge(df, con_info, by = "collection_id", all.x = TRUE)
  return(df)
}




#' Download Neurovault Atlas and Label file
#'
#' @param id id of the atlas
#' @param verbose print diagnostic messages
#' @param outdir output directory for images
#' @param overwrite Will only overwrite existing file if \code{TRUE}.
#' @param ... additional options to pass to \code{\link{GET}}
#'
#' @return A \code{data.frame} of the image information
#' and their output filenames
#' @export
#'
#' @examples
#' res = nv_atlas(id = 1408, verbose = TRUE)
#' df = results_to_df(res$content$results)
#' if (requireNamespace("curl", quietly = TRUE)) {
#' r = curl::curl_fetch_memory(df$file[1],
#' handle = curl::new_handle(verbose = TRUE))
#' }
#' res = download_nv_atlas(id = 1408)
#'
download_nv_atlas = function(
  id,
  verbose = TRUE,
  outdir = tempfile(),
  overwrite = TRUE,
  ...) {

  res = nv_atlas(id = id, verbose = verbose, ...)
  df = results_to_df(res$content$results)
  if (!dir.exists(outdir)) {
    dir.create(outdir, recursive = TRUE)
  }
  df$outfile = file.path(outdir, basename(df$file))
  dl_results = mapply(function(url, outfile) {
    image_res = GET(
      url,
      httr::write_disk(path = outfile, overwrite = overwrite),
      if (verbose) httr::progress())
    return(image_res)
  }, df$file, df$outfile, SIMPLIFY = FALSE)
  dl_results = lapply(dl_results, httr::warn_for_status)
  status_codes = sapply(dl_results, httr::status_code)
  df$dl_status_code = status_codes

  # downloading label description file
  df$label_description_outfile = file.path(
    outdir,
    basename(df$label_description_file))

  dl_lab_results = mapply(function(url, outfile) {
    image_res = GET(
      url,
      httr::write_disk(path = outfile, overwrite = overwrite),
      if (verbose) httr::progress())
    return(image_res)
  }, df$label_description_file,
  df$label_description_outfile,
  SIMPLIFY = FALSE)

  rm(dl_lab_results)
  return(df)
}
