#' Convert Neurovault results output to a \code{data.frame}
#'
#' @param results Results list from the content of a \code{nv} command
#'
#' @return A \code{data.frame} of the output
#' @export
#'
#' @examples
#' res = nv_collection(doi = "10.1016/j.neurobiolaging.2012.11.002")
#' results = res$content$results
#' df = results_to_df(results)
results_to_df = function(results) {
  nonull = function(x) {
    if (is.null(x) || length(x) == 0) {
      x = NA
    }
    return(x)
  }
  df = lapply(results, function(x) {
    x = lapply(x, nonull)
    if ("statmaps" %in% names(x)) {
      x$statmaps = lapply(x$statmaps, function(r) {
        r = lapply(r, nonull)
        r = as.data.frame(r, stringsAsFactors = FALSE)
      })
    }
    x = as.data.frame(x, stringsAsFactors = FALSE)
  })
  df = bind_list(df)
  return(df)
}
