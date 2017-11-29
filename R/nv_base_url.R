#' Neurovault Base URL
#'
#' @param secure Should https be used (may be necessary)
#'
#' @return Character vector (length 1) of URL
#' @export
#'
#' @examples
#' nv_base_url()
nv_base_url = function(secure = TRUE) {
  url = "http"
  if (secure) {
    url = paste0(url, "s")
  }
  url = paste0(url, "://neurovault.org/api")
  return(url)
}
