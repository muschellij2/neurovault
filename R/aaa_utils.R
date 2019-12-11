ensure_colnames = function(x, cn) {
  sd = setdiff(colnames(x), cn)
  for (isd in sd) {
    x[, isd] = NA
  }
  sd = setdiff(cn, colnames(x))
  for (isd in sd) {
    x[, isd] = NA
  }
  return(x)
}

bind_list = function(L) {
  cn = sapply(L, colnames)
  cn = unique(c(unlist(cn)))
  L = lapply(L, function(x){
    x = ensure_colnames(x, cn)
    x[, cn]
  })
  L = do.call("rbind", L)
  return(L)
}


get_results = function(
  url, query = list(),
  verbose = TRUE, nonstop = FALSE, ...) {
  res = httr::GET(url, query = query,
                  if (verbose > 1) httr::verbose(),
                  ...)
  if (verbose) {
    message("GET command is:")
    print(res)
  }
  if (!nonstop) {
    httr::stop_for_status(res)
  } else {
    httr::warn_for_status(res)
  }

  cr = httr::content(res)
  return(list(response = res, content = cr))
}

append_results = function(
  content,
  query = list(),
  verbose = TRUE, max_count = Inf, ...) {
  count = content$count
  n_res = length(content$results)
  if (is.null(count)) {
    count = n_res
  }
  if (count > n_res) {
    if (verbose) {
      msg = "Multiple pages must be called - more results than 1 call"
      message(msg)
    }
  }
  next_url = content$`next`
  while (!is.null(next_url) && n_res < max_count) {
    next_gr = get_results(
      url = next_url,
      query = query,
      verbose = verbose,
      nonstop = TRUE, ...)
    next_cr = next_gr$content
    content$results = c(content$results, next_cr$results)
    n_res = length(content$results)
    rm(list = "next_gr")
    next_url = next_cr$`next`
  }
  n_res = length(content$results)
  if (count > n_res && max_count > n_res) {
    warning("Not all records received, may be a problem with the call")
  }
  return(content)
}
