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
