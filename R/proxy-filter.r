#' Proxy: filtering
filter_proxy <- function(mf, i = NULL, j = NULL, rn) {
  if (is.null(i)) {
    varlist <- proxy_bindings(mf, j)
  } else {
    varlist <- filter_bindings(mf, j, i)    
  }

  if (anyDuplicated(names(varlist)))
    names(varlist) <- make.unique(names(varlist))
  .mutaframe(varlist, rn)
}

