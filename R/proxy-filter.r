#' Proxy: filtering
filter_proxy <- function(mf, i = NULL, j = NULL, rn) {
  if (is.null(i)) {
    varlist <- proxy_bindings(mf, j)
  } else {
    varlist <- filter_bindings(mf, j, i)    
  }

  .mutaframe(varlist, rn)
}

