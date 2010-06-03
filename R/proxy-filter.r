#' Proxy: filtering
filter_proxy <- function(mf, i = NULL, j = NULL) {       
  varlist <- .proxyVars(x, j, i)
  if (anyDuplicated(names(varlist)))
    names(varlist) <- make.unique(names(varlist))
  .mutaframe(varlist, rn)
}



### FIXME: want to get out active binding function, but this is not possible
### We could store the functions separately... not sure.. optimize later
### TODO: register listeners to forward changes
### TODO: register listener to catch column deletion
### NOTE: not clear how many cases this should handle.. row subsetting too much?
.proxyVars <- function(x, j, i = NULL) {
  .subset <- if (is.null(i)) {
    function(sym) {
      function(v) {
        if (missing(v)) {
          get(sym, x)
        } else {
          assign(sym, v, x)
        } 
      }
    }
  } else {
    function(sym) {
      function(v) {
        if (is.character(i))
          i <- pmatch(i, rownames(x), duplicates.ok = TRUE)
        xval <- get(sym, x)[i]
        if (missing(v))
          xval
        else {
          xval[i] <- v
          assign(sym, xval, x)
        }
      }
    }    
  }
  lapply(j, .subset)
}