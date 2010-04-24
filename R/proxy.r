### FIXME: want to get out active binding function, but this is not possible
### We could store the functions separately... not sure.. optimize later
### TODO: register listeners to forward changes
### TODO: register listener to catch column deletion
### NOTE: not clear how many cases this should handle.. row subsetting too much?
.proxyVars <- function(x, j, i = NULL) {
  .noSubset <- function(sym) {
    function(v) {
      if (missing(v))
        get(sym, x)
      else assign(sym, v, x)
    }
  }
  .rowSubset <- function(sym) {
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
  lapply(j, if (is.null(i)) .noSubset else .rowSubset)
}