### FIXME: want to get out active binding function, but this is not possible
### We could store the functions separately... not sure.. optimize later
### TODO: register listeners to forward changes
### TODO: register listener to catch column deletion
### NOTE: not clear how many cases this should handle.. row subsetting too much?

#' Generate binding for proxies.
#'
#' @param x mutaframe to inherit from
#' @param j columns to generate bindings for
.proxyBinding <- function(mf, j = names(mf)) {
  binder <- function(sym) {
    function(v) {
      if (missing(v)) {
        get(sym, mf)
      } else {
        notify_listeners(mf, sym, which[v != get(sym, mf)])
        assign(sym, v, mf)
      } 
    }
  }
  names(j) <- j
  lapply(j, binder)
}

#' @param i rows to filter
#' @returns named list of binding functions
.filterBinding <- function(mf, j = names(mf), i) {
  binder <- function(sym) {
    function(v) {
      if (is.character(i))
        i <- pmatch(i, rownames(mf), duplicates.ok = TRUE)
      xval <- get(sym, mf)[i]
      if (missing(v))
        xval
      else {
        notify_listeners(mf, sym, i[xval != v])
        xval[i] <- v
        assign(sym, xval, mf)
      }
    }
  }    
  names(j) <- j
  lapply(j, binder)
}

#' Generate binding for raw values
#' 
#' @param data list of values
#' @returns named list of binding functions
.rawBinding <- function(mf, data) {
  binder <- function(sym) {
    function(v) {
      if (missing(v)) {
        data[[sym]]
      } else {
        notify_listeners(mf, sym, which(data[[sym]] != v))
        data[[sym]] <<- v
      }      
    }
  }
  lapply(names(data), binder)
}