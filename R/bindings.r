### FIXME: want to get out active binding function, but this is not possible
### We could store the functions separately... not sure.. optimize later
### TODO: register listeners to forward changes
### TODO: register listener to catch column deletion
### NOTE: not clear how many cases this should handle.. row subsetting too much?

#' Generate binding for proxies.
#'
#' @param x mutaframe to inherit from
#' @param j columns to generate bindings for
proxy_bindings <- function(mf, j = names(mf)) {
  binder <- function(sym) {
    function(v) {
      if (missing(v)) {
        get(sym, mf)
      } else {
        assign(sym, v, mf)
      } 
    }
  }
  names(j) <- j
  lapply(j, binder)
}

#' @param i rows to filter
#' @returns named list of binding functions
filter_bindings <- function(mf, j = names(mf), i) {
  binder <- function(sym) {
    function(v) {
      if (is.character(i))
        i <- pmatch(i, rownames(mf), duplicates.ok = TRUE)
      xval <- get(sym, mf)
      if (missing(v))
        xval[i]
      else {
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
raw_bindings <- function(mf, data) {
  binder <- function(sym) {
    function(new) {
      old <- data[[sym]]
      if (missing(new)) {
        old
      } else {
        notify_listeners(mf, which(old != new), sym)
        data[[sym]] <<- new
      }      
    }
  }
  lapply(names(data), binder)
}