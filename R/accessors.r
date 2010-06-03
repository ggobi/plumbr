## extraction

# Any operation that would return a 

"$.mutaframe" <- function(x, name) {
  x[[name, exact=FALSE]]
}

"[[.mutaframe" <- function(x, i, j, ...) {
  dotArgs <- list(...)
  if (length(dotArgs) > 0)
    dotArgs <- dotArgs[names(dotArgs) != "exact"]
  if (!missing(j) || length(dotArgs) > 0)
    stop("invalid subsetting")
  if (missing(i))
    stop("subscript is missing")
  if (!is.character(i) && !is.numeric(i))
    stop("invalid subscript type")
  if (length(i) < 1L)
    stop("attempt to select less than one element")
  if (length(i) > 1L)
    stop("attempt to select more than one element")
  if (!is.character(i) && !is.na(i) && (i < 1L || i > length(x)))
    stop("subscript out of bounds")
  if (is.character(i) && identical(dotArgs$exact, FALSE))
    i <- pmatch(i, names(x))
  if (is.numeric(i))
    i <- names(x)[i]
  get(i, x, inherits=FALSE)
}

"$<-.mutaframe" <- function(x, name, value) {
  x[[name]] <- value
  x
}

### TODO: emit events
"[[<-.mutaframe" <- function(x, i, j,..., value) {
  nrx <- nrow(x)
  lv <- length(value)
  if (!missing(j) || length(list(...)) > 0)
    warning("arguments beyond 'i' ignored")
  if (missing(i))
    stop("subscript is missing")
  if (!is.character(i) && !is.numeric(i))
    stop("invalid subscript type")
  if (length(i) < 1L)
    stop("attempt to select less than one element")
  if (length(i) > 1L)
    stop("attempt to select more than one element")
  if (is.numeric(i) && (i < 1L || i > ncol(x) + 1L))
    stop("subscript out of bounds")
  if (!is.null(value) && (nrx != lv)) {
    if ((nrx == 0) || (nrx %% lv != 0))
      stop(paste(lv, "elements in value to replace", nrx, "elements"))
    else value <- rep(value, length.out = nrx)
  }
  if (is.numeric(i)) {
    if (i > length(x))
      i <- paste("V", i, sep = "")
    else i <- names(x)[i]
  }
  if (!exists(i, x)) {
    ## ensure unique, valid names    
    nms <- make.names(c(names(x), i), unique=TRUE)
    names(x) <- nms
    i <- tail(nms, 1L)
  }
  assign(i, value, x)
  x
}

"[.mutaframe" <- function(x, i, j, ..., drop) {
  if (length(list(...)) > 0)
    warning("parameters in '...' not supported")
  
  ## no ',' -- forward to list
  ## NOTE: matrix-style subsetting by logical matrix not supported
  if ((nargs() - !missing(drop)) < 3) { 
    if (!missing(drop))
      warning("parameter 'drop' ignored by list-style subsetting")
    if (missing(i))
      return(x)
    iInfo <- .bracket.Index(i, ncol(x), colnames(x))
    if (!is.null(iInfo[["msg"]]))
      stop("subsetting as list: ", iInfo[["msg"]])
    varlist <- .proxyVars(x, iInfo[["idx"]])
    if (anyDuplicated(names(varlist)))
      names(varlist) <- make.unique(names(varlist))
    return(.mutaframe(varlist, rownames(x)))
  }

### NOTE: the indexing into columns is static, so negative column
### indices will not allow new columns to propagate
  
  dim <- dim(x)
  rn <- rownames(x)
  if (!missing(j)) {
    jInfo <- .bracket.Index(j, ncol(x), colnames(x))
    if (!is.null(jInfo[["msg"]]))
      stop("selecting cols: ", jInfo[["msg"]])
    j <- jInfo[["idx"]]
    dim[2L] <- length(j)
  } else j <- names(x)
  
  if (!missing(i)) {
    iInfo <- .bracket.Index(i, nrow(x), rownames(x), dup.nms = TRUE,
                            allowNumeric = TRUE)
    if (!is.null(iInfo[["msg"]]))
      stop("selecting rows: ", iInfo[["msg"]])
    i <- iInfo[["idx"]]  
    dim[1L] <- length(seq(dim[1L])[i]) # may have 0 cols, no rownames
    rn <- rn[i]
    if (anyDuplicated(rn))
      rn <- make.unique(rn)
  } else {
    i <- TRUE
  }

  
  if (missing(drop)) ## drop by default if only one column left
    drop <- length(dim[2L]) == 1
    
  if (dim[2L] == 1 && drop) {
    # Single column output, and want to drop, so return static clone
    x[[j]][i]
  } else {
    # Otherwise return proxy
    varlist <- .proxyVars(x, j, i)
    if (anyDuplicated(names(varlist)))
      names(varlist) <- make.unique(names(varlist))
    .mutaframe(varlist, rn)
  }
}

### TODO: emit events
"[<-.mutaframe" <- function(x, i, j, ..., value) {
  if (length(list(...)) > 0)
    warning("parameters in '...' not supported")
  
  if (nargs() < 4) {
    iInfo <- list(msg = NULL, useIdx = FALSE, idx = NULL)
    if (missing(i)) {
      jInfo <-
        list(msg = NULL, useIdx = FALSE, idx = seq_len(ncol(x)))
    } else {
      jInfo <- .bracket.Index(i, ncol(x), colnames(x))
    }
  } else {
    if (missing(i)) {
      iInfo <- list(msg = NULL, useIdx = FALSE, idx = NULL)
    } else {
      iInfo <-
        .bracket.Index(i, nrow(x), rownames(x), allowNumeric=TRUE)
    }
    if (missing(j)) {
      jInfo <-
        list(msg = NULL, useIdx = FALSE, idx = seq_len(ncol(x)))
    } else {
      jInfo <- .bracket.Index(j, ncol(x), colnames(x))
    }
  }
  if (!is.null(iInfo[["msg"]]))
    stop("replacing rows: ", iInfo[["msg"]])
  if (!is.null(jInfo[["msg"]]))
    stop("replacing cols: ", jInfo[["msg"]])
  i <- iInfo[["idx"]]
  j <- jInfo[["idx"]]
  useI <- iInfo[["useIdx"]]
  if (!is(value, "mutaframe")) {
    if (useI)
      li <- length(i)
    else
      li <- nrow(x)
    lv <- length(value)
    if (li != lv) {
      if ((li == 0) || (li %% lv != 0))
        stop(paste(lv, "rows in value to replace",
                   li, "rows"))
      else
        value <- rep(value, length.out = li)
    }
    if (useI) {
      vals <- mget(j, x)
      for (ji in j) {
        y <- vals[[ji]]
        y[i] <- value
        assign(ji, y, x)
      }
    } else {
      for (ji in j)
        assign(ji, value, x)
    }
  } else {
    if (ncol(value) != length(j))
      stop("ncol(x[j]) != ncol(value)")
    if (useI)
      li <- length(i)
    else
      li <- nrow(x)
    nrv <- nrow(value)
    if (li != nrv) {
      if ((li == 0) || (li %% nrv != 0))
        stop(paste(nrv, "rows in value to replace",
                   li, "rows"))
      else
        value <-
          value[rep(seq_len(nrv), length.out = li), ,
                drop=FALSE]
    }
    if (useI) {
      vals <- mget(j, x)      
      for (ji in j) {
        y <- vals[[ji]]
        y[i] <- value[[ji]]
        assign(ji, y, x)
      }
    } else {
      for (ji in j)
        assign(ji, value[[ji]], x)
    }
  }
  x
}

anyMissingOrOutside <- function(x, lower = -.Machine$integer.max,
                                upper = .Machine$integer.max)
{
  any(is.na(x) | x < lower | x > upper)
}

.bracket.Index <-
  function(idx, lx, nms = NULL, dup.nms = FALSE, allowNumeric = FALSE)
{
  msg <- NULL
  if (is.numeric(idx)) {
    if (!is.integer(idx))
      idx <- as.integer(idx)
    if (anyMissingOrOutside(idx, -lx, lx)) {
      msg <- "subscript contains NAs or out of bounds indices"
    } else {
      anyPos <- anyMissingOrOutside(idx, -lx, 0L)
      anyNeg <- anyMissingOrOutside(idx, 0L, lx)
      if (anyPos && anyNeg)
        msg <- "negative and positive indices cannot be mixed"
    }
  } else if (is.logical(idx)) {
    if (anyMissing(idx))
      msg <- "subscript contains NAs"
    else if (length(idx) > lx)
      msg <- "subscript out of bounds"
  } else if (is.character(idx) || is.factor(idx)) {
    if (anyMissing(idx))
      msg <- "subscript contains NAs"
    else if (is.null(nms) && length(idx) > 0)
      msg <- "cannot subset by character when names are NULL"
    else {
      if (dup.nms)
        m <- pmatch(idx, nms, duplicates.ok = TRUE)
      else
        m <- match(idx, nms)
      if (!dup.nms && anyMissing(m))
        msg <- "mismatching names"
    }
  } else if (!is.null(idx)) {
    msg <- "invalid subscript type"
  }
  if (!is.null(msg)) {
    useIdx <- NULL
    idx <- NULL
  } else {
    useIdx <- TRUE
    if (!is.character(idx)) {
      if (allowNumeric) {
        if (is.logical(idx)) {
          if (all(idx))
            useIdx <- FALSE
          if (length(idx) < lx)
            idx <- rep(idx, length.out = lx)
          idx <- which(idx)
        }
      } else idx <- nms[idx]
    }
  }
  
  list(msg = msg, useIdx = useIdx, idx = idx)
}