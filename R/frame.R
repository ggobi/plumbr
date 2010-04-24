## Ideas for mutable, dynamic frames

## Mutability achieved by storing columns in environments
## Dynamic frames achieved through active bindings in environment

## All sorts of cool things can be built on this, like proxies.

## Methods:
##
## Extraction: [[, [, $
## Sub-assignment: [[<-, $<-, [<-
## Combination: cbind, rbind
## Accessors: dimnames, dim
## Subset: subset, head, tail, na.*
## Aggregation: aggregate, xtabs
## Transform: transform
## Apply: by, lapply
## Split: split
## Coercion: as.data.frame
## Summary: summary
## Duplicates: duplicated, anyDuplicated
## Display: print

## The methods that would return another frame could return a proxy,
## or a rooted frame. The methods [, c/rbind, subsetting, aggregate,
## and split fall into this category. Particularly with subsetting
## methods, one may want to delete the original frame, rather than
## proxy it. Passing 'drop = TRUE' to [, for example, could root the
## frame. We can add the 'drop' argument on to our other methods, like
## subset.data.frame does. There can be a function could be used to
## explictly root a proxy.

## For the subassign functions, we cannot create a new proxy, for
## reasons inherent in the language. But how do we decide if the
## modification happens at the proxy or the root? This is like <-
## vs. <<-. Given the mutable nature of the design, we should probably
## take the <<- route. Follow the parent links until the definition is
## found, and replace it. This requires a reverse pipeline.
## Transformations need to reverse transform (if possible) any
## assignment. We should probably just throw an error when reversal is
## impossible.

## Being an environment gives us with(), modeling functions, etc, for free

## The model could record the call that constructed it, which would
## allow displaying the workflow or "pipeline" behind it

## Would also be nice if something provided a Qt data model on top of it

## The next step is to figure out change notification. Handlers need
## to be registered, with two arguments, i and j, that index into the
## rows and columns, respectively. Any replacement function will
## necessarily modify the frame in-place, and the change event will be
## emitted. Parameters of dynamic columns can of course be changed
## through the use of closures, but the frame must be explicitly
## informed of the change.

## Every binding will need to be active, so that when a change is
## made, an event is reported. Event reporting will need to be frozen
## when many changes are about to be made. When frozen, the events are
## aggregated. When the thaw method is called, the aggegated event is
## dispatched. Each frame will need to listen to each of its
## parents/components and forward the events. Some stages may need to
## add more changes. For example, a subset() stage will check if its
## subsetting has changed and, if so, add all columns to the event. A
## transform() frame would report changes to any transformed columns,
## if not already marked.


## constructor
pframe <- function(...) {
  nr <- 0
  listData <- list(...)
  varlist <- vector("list", length(listData))
  if (length(listData) > 0) {
    dotnames <- names(listData)
    dotvalues <- 
      sapply(as.list(substitute(list(...)))[-1L],
             function(arg) deparse(arg)[1L])
    if (is.null(dotnames)) {
      emptynames <- rep.int(TRUE, length(listData))
      names(listData) <- dotvalues
    } else {
      emptynames <- !nzchar(dotnames)
      if (any(emptynames)) {
        names(listData)[emptynames] <- dotvalues[emptynames]
      }
    }
    varnames <- as.list(names(listData))
    nrows <- ncols <- integer(length(varnames))
    for (i in seq_along(listData)) {
      element <- try(as.pframe(listData[[i]]), silent = TRUE)
      if (inherits(element, "try-error"))
        stop("cannot coerce class \"", class(listData[[i]]),
             "\" to a DataFrame")
      nrows[i] <- nrow(element)
      ncols[i] <- ncol(element)
      varlist[[i]] <-
        if (is.environment(listData[[i]]))
          .proxyVars(element, names(element))
        else as.list(element)
      if ((length(dim(listData[[i]])) > 1) ||
          (ncol(element) > 1)) {
        if (emptynames[i])
          varnames[[i]] <- colnames(element)
        else
          varnames[[i]] <- paste(varnames[[i]], colnames(element), sep = ".")
      }
    }
    nr <- max(nrows)
    for (i in which((nrows > 0L) & (nrows < nr) & (nr %% nrows == 0L))) {
      recycle <- rep(seq_len(nrows[i]), length.out = nr)
      varlist[[i]] <- lapply(varlist[[i]], function(sym) {
        if (is.function(x))
          function(v) {
            if (!missing(v))
              .irreversible("replication")
            x()[recycle, drop=FALSE]
          }
        else x[recycle, drop=FALSE]
      })
      nrows[i] <- nr
    }
    if (!all(nrows == nr))
      stop("different row counts implied by arguments")
    varlist <- unlist(varlist, recursive = FALSE, use.names = FALSE)
    names(varlist) <- make.names(unlist(varnames[ncols > 0L]), unique = TRUE)
  }
  
  if (!is.null(row.names)) {
    if (anyMissing(row.names))
      stop("missing values in 'row.names'")
    if (length(varlist) && length(row.names) != nr)
      stop("invalid length of row names")
    if (anyDuplicated(row.names))
      stop("duplicate row names")
    row.names <- as.character(row.names)
  } else row.names <- as.character(seq(max(nr)))

  env <- .pframe(varlist, row.names)
  provenance(env) <- sys.call()
  env
}

.pframe <- function(varlist, row.names) {
  env <- new.env()
  class(env) <- c("pframe", class(env))
  mapply(function(name, value) {
    if (is.function(value))
      makeActiveBinding(name, value, env)
    else assign(name, value, env)
  }, names(varlist), varlist)
  ## we have the names in the 'env', but this keeps their order
  colnames(env) <- names(varlist)
  rownames(env) <- row.names
  env
}

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

## extraction

"$.pframe" <- function(x, name) {
  x[[name, exact=FALSE]]
}

"[[.pframe" <- function(x, i, j, ...) {
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

"$<-.pframe" <- function(x, name, value) {
  x[[name]] <- value
  x
}

### TODO: emit events
"[[<-.pframe" <- function(x, i, j,..., value) {
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

"[.pframe" <- function(x, i, j, ..., drop) {
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
    return(.pframe(varlist, rownames(x)))
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
  } else i <- NULL

  varlist <- .proxyVars(x, j, i)
  if (anyDuplicated(names(varlist)))
    names(varlist) <- make.unique(names(varlist))
  x <- .pframe(varlist, rn)
  
  if (missing(drop)) ## drop by default if only one column left
    drop <- dim[2L] == 1
  if (drop) {
    ## one column left
    if (dim[2L] == 1) 
      return(x[[1L]])
    ## one row left
    if (dim[1L] == 1)
      return(as(x, "list"))
  }
  
  x
}

### TODO: emit events
"[<-.pframe" <- function(x, i, j, ..., value) {
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
  if (!is(value, "pframe")) {
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

## coercion

as.pframe <- function(x, ...) UseMethod("as.pframe")
as.pframe.pframe <- function(x) {
  cl <- oldClass(x)
  i <- match("pframe", cl)
  if (i > 1L) 
    class(x) <- cl[-(1L:(i - 1L))]
  x
}
as.pframe.data.frame <- function(x) .pframe(x, rownames(x))
as.pframe.default <- function(x) as.pframe(as.data.frame(x))

as.data.frame.pframe <- function(x, row.names=NULL, optional=FALSE, ...) {
}

## utils

.irreversible <- function(reason)
  stop("Reversal impossible due to ", reason, ".")
