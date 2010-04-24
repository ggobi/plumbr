

## constructor
pframe <- function(...) {
  listData <- list(...)

  # If no data, return NULL pframe
  if (length(listData) == 0) return(.pframe())

  nr <- 0
  varlist <- vector("list", length(listData))

  # Work out names
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

.pframe <- function(varlist = list(), row.names = NULL) {
  env <- new.env()
  class(env) <- c("pframe", class(env))
  mapply(function(name, value) {
    if (is.function(value))
      makeActiveBinding(name, value, env)
    else assign(name, value, env)
  }, names(varlist), varlist)
  ## we have the names in the 'env', but this keeps their order
  attr(env, "col.names") <- names(varlist)
  attr(env, "row.names") <- row.names
  env
}

