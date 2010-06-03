## constructor
mutaframe <- function(..., row.names = NULL) {
  listData <- list(...)

  # If no data, return NULL mutaframe
  if (length(listData) == 0) return(.mutaframe())

  # Work out names
  names(listData) <- variable_names(names(listData) %||% 
    rep(NA_character_, length(listData)))
  
  varnames <- as.list(names(listData))
  varlist <- vector("list", length(listData))
  nrows <- ncols <- integer(length(varnames))
  for (i in seq_along(listData)) {
    element <- try(as.mutaframe(listData[[i]]), silent = TRUE)
    if (inherits(element, "try-error"))
      stop("cannot coerce class \"", class(listData[[i]]),
           "\" to a DataFrame")
    nrows[i] <- nrow(element)
    ncols[i] <- ncol(element)
    varlist[[i]] <-
      if (is.environment(listData[[i]]))
        .proxyBinding(element, names(element))
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
    varlist[[i]] <- lapply(varlist[[i]], function(x) {
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
    if (any(is.na(row.names)))
      stop("missing values in 'row.names'")
    if (length(varlist) && length(row.names) != nr)
      stop("invalid length of row names")
    if (any(duplicated(row.names)))
      stop("duplicate row names")
    row.names <- as.character(row.names)
  } else row.names <- as.character(seq(max(nr)))


  env <- .mutaframe(varlist, row.names)
  # provenance(env) <- sys.call()
  env
}

#' Raw constructor.
#' Constructs a mutaframe without checking that variables are of the correct
#' type and length.
.mutaframe <- function(varlist = list(), row.names = NULL) {
  mf <- new.env(parent = emptyenv())
  
  # Ensure all atomic vectors converted to binding functions
  binders <- as.list(varlist)
  fun <- sapply(varlist, is.function)
  binders[!fun] <- .rawBinding(mf, binders[!fun])
  
  # Activate bindings
  for(name in names(binders)) {
    makeActiveBinding(name, binders[[name]], mf)
  }

  structure(mf,
    col.names = names(varlist), # keep variable order
    row.names = row.names,
    listeners = list(),
    class = c("mutaframe", class(mf))
  )
}

