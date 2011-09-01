### A mutable list
### Assumption: at least initially, no support for dynamic/pipeline behavior

### Two names attributes:
## envNames: names of symbols in the environment, storing order
## userNames: like 'names' attribute on a list (NULL, duplicates, NAs)
### If user specifies a qualifying vector of names, they are stored in both.
### Otherwise, we fix their names for storing in the internal envNames.

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Constructor.
###

##' The mutalist is a mutable list. Modifications to a mutalist occur
##' by a reference semantic. Otherwise, it should act like an ordinary
##' R list and provides a similar API. If anything is found missing,
##' please inform the authors.
##'
##' @title mutalist
##' @param ... elements to include in the list
##' @return a new mutalist
##' @author Michael Lawrence
##' @rdname mutalist
##' @export
##' @exportClass mutalist
mutalist <- function(...) {
  ml <- new.env(parent = emptyenv())
  class(ml) <- c("mutalist", class(ml))
  .list2mutalist(list(...), ml)
}

## Different from list2env in that we always clear
.list2mutalist <- function(x, ml) {
  rm(list = ls(ml), envir = ml)
  nms <- names(x)
  if (is.null(nms))
    names(x) <- paste("X", seq(length(x)), sep = "")
  if (any(is.na(nms)))
    names(x) <- make.names(nms, TRUE)
  else if (anyDuplicated(nms))
    names(x) <- make.unique(nms)
  list2env(x, ml)
  .envNames(ml) <- names(x)
  .userNames(ml) <- nms
  ml
}

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Accessors.
###


##' @rdname mutalist
##' @param x a mutalist
##' @S3method length mutalist
length.mutalist <- function(x) {
  length(.envNames(x))
}

##' @rdname mutalist
##' @param ... arguments passed to methods
##' @param value replacement value
##' @S3method names<- mutalist
`names<-.mutalist` <- function(x, ..., value) {
  if (!is.null(value)) {
    value <- as.character(value)
    if (length(value) != length(x))
      stop("'names' [", length(value), "] must equal 'length(x)' [",
           length(x), "]")
  }
  l <- as.list(x)
  names(l) <- value
  .list2mutalist(l, x) # simplest thing to do
}

##' @rdname mutalist
##' @S3method names mutalist
names.mutalist <- function(x) .userNames(x)

## internal name accessors

.envNames <- function(x) attr(x, "envNames")
`.envNames<-` <- function(x, value) {
  attr(x, "envNames") <- value
  x
}

.userNames <- function(x) attr(x, "userNames")
`.userNames<-` <- function(x, value) {
  attr(x, "userNames") <- value
  x
}

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Subsetting.
###

##' @rdname mutalist
##' @param i element indices
##' @param j unused
##' @S3method [[ mutalist
`[[.mutalist` <- function(x, i, j, ...) {
  dotArgs <- list(...)
  if (length(dotArgs) > 0)
    dotArgs <- dotArgs[names(dotArgs) != "exact"]
  if (missing(i) || !missing(j) || length(dotArgs) > 0)
    stop("incorrect number of subscripts")
  if (is.logical(i))
    i <- which(i)
  else if (!is.character(i) && !is.numeric(i))
    stop("invalid subscript type '", class(i), "'")
  if (length(i) < 1L)
    stop("attempt to extract less than one element")
  if (length(i) > 1L)
    stop("attempt to extract more than one element")
  if (is.na(i))
    stop("invalid subscript NA")
  if (is.numeric(i)) {
    if (!is.integer(i))
      i <- as.integer(i)
    if (i < 1L || length(x) < i)
      stop("subscript out of bounds")
  } else {
    ## 'i' is a character string
    names_x <- names(x)
    m <- match(i, names_x)
    if (is.na(m))
      return(NULL)
    i <- m
  }
  get(.envNames(x)[i], x)
}

##' @rdname mutalist
##' @S3method [[<- mutalist
`[[<-.mutalist` <- function(x, i, j, ..., value) {
  if (!missing(j) || length(list(...)) > 0)
    stop("invalid replacement")
  if (is.logical(i))
    i <- which(i)
  else if (!is.character(i) && !is.numeric(i))
    stop("invalid subscript type '", class(i), "'")
  if (length(i) < 1L)
    stop("attempt to extract less than one element")
  if (length(i) > 1L)
    stop("attempt to extract more than one element")
  if (is.na(i))
    stop("invalid subscript NA")
  if (is.numeric(i)) {
    if (!is.integer(i))
      i <- as.integer(i)
    ## FIXME: support filling NULLs when length(x) < i
    if (i < 1L || length(x) < i)
      stop("subscript out of bounds")
  } else {
    ## 'i' is a character string
    names_x <- names(x)
    m <- match(i, names_x)
    if (is.na(m)) {
      ## we know the name does not exist in our environment
      .envNames(x) <- c(.envNames(x), i)
      .userNames(x) <- c(.userNames(x), i)
    }
    i <- m
  }
  assign(.envNames(x)[i], value, x)
  x
}

##' @rdname mutalist
##' @param name element name
##' @S3method $<- mutalist
`$<-.mutalist` <- function(x, name, value) {
  x[[name]] <- value
  x
}

##' @rdname mutalist
##' @param drop unused
##' @S3method [ mutalist
`[.mutalist` <- function(x, i, j, ..., drop) {
### Supported 'i' types: numeric, character, logical, NULL and missing.
  if (!missing(j) || length(list(...)) > 0)
    stop("invalid subsetting")
  l <- as.list(x)
  if (!missing(i)) {
    lx <- length(x)
    if (is.numeric(i)) {
      if (any(i < 0) && any(i > 0))
        stop("only 0's may be mixed with positive indices")
    }
    else if (!is.logical(i) && !is.character(i) && !is.factor(i) && !is.null(i))
      stop("invalid subscript type")
    l <- l[i]
  }
  do.call(mutalist, l)
}

##' @rdname mutalist
##' @S3method [<- mutalist
`[<-.mutalist` <- function(x, i, j, ..., value) {
  if (!missing(j) || length(list(...)) > 0)
    stop("invalid replacement")
  l <- as.list(x)
  if (!missing(i)) {
    lx <- length(x)
    if (is.numeric(i)) {
      if (any(i < 0) && any(i > 0))
        stop("only 0's may be mixed with positive indices")
    }
    else if (!is.logical(i) && !is.character(i) && !is.factor(i) && !is.null(i))
      stop("invalid subscript type")
    l[i] <- value
  } else l[] <- value
  .list2mutalist(l, x)
}

##' @rdname mutalist
##' @param n number of elements in subset
##' @S3method head mutalist
head.mutalist <- function(x, n = 6L, ...) {
  stopifnot(length(n) == 1L)
  if (n < 0L)
    n <- max(length(x) + n, 0L)
  else
    n <- min(n, length(x))
  x[seq(n)]
}

##' @rdname mutalist
##' @S3method tail mutalist
tail.mutalist <- function(x, n = 6L, ...) {
  stopifnot(length(n) == 1L)
  xlen <- length(x)
  if (n < 0L)
    n <- max(xlen + n, 0L)
  else
    n <- min(n, xlen)
  x[xlen - rev(seq(n)) + 1L]
}

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Combining and splitting.
###

##' @rdname mutalist
##' @param recursive whether to perform recursively
##' @S3method c mutalist
c.mutalist <- function(x, ..., recursive = FALSE) {
  do.call(mutalist, do.call(c, lapply(list(x, ...), as.list)))
}

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Looping.
###

### TESTME: do we get sapply for free? what about tapply? etc

##' @rdname mutalist
##' @param X a mutalist
##' @param FUN a function to apply over the elements
##' @S3method lapply mutalist
lapply.mutalist <- function(X, FUN, ...)
  lapply(as.list(X), FUN = FUN, ...)

### TODO: mapply?

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Coercion.
###

##' @rdname mutalist
##' @S3method as.list mutalist
as.list.mutalist <- function(x, ...) {
  l <- mget(.envNames(x), x) # does not duplicate like as.list.environment
  names(l) <- names(x)
  l
}

##' @rdname mutalist
##' @S3method as.data.frame mutalist
as.data.frame.mutalist <- function(x, ...){
    as.data.frame(as.list(x))
}

##' @rdname mutalist
##' @S3method unlist mutalist
##' @param use.names whether to preserve the names
unlist.mutalist <- function(x, recursive = TRUE, use.names = TRUE) {
  unlist(as.list(x), recursive, use.names)
}

##' @rdname mutalist
##' @param envir environment to populate
##' @param parent parent for new environment, if created
##' @param hash whether to hash the new environment
##' @param size initial size of hash table
##' @export
mutalist2env <- function(x, envir = new.env(hash, parent, size),
                         parent = parent.frame(), hash = FALSE, size = 29L)
{
  list2env(as.list(x), envir)
}

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Utilities.
###

##' @rdname mutalist
##' @S3method rev mutalist
rev.mutalist <- function(x) {
  if (length(x) == 0)
    x
  else
    x[length(x):1]
}

##' @rdname mutalist
##' @S3method rep mutalist
rep.mutalist <- function(x, ...)
  x[rep(seq_len(length(x)), ...)]

##' @rdname mutalist
##' @S3method print mutalist
print.mutalist <- function(x) {
  print(as.list(x))
}
