as.mutaframe <- function(x, ...) UseMethod("as.mutaframe")
as.mutaframe.mutaframe <- function(x) {
  cl <- oldClass(x)
  i <- match("mutaframe", cl)
  if (i > 1L) 
    class(x) <- cl[-(1L:(i - 1L))]
  x
}
as.mutaframe.data.frame <- function(x) .mutaframe(x, rownames(x))
as.mutaframe.default <- function(x, ...) as.mutaframe(as.data.frame(x, ...))

as.data.frame.mutaframe <- function(x, row.names=rownames(x), optional=FALSE, ...) {
  cols <- lapply(names(x), function(j) x[[j]])
  names(cols) <- names(x)
  df <- as.data.frame(cols, optional = optional, ...)
  ## we set row.names this way for cases where we have no columns, but >0 rows
  ## as.data.frame complains in that case
  attr(df, "row.names") <- row.names
  df
}

is.mutaframe <- function(x) inherits(x, "mutaframe")

## as.list.environment does not resolve active bindings
as.list.mutaframe <- function(x) lapply(x, do.call, list())
