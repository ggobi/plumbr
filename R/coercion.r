as.mutaframe <- function(x, ...) UseMethod("as.mutaframe")
as.mutaframe.mutaframe <- function(x) {
  cl <- oldClass(x)
  i <- match("mutaframe", cl)
  if (i > 1L) 
    class(x) <- cl[-(1L:(i - 1L))]
  x
}
as.mutaframe.data.frame <- function(x) .mutaframe(x, rownames(x))
as.mutaframe.default <- function(x) as.mutaframe(as.data.frame(x))

as.data.frame.mutaframe <- function(x, row.names=NULL, optional=FALSE, ...) {
  cols <- lapply(names(x), function(j) x[[j]])
  names(cols) <- names(x)
  as.data.frame(cols)
}

is.mutaframe <- function(x) inherits(x, "mutaframe")