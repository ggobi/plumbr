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