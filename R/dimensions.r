
nrow.pframe <- function(x) length(attr(x, "row.names")) %||% 0

ncol.pframe <- function(x) length(attr(x, "col.names")) %||% 0
dim.pframe <- function(x) c(nrow.pframe(x), ncol.pframe(x))
