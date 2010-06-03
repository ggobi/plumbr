
nrow.mutaframe <- function(x) length(attr(x, "row.names")) %||% 0

ncol.mutaframe <- function(x) length(attr(x, "col.names")) %||% 0
dim.mutaframe <- function(x) c(nrow.mutaframe(x), ncol.mutaframe(x))
