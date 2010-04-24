
names.pframe <- function(x, ...) attr(x, "col.names")
dimnames.pframe <- function(x, ...) {
  list(attr(x, "row.names"), attr(x, "col.names"))
}

`names<-.pframe` <- function(x, value, ...) {
  attr(x, "col.names") <- value
  x
}

`dimnames<-.pframe` <- function(x, value, ...) {
  attr(x, "row.names") <- value[[1]]
  attr(x, "col.names") <- value[[2]]
  x
}