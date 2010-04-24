
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


#' Make valid variable names
variable_names <- function(var_names) {
  no_name <- is.na(var_names) | nzchar(var_names) == 0
  var_names[no_name] <- paste("X", seq_len(sum(no_name)), sep = "")

  make.names(var_names, unique = T)
}