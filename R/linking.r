### Linking Routines ###

if (FALSE) { # please ignore my bad programming habits
  ## Example:
  ## Assume we have a dataset:
  data(Cars93, package="MASS")
  mf <- mutaframe(Cars93)
  mf$.color <- "gray"
  ## First step is to create a base selection
  sel <- ItemSelection()
  ## Now, link that selection to other cases in same dataset by some variable
  linked_sel <- sel$link(match_any_linker(Cars93["Manufacturer"]))
  ## Finally, scale that linked selection to the data
  linked_sel$scale(function(x, d) d[as.logical(x), ".color"] <- "red", mf)
  ## To test, select some cases
  cases <- rep(FALSE, nrow(Cars93))
  cases[seq(1, 10, 2)] <- TRUE
  sel$replace(cases)
}

duplex_data_linker <- function(delegate, from_data, to_data = from_data) {
  function(from_selection, new_selection) {
    if (!missing(new_selection))
      delegate(new_selection, to_data, from_data)
    else delegate(from_selection, from_data, to_data)
  }
}

generate_key <- function(data) {
  if (ncol(data) == 1L)
    data[[1]]
  else do.call(paste, c(as.list(data), sep = "\r"))
}

match_any_linker <- function(from_data, to_data = from_data)
{
  duplex_data_linker(function(from_selection, from_data, to_data) {
    from_logical <- as.logical(from_selection)
    generate_key(to_data) %in% generate_key(from_data)[from_logical]
  }, from_data, to_data)
}

## requires that all records in from_data that map to to_data are selected
match_all_linker <- function(from_data, to_data = from_data) {
  match_weight <- match_weight_linker(from_data, to_data)
  duplex_data_linker(function(from_selection, from_data, to_data) {
    match_weight(from_selection, from_data, to_data) == 1.0
  }, from_data, to_data)
}

## weights by selected prsoportion of records in to_data that map to from_data
match_weight_linker <- function(from_data, to_data = from_data) {
  duplex_data_linker(function(from_selection, from_data, to_data) {
    m <- match(generate_key(from_data), generate_key(to_data))
    tabulate(m[as.logical(from_selection)]) / tabulate(m)
  }, from_data, to_data)
}

## FANCIER STUFF IN PROGRESS

distance_cutoff_linker <- function(from_data, to_data = from_data,
                                   cutoff = 0, method)
{
  
}

distance_weight_linker <- function(from_data, to_data = from_data, method)
{    
  duplex_data_linker(function(from_selection, from_data, to_data) {
    
  })
}

## would help from the 'graph' package
## to do enough cool stuff (p-neighborhood, connected components)
## maybe separate package?
graph_linker <- function(from_data, to_data = from_data, graph, p = 1L) { }


