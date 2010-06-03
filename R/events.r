# Plumbr events
#
# Plumbr data structures send only single event for data changes:
# data_changed.  This has a single argument which is either NULL, 
# representing a change in the shape of the underlying data, or a two column
# matrix giving the locations of changed data values.
#



#' @events a list of event parameters
#' @returns a unified event
combine_data_events <- function(events) {
  if (any(sapply(events, is.null))) return(NULL)
  
  do.call("rbind", events)
}

add_listener <- function(mf, callback) {
  attr(mf, "listeners") <- append(listeners(mf), callback)
}

listeners <- function(mf) {
  attr(mf, "listeners")
}

notify_listeners <- function(mf, i, j) {
  for(listener in listeners(mf)) {
    listener(i, j)
  }
}