# Plumbr events
#
# Plumbr data structures send only single event for data changes:
# data_changed.  This has a two arguments, i and j.  Either both are NULL,
# indicating a change in the shape of the underlying data, or they give the
# the locations of changed data values.
#

shape_changed <- function(i, j) is.null(i) || is.null(j)


#' @events a list of event parameters
#' @returns a unified event
combine_data_events <- function(events) {
  for(event in events) {
    if (shape_changed(event$i, event$j)) return(list(i = NULL, j = NULL))
  }
  
  unique(do.call("rbind", lapply(events, as.data.frame, 
    stringsAsFactors = FALSE)))
}

add_listener <- function(mf, callback) {
  attr(mf, "listeners") <- append(listeners(mf), callback)
}

listeners <- function(mf) {
  attr(mf, "listeners")
}

notify_listeners <- function(mf, i, j) {
  if (is_paused(mf)) {
    attr(mf, "events") <- append(attr(mf, "events"), list(list(i = i, j = j)))
  } else {
    for(listener in listeners(mf)) {
      listener(i, j)
    }    
  }
}

# Pausing and playing.
# 
# When a mutaframe is paused, it accumulates events without passing them on.
# When unpaused, it accumulates all events into a single event and passes it
# on.

pause <- function(mf) {
  attr(mf, "paused") <- TRUE
  attr(mf, "events") <- list()
}
unpause <- function(mf) {
  events <- attr(mf, "events")

  attr(mf, "paused") <- FALSE
  attr(mf, "events") <- NULL

  all <- combine_data_events(events)  
  notify_listeners(mf, all$i, all$j)
}

is_paused <- function(x) attr(x, "paused") %||% FALSE