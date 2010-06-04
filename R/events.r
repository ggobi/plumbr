#' Plumbr events
#'
#' Plumbr data structures send only single event for data changes:
#' data_changed.  This has a two arguments, i and j.  Either both are NULL,
#' indicating a change in the shape of the underlying data, or they give the
#' the locations of changed data values.
#'
#' @params mf muta frame
#' @params callback function with arguments i and j
#' @export
add_listener <- function(mf, callback) {
  attr(mf, "listeners") <- append(listeners(mf), callback)
}

#' Is the event a shape changed event?
#' 
#' @params i col index
#' @params j row index
#' @export
shape_changed <- function(i, j) is.null(i) || is.null(j)

#' Combine list of events into single event.
#'
#' If any event is a \code{shape_changed} event, return it.  Otherwise, 
#' take the unique elements of the union of all element changes.
#'
#' @events a list of event parameters
#' @returns a unified event
combine_data_events <- function(events) {
  for(event in events) {
    if (shape_changed(event$i, event$j)) return(list(i = NULL, j = NULL))
  }
  
  unique(do.call("rbind", lapply(events, as.data.frame, 
    stringsAsFactors = FALSE)))
}

#' List mutaframe listeners.
listeners <- function(mf) {
  attr(mf, "listeners")
}

#' Notify listeners that data has changed.
notify_listeners <- function(mf, i, j) {
  if (is_paused(mf)) {
    attr(mf, "events") <- append(attr(mf, "events"), list(list(i = i, j = j)))
  } else {
    for(listener in listeners(mf)) {
      listener(i, j)
    }    
  }
}

#' Pause (cache) events.
#' 
#' When a mutaframe is paused, it accumulates events without passing them on.
#' When unpaused, it accumulates all events into a single event and passes it
#' on.
#'
#' This is a performance optimisation for when you expect many changes: 
#' pause the mutaframe, perform all the changes and then unpause.
#'
#' @param mf mutaframe
#' @export
pause <- function(mf) {
  attr(mf, "paused") <- TRUE
  attr(mf, "events") <- list()
}

#' Unpause (reply) events.
#'
#' @param mf mutaframe
#' @export
unpause <- function(mf) {
  events <- attr(mf, "events")

  attr(mf, "paused") <- FALSE
  attr(mf, "events") <- NULL

  all <- combine_data_events(events)  
  notify_listeners(mf, all$i, all$j)
}

#' Is a mutaframe currently paused?
is_paused <- function(x) attr(x, "paused") %||% FALSE