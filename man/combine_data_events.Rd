\name{combine_data_events}
\alias{combine_data_events}
\title{Combine list of events into single event.}
\usage{
  combine_data_events(events)
}
\arguments{
  \item{events}{a list of event parameters}
}
\value{
  a unified event
}
\description{
  If any event is a \code{shape_changed} event, return it.
  Otherwise, take the unique elements of the union of all
  element changes.
}

