\name{add_listener}
\alias{add_listener}
\title{Plumbr events}
\usage{
  add_listener(mf, callback)
}
\arguments{
  \item{mf}{muta frame}

  \item{callback}{function with arguments i and j}
}
\description{
  Plumbr data structures send only single event for data
  changes: data_changed.  This has a two arguments, i and
  j.  Either both are NULL, indicating a change in the
  shape of the underlying data, or they give the the
  locations of changed data values.
}

