\name{pause}
\alias{pause}
\title{Pause (cache) events.}
\usage{
  pause(mf)
}
\arguments{
  \item{mf}{mutaframe}
}
\description{
  When a mutaframe is paused, it accumulates events without
  passing them on. When unpaused, it accumulates all events
  into a single event and passes it on.
}
\details{
  This is a performance optimisation for when you expect
  many changes: pause the mutaframe, perform all the
  changes and then unpause.
}

