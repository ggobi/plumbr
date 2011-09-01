\name{duplex_data_linker}
\alias{duplex_data_linker}
\title{Duplex linking}
\usage{
  duplex_data_linker(delegate, from_data, to_data =
  from_data)
}
\arguments{
  \item{delegate}{The linking function that performs the
  mapping, such as \code{\link{match_any_linker}}.}

  \item{from_data}{A \code{data.frame} of keys}

  \item{to_data}{A \code{data.frame} of keys}
}
\value{
  A two-way linking function as described in the details.
}
\description{
  A utility for creating linking functions that operate in
  both directions (full duplex).
}
\details{
  The generated linker function takes two arguments:
  \code{from_selection} and \code{new_selection}. If
  \code{new_selection} is specified, \code{new_selection}
  is mapped from \code{to_data} to \code{from_data}.
  Otherwise, \code{from_selection} is mapped from
  \code{from_data} to \code{to_data}.
}
\author{
  Michael Lawrence
}

