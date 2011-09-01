\name{match_any_linker}
\alias{match_any_linker}
\title{match_any_linker}
\usage{
  match_any_linker(from_data, to_data = from_data)
}
\arguments{
  \item{from_data}{A \code{data.frame}-like object
  containing the keys for linking the corresponding rows to
  rows in \code{to_data}}

  \item{to_data}{A \code{data.frame}-like object containing
  the keys that will be matched against the keys in
  \code{from_data}}
}
\value{
  a logical vector, indicating which \code{from_data} rows
  are linked
}
\description{
  Linking functions return a logical vector, with the
  \code{TRUE} elements indicating rows in the data that are
  linked.
}
\details{
  The \code{match_any_linker} function links rows in
  \code{from_data} to rows in \code{to_data} that share the
  same key.

  By convention, a key is defined as the combination of the
  values in every column of \code{from_data} and
  \code{to_data}. Thus, \code{from_data} and \code{to_data}
  should contain only the columns necessary for key
  generation. They should not be an entire dataset.
}
\author{
  Michael Lawrence
}

