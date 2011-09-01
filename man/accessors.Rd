\name{$.mutaframe}
\alias{[<-.mutaframe}
\alias{[.mutaframe}
\alias{[[<-.mutaframe}
\alias{[[.mutaframe}
\alias{$<-.mutaframe}
\alias{$.mutaframe}
\title{Extraction and Replacement}
\arguments{
  \item{x}{A mutaframe}

  \item{name}{Name of the column to extract}

  \item{i}{The row indices}

  \item{j}{The column indices}

  \item{...}{Arguments passed to methods}

  \item{value}{The replacement column}

  \item{drop}{If \code{TRUE} and the result of subsetting
  is a single column or row, that column or row is
  extracted as the result. By default, this is \code{TRUE}
  if the result has one column.}
}
\value{
  The selected column

  The selected column

  A dynamic, filtering mutaframe
}
\description{
  These functions extract, subset and replace data in a
  mutaframe. For the most part, these behave much like
  those for \code{data.frame}.
}
\details{
  The subset function, \code{[}, does not copy the data; it
  establishes a dynamic filter.

  Replacing an existing variable will pass the replacement
  data up the reverse pipeline, towards the root. When
  defining a new variable, the variable is stored in the
  current mutaframe; not at the root.
}

