\name{as.data.frame.mutaframe}
\alias{as.data.frame.mutaframe}
\title{Coercion to data.frame}
\usage{
  \method{as.data.frame}{mutaframe} (x, row.names =
  rownames(x), optional = FALSE, ...)
}
\arguments{
  \item{x}{a mutaframe}

  \item{row.names}{character vector of rownames, defaults
  to rownames of \code{x}}

  \item{optional}{see \code{\link{as.data.frame}}}

  \item{...}{see \code{\link{as.data.frame}}}
}
\value{
  a \code{data.frame}
}
\description{
  Coerces a mutaframe to a \code{data.frame}
}

