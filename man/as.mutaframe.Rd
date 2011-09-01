\name{as.mutaframe}
\alias{as.mutaframe}
\alias{as.mutaframe.data.frame}
\alias{as.mutaframe.default}
\alias{as.mutaframe.mutaframe}
\title{Coercion to mutaframe}
\usage{
  as.mutaframe(x, ...)

  \method{as.mutaframe}{mutaframe} (x, ...)

  \method{as.mutaframe}{data.frame} (x, ...)

  \method{as.mutaframe}{default} (x, ...)
}
\arguments{
  \item{x}{the object to coerce}

  \item{...}{arguments passed to methods}
}
\value{
  a mutaframe
}
\description{
  Coerce an object to a mutaframe. Supported types include
  \code{data.frame}, or anything coercible to one.
}

