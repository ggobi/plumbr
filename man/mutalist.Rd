\name{mutalist}
\alias{as.data.frame.mutalist}
\alias{as.list.mutalist}
\alias{c.mutalist}
\alias{head.mutalist}
\alias{lapply.mutalist}
\alias{length.mutalist}
\alias{mutalist}
\alias{[<-.mutalist}
\alias{[.mutalist}
\alias{[[<-.mutalist}
\alias{[[.mutalist}
\alias{$<-.mutalist}
\alias{$.mutalist}
\alias{mutalist2env}
\alias{names<-.mutalist}
\alias{names.mutalist}
\alias{print.mutalist}
\alias{rep.mutalist}
\alias{rev.mutalist}
\alias{tail.mutalist}
\alias{unlist.mutalist}
\title{mutalist}
\usage{
  mutalist(...)

  \method{length}{mutalist} (x)

  \method{names}{mutalist}(x, ...) <- value

  \method{names}{mutalist} (x)

  \method{[[}{mutalist}(x, i, j, ...)

  \method{[[}{mutalist}(x, i, j, ...) <- value

  \method{$}{mutalist}(x, name) <- value

  \method{[}{mutalist}(x, i, j, ..., drop)

  \method{[}{mutalist}(x, i, j, ...) <- value

  \method{head}{mutalist} (x, n = 6L, ...)

  \method{tail}{mutalist} (x, n = 6L, ...)

  \method{c}{mutalist} (x, ..., recursive = FALSE)

  \method{lapply}{mutalist} (X, FUN, ...)

  \method{as.list}{mutalist} (x, ...)

  \method{as.data.frame}{mutalist} (x, ...)

  \method{unlist}{mutalist} (x, recursive = TRUE, use.names
  = TRUE)

  mutalist2env(x, envir = new.env(hash, parent, size),
  parent = parent.frame(), hash = FALSE, size = 29L)

  \method{rev}{mutalist} (x)

  \method{rep}{mutalist} (x, ...)

  \method{print}{mutalist} (x, ...)
}
\arguments{
  \item{...}{elements to include in the list or arguments
  passed to methods}

  \item{x}{a mutalist}

  \item{value}{replacement value}

  \item{i}{element indices}

  \item{j}{unused}

  \item{name}{element name}

  \item{drop}{unused}

  \item{n}{number of elements in subset}

  \item{recursive}{whether to perform recursively}

  \item{X}{a mutalist}

  \item{FUN}{a function to apply over the elements}

  \item{use.names}{whether to preserve the names}

  \item{envir}{environment to populate}

  \item{parent}{parent for new environment, if created}

  \item{hash}{whether to hash the new environment}

  \item{size}{initial size of hash table}
}
\value{
  a new mutalist
}
\description{
  The mutalist is a mutable list. Modifications to a
  mutalist occur by a reference semantic. Otherwise, it
  should act like an ordinary R list and provides a similar
  API. If anything is found missing, please inform the
  authors.
}
\author{
  Michael Lawrence
}

