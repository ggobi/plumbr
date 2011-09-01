\name{RegionSelection-class}
\docType{class}

\alias{class:RegionSelection}
\alias{RegionSelection-class}

\alias{ItemSelection}

\description{
  The \code{ItemSelection} class implements
  \code{\linkS4class{Selection}} for the selection of 1D and 2D regions
  in plot/data space.
}

\section{Constructor}{
  \describe{\item{}{\code{RegionSelection(delegate = NULL)}: Constructs
      an \code{RegionSelection} object with the underlying selection
      provided by \code{delegate}, which may be a function or any other
      R object. If it is not a function, \code{delegate} must support
      coercion to a \code{matrix} as described in the next section.
      However, \code{delegate} is usually a function that is invoked
      whenever the selection is stored or retrieved. If the function is
      called with no arguments, it should return the
      selection. Otherwise, the argument is the new selection status,
      and the function should store it. This is the same semantic as
      \link[=makeActiveBinding]{active bindings}. This dynamic
      functionality allows proxying of other \code{Selection} objects or
      external sources, such as a selection model from a GUI toolkit.
    }
  }
}

\section{Interpreting the Selection}{
  Any R object can represent the underlying selection, so for simplicity
  we recommend that the client interpret the selection through
  coercion. Currently, there is only one supported coercion of
  \code{RegionSelection}:
  \describe{
    \item{}{\code{as.matrix(x)}: returns a matrix with a column for each
      dimension and a row for each point. In the 2D case, the points
      describe one or more polygons. As with the \code{\link{polygon}}
      function, polygons are separated by rows of \code{NA}, and the last
      point is connected with the first. In the 1D case, the single column
      might encode, for example, selections of factor levels in an area
      plot.
    }
  }

  We will probably need to add more coercions as use cases arise. This
  is still very preliminary.
}

\section{Supported Selection Calculus}{
  For now, \code{RegionSelection} only supports the \code{add} operation
  described in the documentation for \code{\linkS4class{Selection}}.
}

\seealso{\code{\linkS4class{Selection}} for the rest of the details.}

\author{Michael Lawrence}

\examples{
## forthcoming
}
