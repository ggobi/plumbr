\name{ItemSelection-class}
\docType{class}

\alias{class:ItemSelection}
\alias{ItemSelection-class}

\alias{ItemSelection}

%% coercion methods
\alias{coerce,ItemSelection,vector-method}
\alias{as.vector,ItemSelection-method}
\alias{coerce,ItemSelection,integer-method}
\alias{as.integer,ItemSelection-method}
\alias{coerce,ItemSelection,numeric-method}
\alias{as.numeric,ItemSelection-method}
\alias{coerce,ItemSelection,logical-method}
\alias{as.logical,ItemSelection-method}
\alias{coerce,ItemSelection,factor-method}
\alias{as.factor,ItemSelection-method}
\alias{which,ItemSelection-method}

\description{
  The \code{ItemSelection} class implements
  \code{\linkS4class{Selection}} for the very common case of selecting
  items in a dataset, optionally with weights.
}

\section{Constructor}{
  \describe{\item{}{\code{ItemSelection(delegate = NULL)}: Constructs an
      \code{ItemSelection} object with the underlying selection provided
      by \code{delegate}, which may be a function or any other R
      object. If it is not a function, \code{delegate} must support the
      coercions described in the next section. A good example would be a
      logical vector. However, \code{delegate} is usually a function
      that is invoked whenever the selection is stored or retrieved. If
      the function is called with no arguments, it should return the
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
  coercion. Each of these simply delegate to the underlying
  selection object, which will need to support all of them for
  consistency. The following coercions are supported, where \code{x} is
  a \code{ItemSelection} instance:
  \describe{
    \item{}{\code{which(x)}: integer indices of the selected items.}
    \item{}{\code{as.logical(x)}: \code{TRUE} where selected.}
    \item{}{\code{as.integer(x)}: usually 0L (unselected) or 1L
      (selected), but in general it is a weighting of the selection.}
    \item{}{\code{as.numeric(x)}: similar to \code{as.integer}, except
      with real values.}
    \item{}{\code{as.factor(x)}: ordinarily this will have two
      levels, \code{FALSE} and \code{TRUE}, although it could have more,
      which confers support for multinary selections.}
  }
}

\section{Supported Selection Calculus}{
  All operations mentioned in \code{\linkS4class{Selection}} are
  supported: \code{add}, \code{subtract}, \code{toggle}, \code{intersect}.
}

\seealso{\code{\linkS4class{Selection}} for the rest of the details.}

\author{Michael Lawrence}

\examples{
  ## Assume we have a dataset:
  data(Cars93, package="MASS")
  mf <- mutaframe(Cars93)
  mf$.color <- "gray"
  ## First step is to create a base selection
  sel <- ItemSelection()
  ## Now, link that selection to other cases in same dataset by some variable
  linked_sel <- sel$link(match_any_linker(Cars93["Manufacturer"]))
  ## Finally, scale that linked selection to the data
  linked_sel$scale(function(x, d) {
    d[as.logical(x), ".color"] <- "red"
  }, mf)
  ## To test, select some cases
  cases <- rep(FALSE, nrow(mf))
  cases[seq(1, 10, 2)] <- TRUE
  sel$replace(cases)
}
