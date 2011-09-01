\name{Selection-class}
\docType{class}

\alias{class:Selection}
\alias{Selection-class}

\title{Selection}

\description{
  A virtual base class for data models that store a selection, which
  might be of items, regions, or whatever.  Clients can register
  handlers for selection changes and can create proxy models to
  transform selections, link across datasets and map selections to
  actions on the data.

  \strong{This design is preliminary and subject to change.}
}

\section{Interpreting The Selection}{
  Internally, the selection may be stored as any object, including as a
  function that is invoked whenever the selection is stored or
  retrieved. The function allows dynamic mapping of selections. Due to
  this generality, the client should not access the selection
  directly. Instead, it should explicitly coerce the selection object to
  an interpretable representation. The set of supported coercions
  depends on the subclass.  For example,
  \code{\linkS4class{ItemSelection}} has a \code{as.logical} method that
  coerces it to a logical vector, where an element is \code{TRUE} if the
  corresponding element in the dataset is selected.
}

\section{Responding to Selection Changes}{
  Whenever the selection is changed, the \code{changed} signal is
  emitted. The signal has zero arguments. See the \code{objectSignals}
  package for details on using signals.

  Eventually, a selection leads to the execution of some action by the
  application. In interactive graphics, that action usually involves
  scaling/transforming the selection to a modification on the data. The
  \code{x$scale(scaler, data)} method tries to facilitate these
  operaitons. All it does is create a handler for the \code{changed}
  signal on \code{x} that passes \code{x} and \code{data} to the function
  \code{scaler}, which implements the change.
}

\section{The Selection Calculus}{
  Since any type of object can represent a selection, setting the
  selection has very few constraints. There are several ways to modify
  the selection. Not all of them will be supported by every subclass. In
  the code snippets below, \code{x} represents a \code{Selection} object
  and \code{selection} represents the primary representation of a
  selection, like a logical vector.
  
  \describe{
    \item{Replacement}{\code{x$replace(selection)}: this is supported by all
      implementations.}
    \item{Or/Addition}{\code{x$add(selection)}: the result contains the
      union of the original selection and \code{selection}.}
    \item{Setdiff/Subtract}{\code{x$subtract(selection)}: the result
      contains the original selection except that indicated
      by \code{selection}.}
    \item{And/Intersect}{\code{x$intersect(selection)}: the result
      contains the intersection of the original selection and
      \code{selection}.}
    \item{Xor/Toggle}{\code{x$toggle(selection)}: 
      The intersection of the original selection and \code{selection} is
      deselected, that only in \code{selection} is selected.}
  }
}

\section{Linking Selections}{
  In interactive graphics, it is often necessary to link selections
  within and across datasets. The \code{x$link(linker)} method creates a
  new \code{Selection} object that proxies \code{x} and maps the
  selection in \code{x} through \code{linker}. Changes to the selection
  in \code{x} will propagate via \code{linker} to changes in the
  proxy. Analogously, the \code{linker} will pass modifications to the
  proxy down to \code{x}.

  The \code{linker} may be provided as an integer vector, like that
  returned by \code{\link{match}}, but it is usually a function, as that
  allows very general linking strategies. As an example, let us consider
  a simple linker between two datasets based on key matching. We assume
  that the keys, \code{source_keys} and \code{dest_keys}, are in the
  enclosure of our linker function.

  \preformatted{
    function(source_selection, new_dest_value) {
      if (missing(new_dest_value))
        dest_keys %in% source_keys[as.logical(source_selection)]
      else source_keys %in% dest_keys[as.logical(new_dest_value)] 
    }
  }
  
  The \code{linker} function takes one or two arguments, depending on
  whether the selection is being retrieved or stored. When the selection
  is being retrieved, \code{source_selection} is passed as the only
  argument. The duty of the \code{linker} is then to retrieve the
  underlying selection from \code{source_selection} (through coercion,
  see above) and figure out which keys in the destination selection
  match the selected source keys. The \code{new_dest_value} argument is
  provided whenever the selection is being stored/set. In that case, the
  analogous operation is performed, in the opposite direction. The
  symmetry here is fairly obvious, and \code{\link{duplex_data_linker}}
  is a utility for facilitating the implementation of such two-way
  linking functions.
}

\seealso{The \code{\link{ItemSelection}} and \code{\link{RegionSelection}}
  subclasses, which have examples.}

\author{Michael Lawrence}
