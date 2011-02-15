## generic signals, through which mutable objects report state changes

## A reference class cannot inherit from 'function'. So if we use a
## ref class, we have:
## connect syntax: obj$mySignal$connect(handler, ...)
## emit syntax: obj$mySignal$emit(...)

## Or, one could have an S4 class that extends 'function':
## connect syntax: connect(obj, mySignal = handler, ...)
## emit syntax: obj$mySignal(...)
## This is like the C# syntax.

## Kind of favoring the former syntax, because it is symmetric and it
## is more obvious when a signal is being emitted. The latter syntax
## is implemented in the MutableRanges package. Might change that
## though.

## Arguments passed to emit() are passed to handlers, with some caveats:
## - If namedArgs=TRUE, arguments are named in call to handler.
## - If a handler is missing a signal argument, the argument is
##   dropped when calling the handler. The handler is just not interested.
## - A handler may have arguments not in the signal signature.
##   It could be called in other contexts.
## - Extra arguments to pass to a handler may be specified upon connection.
## - All signal args must be passed to emit(), unless they have a default.
## - Probably not a good idea for a signal to have '...' in its signature.

## mutaframe/list should use this internally

## tests:

## Signal(x, y)

## signal <- Signal(x, y, z = NA)
## signal$connect(function(n, x, option = "none") message("x:", x),
##                namedArgs = TRUE)
## signal$connect(function(z, ...) message("z:", z, " x:", list(...)$x),
##                namedArgs = TRUE)
## signal$emit(0, 1)

## signal$connect(function(x, y, option = "none")
##                message("y:", y, " op:", option), TRUE)
## signal$connect(function(x, y, option = "none")
##                message("op:", option), option = "test")
## signal$connect(function(x, y, option = "none")
##                message("op:", option), FALSE, "test")
## id <- signal$connect(function(x, y, option = "none")
##                      message("op:", option), TRUE, "test")

## signal$emit(0, 1)

## signal$disconnect(id)
## signal$emit(0, 2)

## signal <- Signal(x)
## signal$connect(function(i) print(i))

## signal$block()
## signal$emit(0)
## signal$unblock()
## signal$emit(0)

## signal$buffer()
## signal$emit(0); signal$emit(1); signal$emit(3)
## signal$flush()

## signal$accumulator(function(prev, cur) {
##   prev$x <- c(prev$x, cur$x)
##   prev
## })
## signal$buffer()
## signal$emit(0); signal$emit(1); signal$emit(3)
## signal$flush()

Signal.gen <- setRefClass("Signal",
                          fields = list(.listeners = "list", emit = "function",
                            .idCounter = "integer", .blocked = "logical",
                            .buffered = "logical", .queue = "list",
                            .accumulator = "function"))

Signal <- function(...) {
  call <- sys.call()[-1L]
  hasDefault <-
    if (is.null(names(call)))
      rep(FALSE, length(call))
    else nzchar(names(call))
  names(call)[!hasDefault] <- sapply(call[!hasDefault], deparse)
  call[!hasDefault] <- alist(foo=)
  signal <- Signal.gen$new(.idCounter = 0L, .blocked = FALSE, .buffered = FALSE)
  signal$emit <- as.function(c(as.list(call), quote({
    if (.blocked)
      return(invisible())
    if (.buffered) {
      event <- lapply(match.call()[-1], eval, environment())
      if (length(formals(.accumulator)) == 2L && length(.queue))
        .queue[[1]] <<- .accumulator(.queue[[1]], event)
      else .queue <<- c(.queue, list(event))
    } else {
      for (listener in .listeners)
        eval(listener)
    }
    invisible()
  })), signal@.xData) # is getting .xData bad practice?
  signal
}

Signal.gen$methods(connect = function(FUN, namedArgs = FALSE, ...) {
  ## FUN is a function
  extraArgs <- list(...)
  if (length(extraArgs) > length(formals(FUN)))
    stop("More args in '...' than in formals of 'FUN'")
  args <- sapply(names(formals(emit)), as.name)
  ## the wrapper adds the extra args, and also uses '...' to drop unwanted args
  wrapperFormals <- formals(FUN)
  handlerArgs <- sapply(names(wrapperFormals), as.name)
  if (!("..." %in% names(wrapperFormals)))
    wrapperFormals <- c(wrapperFormals, alist(...=))
  wrapper <- as.function(c(wrapperFormals, as.call(c(FUN, handlerArgs))))
  .idCounter <<- .idCounter + 1L
  id <- as.character(.idCounter)
  if (!namedArgs)
    names(args) <- NULL
  .listeners[[id]] <<- as.call(c(list(wrapper), c(args, extraArgs)))
  invisible(id)
})

Signal.gen$methods(disconnect = function(id) {
  .listeners[[id]] <<- NULL
  invisible(.self)
})

Signal.gen$methods(block = function() {
  .blocked <<- TRUE
  invisible(.self)
})

Signal.gen$methods(unblock = function() {
  .blocked <<- FALSE
  invisible(.self)
})

Signal.gen$methods(buffer = function() {
  .buffered <<- TRUE
  invisible(.self)
})

Signal.gen$methods(flush = function() {
  if (length(formals(.accumulator)) == 1L)
    .queue <<- list(.accumulator(.queue))
  .buffered <<- FALSE
  .accumulator <<- function() NULL
  lapply(.queue, do.call, what = emit)
  .queue <<- list()
  invisible(.self)
})

## Allows C++-style initializer chaining
Signal.gen$methods(accumulator = function(value) {
  if (missing(value))
    .accumulator
  else {
    .accumulator <<- value
    invisible(.self)
  }
})

setMethod("show", "Signal", function(object) {
  cat(deparse(as.call(c(as.name(class(object)), formals(object$emit)))), "with",
      length(object$.listeners), "listeners\n")
})


##' Convenience function for defining a reference class field that
##' signals when set.
##'
##' @title Signaling Fields
##' @param name Name of the field
##' @param class Class name of the field
##' @param signalName Name of the signal
##' @return A list that is easily concatenated into the field list
##' @author Michael Lawrence
##' @examples Brush.gen <- setRefClass("Brush",
##' fields = signalingField("color", "character"))
##' brush <- Brush.gen$new(color = "blue")
##' brush$colorChanged$connect(function() print(brush$color))
##' brush$color <- "red"
signalingField <- function(name, class,
                           signalName = paste(name, "Changed", sep = ""))
{
  .name <- paste(".", name, sep = "")
  body <- substitute({
    if (missing(val))
      .name
    else {
      if (!is(val, .class))
        stop("Cannot set an object of type '", class(val), "' on '", name,
             "', a field of type '", .class, "'")
      changed <- !identical(.name, val)
      .name <<- val
      if (changed) {
        if (is.null(body(signal$emit)))
          signal <<- Signal() # lazy construction of signal
        signal$emit()
      }
    }
  }, list(.name = as.name(.name), name = name, signal = as.name(signalName),
          .class = class))
  structure(list(as.function(c(alist(val=), body)), class, "Signal"),
            names = c(name, .name, signalName))
}

