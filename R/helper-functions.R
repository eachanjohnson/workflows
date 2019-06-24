#' @title Get unique values
#' @description Get unique values of an element of an object.
#' @param x Object of class "data.frame"".
#' @param column Character. Element of \code{x} to find unique values for.
#' @param sort_result Logical. Sort the unique values before returning?
#' @param ... Other arguments.
#' @return Vector of same class as \code{column}
get_unique_values <- function(x, ...) UseMethod('get_unique_values')

#' @rdname get_unique_values
get_unique_values.default <- function(x, ...) stop('Can\'t get unique values of ', class(x))

#' @rdname get_unique_values
get_unique_values.data.frame <- function(x, column, sort_result=TRUE, ...) {

  column_values <- getElement(x, column)
  unique_values <-  unique(column_values)

  if ( sort_result ) unique_values <- sort(unique_values)

  return ( unique_values )

}

#' @title Generate calls of function applied to arguments
#' @description Generate calls of function applied to arguments.
#' @param f Function.
#' @param ... Objects to apply f to.
#' @return A call.
call_factory <- function(f, ...) UseMethod('call_factory')

call_factory.default <- function(f, ...) stop('Can\t generate calls for', class(f))

call_factory.function <- function(f, ...) as.call(c(list(f), lapply(c(...), function(x) x)))

#' #@title Generate calls of function applied to arguments
#' #@description Generate calls of function applied to arguments.
#' #@param X Function.
#' #@param ... Objects to apply f to.
#' #@return A call.
mclapply2 <- function(X, FUN, ...,
                      mc.preschedule = TRUE, mc.set.seed = TRUE,
                      mc.silent = FALSE, mc.cores = get_cores(),
                      mc.cleanup = TRUE, mc.allow.recursive = FALSE) {

  original_class <- class(X)

  if ( is.data.frame(X) ) new_x <- FUN(X, ...)
  else                    new_x <- parallel::mclapply(X, FUN, ...,
                                                      mc.preschedule = mc.preschedule,
                                                      mc.set.seed = mc.set.seed,
                                                      mc.silent = mc.silent, mc.cores = mc.cores,
                                                      mc.cleanup = mc.cleanup,
                                                      mc.allow.recursive = mc.allow.recursive)

  class(new_x) <- original_class

  return ( new_x )

}

println <- function(...) {

  #library('futile.logger')

  cat(date(), '>', ..., '\n')
  flush.console()

}

splitter <- function(x, char, n=NULL, rejoin_char=char) {

  vapply(x, function(y) {

    split_string <- strsplit(y, char, fixed=TRUE)[[1]]

    if ( is.null(n) ) n_ <- seq_len(length(split_string)) else n_ <- n

    paste(split_string[n_], collapse=rejoin_char)

  }, 'a')

}

