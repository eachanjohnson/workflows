#' Execute a Target object according to its call and provided input data.
#'
#' @param x Object of class "target".
#'        data Apply the target's method to this data.
#'        ... Other arguments.
#' @return The result of applying the target's call to the \code{data}.
#' @export
build <- function(x, ...) UseMethod('build')

#' @export
build.default <- function(x, ...) stop('Cannot build', class(x))

#' @export
build.target <- function(x, data, ...) {

  #stopifnot('pipeline' %in% class(pipeline))

  new_data <- eval(x$call, list(data=data))

  return ( new_data )

}
