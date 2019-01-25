#' Make a new Target object.
#'
#' @param method Name. Method to apply.
#' @param input_file Not used.
#' @param output_file Not used.
#' @param ... Other arguments passed to \code{call_factory()}.
#' @return list of class "target"
#' @export
newTarget <- function(method, input_file=NA, output_file=NA, ...) {

  call <- call_factory(method, quote(data), ...)

  basic_structure <- list(call=call, is_built=FALSE,
                          lock_file=NA, temp_file=NA,
                          input_file=input_file, output_file=NA)

  return ( structure(basic_structure, class='target') )

}
