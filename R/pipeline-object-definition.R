#' Make a new Pipeline object.
#'
#' @param input_data Starting data
#' @param working_directory Character. Top-level directory for data processing. Default is current working directory.
#' @param checkpoint Boolean. Checkpoint intermediate files? Default \code{FALSE}.
#' @param ... Other arguments
#' @return list of class "pipeline"
#' @importFrom magrittr %>%
#' @export
newPipeline <- function(input_data, checkpoint=FALSE, working_directory='.', ...) {

  basic_structure            <- list(...)

  basic_structure$data                <- input_data
  basic_structure$checkpoint          <- checkpoint
  basic_structure$working_directory   <- working_directory

  if ( checkpoint ) dir.create(file.path(working_directory, '_checkpoints'), showWarnings=FALSE)

  basic_structure$checkpoint_filename <- file.path(working_directory, '_checkpoints',
                                                   paste0('checkpoint', as.numeric(Sys.time())))
  basic_structure$temp_file <- paste0(basic_structure$checkpoint_filename, '.temp')
  basic_structure$lock_file <- paste0(basic_structure$checkpoint_filename, '.lock')

  basic_structure$scattered_by    <- 'all'
  basic_structure$scattered_value <- ''

  basic_structure$targets <- list()

  return ( structure(basic_structure, class='pipeline') )

}
