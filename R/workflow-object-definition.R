#' Make a new Worklow object
#'
#' @param input_data Starting data
#'        working_directory Character. Top-level directory for data processing. Default is current working directory.
#'        checkpoint Boolean. Checkpoint intermediate files? Default \code{FALSE}.
#'        load_checkpoint Character. Start from named saved checkpoint. If \code{NULL} (default), don't.
#'        ... Other arguments
#' @return list of class "workflow"
#' @importFrom magrittr %>%
#' @export
newWorkflow <- function(input_data, working_directory='.',
                        checkpoint=FALSE, load_checkpoint=NULL, ...) {

  basic_structure                   <- list(...)

  if ( is.null(load_checkpoint) ) {

    basic_structure$pipelines         <- list(newPipeline(input_data=input_data, checkpoint=checkpoint,
                                                          working_directory=working_directory))

  } else if ( load_checkpoint == 'latest' ) {

    checkpoint_dir       <- file.path(working_directory, '_checkpoints')
    checkpoint_filenames <- list.files(checkpoint_dir, paste('^checkpoint', load_checkpoint))

    file_info <- file.info(file.path(checkpoint_dir, checkpoint_filenames)) %>% as.data.frame() %>%
      dplyr::add_rownames('filename') %>%
      dplyr::mutate(mtime=as.numeric(mtime),
             checkpoint_id=splitter(filename, '-', 1),
             checkpoint_time=as.numeric(splitter(checkpoint_id, 'checkpoint', 2))) %>%
      dplyr::filter(checkpoint_time == max(checkpoint_time)) %>%
      dplyr::select_('filename') %>%
      dplyr::distinct() %>%
      unlist()

    println('Loading checkpointed pipelines matching', load_checkpoint, 'from directory', checkpoint_dir, ':',
            paste(file_info, collapse=', '))

    basic_structure$pipelines <- lapply(file.path(checkpoint_dir, file_info), readRDS)

  } else {

    checkpoint_dir       <- file.path(working_directory, '_checkpoints')
    checkpoint_filenames <- list.files(checkpoint_dir, paste0('^checkpoint', load_checkpoint))

    println('Loading checkpointed pipelines matching', load_checkpoint, 'from directory', checkpoint_dir, ':',
            pyjoin(checkpoint_filenames, ', '))

    basic_structure$pipelines <- lapply(file.path(checkpoint_dir, checkpoint_filenames), readRDS)

  }

  basic_structure$pipelines <- lapply(basic_structure$pipelines, function(z) {

    z$checkpoint <- checkpoint

    return (z)

  })

  return ( structure(basic_structure, class='workflow') )


}
