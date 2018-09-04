#' @title Execute a Workflow or Pipeline
#' @description Execute the Targets in Pipeleines.
#' @param x List of class "workflow" or "pipeline". If workflow, builds the targets in the Pipelines; if Pipeline, builds the targets.
#' @param locality If "locality", execute on local machine. If "sge", spin up one SGE job per parallel pipeline and wait for results;
#' requires a GridEngine cluster manager.
#' @param parallel Logical. If \code{TRUE}, use all available cores for (hopefully) a performance gain.
#' @param clobber Logical. Execute from the very beginning (\code{TRUE}) or from last-built Target (\code{FALSE}).
#' @param ... Other arguments.
#' @return List of class "workflow" or "pipeline" with targets built.
#' @export
execute <- function(x, ...) UseMethod('execute')

execute.default <- function(x, ...) stop('Cannot execute', class(x))

execute.workflow <- function(x, locality='local', parallel=TRUE, clobber=FALSE, ...) {

  if ( locality == 'local' )
    x$pipelines <- mclapply2(x$pipelines, execute, mc.cores=ifelse(parallel, future::availableCores(), 1), clobber=clobber)
  else if ( locality == 'sge' ) {

    println('Distributing GridEngine jobs...')

    # submit jobs to queue
    sge_jobs <- lapply(x$pipelines, execute_distributed, clobber=clobber)

    failed_submissions <- sapply(sge_jobs, function(z) !z$successful_submission)

    if ( sum(failed_submissions) > 0 ) println('Warning: Failed', sum(failed_submissions), 'GridEngine submissions')

    println('Submitted GridEngine jobs...')

    # wait for completion
    while ( any(sapply(sge_jobs, is_out_of_queue)) ) {

      Sys.sleep(30)

    }

    println('GridEngine jobs all finished!')

    x$pipelines <- lapply(sge_jobs, function(z) readRDS(z$checkpoint_file))


  } else

    stop('Locality must be one of local or sge; was ', locality)

  return ( x )

}

execute.pipeline <- function(x, clobber=FALSE, ...) {

  built_target_index <- which(sapply(x$targets, is_built))
  last_built_target_index <- -1

  if ( length(built_target_index) > 0 )  last_built_target_index <- max(built_target_index)

  if ( last_built_target_index > 0 ) println('Last built target is number', last_built_target_index)

  for ( target_n in seq_along(x$targets) ) {

    if ( ! is_built(x$targets[[target_n]]) | clobber ) {

      result <- build(x$targets[[target_n]], data=x$data, checkpoint=x$checkpoint) %except% function(e) {

        print(e)

        return(NA)
      }

      if ( is.na(result) ) {

        # save progress; early return

        println('Processing failed on call', as.character(x$targets[[target_n]]$call))

        return ( x )


      } else {

        x$data <- result
        x$targets[[target_n]]$is_built <- TRUE

        checkpoint(x)

      }

    }

  }

  return ( x )

}

#' @title Execute a Pipeline on SGE
#' @description Execute the Targets in Pipeleines using a GridEngine style cluster.
#' @param x List of class "workflow" or "pipeline". If workflow, builds the targets in the Pipelines; if Pipeline, builds the targets.
#' @param mode Not used.
#' @param submit_command Character. Command used for job submission; usually "qsub" (the default).
#' @param submit_script Character. Path to a SGE submission script template.
#' @param clobber Logical. Execute from the very beginning (\code{TRUE}) or from last-built Target (\code{FALSE}).
#' @param ... Other arguments.
#' @return List of class "SGEjob" containing submission parameters.
#' @export
execute_distributed <- function(x, ...) UseMethod('execute_distributed')

execute_distributed.default <- function(x, ...) stop('Cannot distribted execute', class(x))

execute_distributed.pipeline <- function(x, submit_script,
                                         mode='sge', submit_command='qsub',
                                         clobber=TRUE, ...) {

  # test for SGE
  sge_test <- system2('qstat', stdout=TRUE) %except% NA

  if ( is.na(sge_test[1]) ) stop('SGE commands can\'t be called (tested qstat)')

  script_dir <- file.path(x$working_directory, '_sge-scripts')
  dir.create(script_dir, showWarnings=FALSE)

  log_dir <- file.path(x$working_directory, '_sge-logs')
  dir.create(log_dir, showWarnings=FALSE)

  this_submission_script_filename <- file.path(script_dir, paste0(basename(x$checkpoint_filename), submit_script))
  this_log_filename               <- file.path(log_dir, paste0(basename(x$checkpoint_filename), submit_script, '.log'))
  this_success_filename           <- file.path(log_dir, paste0(basename(x$checkpoint_filename), '.success'))
  this_job_name                   <- paste('worker', basename(x$checkpoint_filename), sep='-')
  this_pipeline_size              <- ceiling(object.size(x) / (1000^3)) * 3  #Gb

  original_script <- readLines(submit_script)

  substitutions <- c('__virtualmemory__'=this_pipeline_size,
                     '__name__'=this_job_name,
                     '__logfile__'=this_log_filename,
                     '__checkpointfile__'=x$checkpoint_filename,
                     '__clobber__'=clobber)

  substituted_script <- original_script

  for ( patt in names(substitutions) ) substituted_script <- gsub(patt, substitutions[patt], substituted_script, fixed=TRUE)

  checkpoint(x)

  writeLines(substituted_script, this_submission_script_filename)

  command_options <- c('-terse', this_submission_script_filename)

  if ( file.exists(this_success_filename) ) unlink(this_success_filename)

  job_id <- system2(submit_command, args=command_options, stdout=TRUE) %except% NA

  return ( structure(list(submit_command=job_id,
                          args=command_options,
                          job_id=job_id,
                          successful_submission=!is.na(job_id),
                          script_filename=this_submission_script_filename,
                          log_filename=this_log_filename,
                          success_filename=this_success_filename,
                          checkpoint_filename=x$checkpoint_filename,
                          job_name=this_job_name,
                          virtual_memory=this_pipeline_size), class='SGEjob') )

}