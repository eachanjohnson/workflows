#' @title Check on SGEjob status
#' @description Check on SGEjob status by getting output of \code{qstat}.
#' @param x List of class "SGEjob".
#' @param ... Other arguments.
#' @return Output of \code{qstat} for \code{jobid} of SGEjob.
#' @export
status <- function(x, ...) UseMethod('status')

#' @export
status.default <- function(x, ...) stop('Cannot check status of', class(x))

#' @export
status.SGEjob <- function(x, ...) system2('qstat', args=c('-j', x$job_id), stdout=TRUE, stderr=FALSE)

#' @title Check if SGEjob still in queue
#' @description Check if SGEjob still in queue
#' @param x List of class "SGEjob".
#' @param ... Other arguments.
#' @return \code{TRUE} if SGEjob still in queue, otherwise \code{FALSE}.
#' @export
is_out_of_queue <- function(x, ...) UseMethod('is_out_of_queue')

#' @export
is_out_of_queue.default <- function(x, ...) stop('Cannot check queue of', class(x))

#' @export
is_out_of_queue.SGEjob <- function(x, ...) system2('qstat', args=c('-j', x$job_id), stdout=FALSE) == 0

#' @title Check if SGEjob still in queue
#' @description Check if SGEjob still in queue
#' @param x List of class "SGEjob".
#' @param ... Other arguments.
#' @return \code{TRUE} if SGEjob still in queue, otherwise \code{FALSE}.
#' @export
is_queued <- function(x, ...) UseMethod('is_queued')

#' @export
is_queued.default <- function(x, ...) stop('Cannot check queue of', class(x))

#' @export
is_queued.SGEjob <- function(x, ...) {

  state <- Filter(function(z) grepl('^job_state', z), status(x))[1]

  return ( grepl('qw$', state) )

}

#' @title Check if SGEjob still running
#' @description Check if SGEjob still running
#' @param x List of class "SGEjob".
#' @param ... Other arguments.
#' @return \code{TRUE} if SGEjob still running, otherwise \code{FALSE}.
#' @export
is_running <- function(x, ...) UseMethod('is_running')

#' @export
is_running.default <- function(x, ...) stop('Cannot check running of', class(x))

#' @export
is_running.SGEjob <- function(x, ...) {

  state <- Filter(function(z) grepl('^job_state', z), status(x))[1]

  return ( grepl('r$', state) )

}

#' @title Check if SGEjob ended successfully
#' @description Check if SGEjob ended successfully, as determined by existence of file at path \code{success_filename}.
#' @param x List of class "SGEjob".
#' @param ... Other arguments.
#' @return \code{TRUE} if SGEjob ended successfully, otherwise \code{FALSE}.
#' @export
is_successful <- function(x, ...) UseMethod('is_running')

#' @export
is_successful.default <- function(x, ...) stop('Cannot check success of', class(x))

#' @export
is_successful.SGEjob <- function(x, ...) file.exists(x$success_filename)
