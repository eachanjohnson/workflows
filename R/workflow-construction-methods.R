#' @title Add a step to a workflow or pipeline
#' @description Add a step to a "workflow" or "pipeline" object where \code{method} will be executed.
#' @param x Object of class "workflow" or "pipeline".
#' @param method Method to execute.
#' @param ... Other arguments.
#' @return If \code{x} is a "workflow", then a list of class "workflow" with a new "target" for \code{method} in each "pipeline". If
#'        \code{x} is a "pipeline", then a pipeline with a new "target" for \code{method}.
#' @export
delay <- function(x, ...) UseMethod('delay')

#' @export
delay.default <- function(x, ...) stop('Cannot delay ', class(x))

#' @export
delay.workflow <- function(x, method, ...) {

  x$pipelines <- lapply(x$pipelines, delay, method=method, ...)

  return ( x )

}

#' @export
delay.pipeline <- function(x, method, ...) {

  input_file  <- NA
  output_file <- NA

  if ( x$checkpoint ) {

    input_file  <- x$targets[length(x$targets)]$output_file
    output_file <- paste(input_file, deparse(substitute(method)), 'checkpoint', sep='.')

  }

  new_target <- newTarget(method=method, input_file=input_file, output_file=output_file, ...)

  if ( length(x$targets) == 0 ) x$targets <- list(new_target)
  else  x$targets <- c(x$targets, list(new_target))

  return ( x )

}

#' @title Revert execution state to previous target
#' @description Sets targets as unbuilt, so running \code{execute} will re-build them.
#' @param x Object of class "workflow" or "pipeline".
#' @param n Integer. If negative, roll back by that number of targets. If positive, go to the target number.
#' @param ... Other arguments.
#' @return A "workflow", or "pipeline" with altered "target" build states.
#' @export
rollback <- function(x, ...) UseMethod('rollback')

#' @export
rollback.default <- function(x, ...) stop('Cannot roll back', class(x))

#' @export
rollback.workflow <- function(x,  n=-1, ...) {

  x$pipelines <- lapply(x$pipelines, rollback)

  return ( x )

}

#' @export
rollback.pipeline <- function(x,  n=-1, ...) {

  n_targets <- length(x$targets)

  target_progress <- 0

  for ( target in x$targets ) {

    if ( is_built(target) ) target_progress <- target_progress + 1
    else break

  }

  rollback_to <- ifelse(n < 0, max(target_progress + n, 0), n)

  println('Rolling back to target number', rollback_to)

  x$targets <- seq_along(x$targets) %>% lapply(function(i) {

    if ( i > rollback_to & is_built(x$targets[[i]]) ) x$targets[[i]]$is_built <- FALSE

    return ( x$targets[[i]] )

  })

  return ( x )

}

#' @title Checkpoint Pipeline to disk
#' @description Serializes Pipeline object under its \code{checkpoint_filename} attribute.
#' @param x Object of class "workflow" or "pipeline".
#' @param ... Other arguments.
#' @return A list of class "pipeline" invisibly.
#' @export
checkpoint <- function(x, ...) UseMethod('checkpoint')

#' @export
checkpoint.default <- function(x, ...) stop('Cannot checkpoint', class(x))

#' @export
checkpoint.pipeline <- function(x, ...) {

  if ( x$checkpoint & ! is_locked(x) ) {

    writeLines(x$lock_file, x$lock_file)
    println('Checkpoint: Saving temporary file', x$temp_file)
    saveRDS(x, x$temp_file)
    println('Checkpoint: Moving to checkpoint file', x$checkpoint_filename)
    file.rename(x$temp_file, x$checkpoint_filename)
    unlink(x$lock_file)

  } else {

    if ( is_locked(x) ) println('Warning: Checkpoint file', x$checkpoint_filename, 'is locked by another process')

  }

  return ( invisible(x) )

}

#' @title Check if Pipeline is locked by another process
#' @description Rerturns \code{TRUE} or \code{FALSE} depending on presence of Pipeline lockfile as defined in the \code{lock_file}
#' attribute.
#' @param x List of class "pipeline".
#' @param ... Other arguments.
#' @return \code{TRUE} or \code{FALSE} depending on presence of Pipeline lockfile.
#' @export
is_locked <- function(x, ...) UseMethod('is_locked')

#' @export
is_locked.default <- function(x, ...) stop('Cannot check lock on', class(x))

#' @export
is_locked.pipeline <- function(x, ...) file.exists(x$lock_file)

#' @title Check if Target is built
#' @description Returns \code{TRUE} or \code{FALSE} depending on whether target has been built.
#' @param x List of class "target".
#' @param ... Other arguments.
#' @return \code{TRUE} or \code{FALSE} depending on whether target has been built.
#' @export
is_built <- function(x, ...) UseMethod('is_built')

#' @export
is_built.default <- function(x, ...) stop('Cannot check build of', class(x))

#' @export
is_built.target <- function(x, ...) x$is_built

#' @title Write Workflow to disk.
#' @description Save serialized Workflow \code{x} as \code{filename}
#' @param x Object of class Workflow.
#' @param filename Character. Filename to serialize \code{x} under.
#' @param ... Other arguments.
#' @return The input Workflow, invisibly.
#' @seealso \link{saveRDS}
#' @export
write_workflow <- function(x, filename, ...) {

  println('Saving workflow as', filename)

  saveRDS(x, file=filename)

  return ( invisible(x) )

}

#' @title Load Workflow from disk
#' @description Load serialized Workflow from \code{filename}.
#' @param filename Character. Path to serialized Workflow.
#' @param ... Other arguments.
#' @return A list of class "workflow".
#' @seealso \link{readRDS}
#' @export
load_workflow <- function(filename, ...) {

  println('Loading', filename)

  x <- readRDS(filename)

  return ( x )

}
