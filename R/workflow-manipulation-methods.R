#' @title  Split pipeline data into parallel chunks
#' @description Split pipeline data into parallel chunks by some filtering condition. Each unique value contained in
#' column \code{by} in the \code{elements} of the \code{data} attribute of each "pipeline" is split into a separate pipeline.
#' All other attributes are copied.
#' @param x Object of class "workflow" or "pipeline".
#' @param elements Elements of \code{data} attribute to look for matches.
#' @param by Column of \code{elements} to chunk on. Each unique value contained in column \code{by} is split into a separate pipeline.
#' @param ... Other arguments.
#' @return If \code{x} is a "workflow", then a list of class "workflow" containing one "pipeline" for each value of \code{by}. If
#'        \code{x} is a "pipeline", then a list of "pipeline"s.
#' @export
scatter <- function(x, ...) UseMethod('scatter')

#' @export
scatter.default <- function(x, ...) stop('Cannot scatter ', class(x))

#' @export
scatter.workflow <- function(x, elements, by, ...) {

  new_pipelines <- lapply(x$pipelines, scatter, elements, by)

  x$pipelines <- unlist(new_pipelines, recursive=FALSE)

  return ( x )

}

#' @export
scatter.pipeline <- function(x, elements, by, ...) {

  original_class <- class(x$data)

  skeleton_pipeline <- x[-which(names(x) == 'data')]

  skeleton_pipeline_data <- x$data#[-which(names(y$data) %in% elements)]

  these_elements_to_chunk <-  x$data[elements]

  chunked_elements <- setNames(lapply(these_elements_to_chunk,
                                      function(y) split(y, getElement(y, by))), elements)

  n_chunks <- length(chunked_elements[[1]])

  new_pipeline_datas <- lapply(seq_len(n_chunks), function(i) {

    new_pipeline_data <- skeleton_pipeline_data


    for ( element in elements ) {

      new_pipeline_data[[element]] <- chunked_elements[[element]][[i]]

    }

    return ( structure(new_pipeline_data, class=original_class) )
  })

  new_pipelines <- lapply(new_pipeline_datas, function(z) {

    skeleton_pipeline$data                <- z
    skeleton_pipeline$scattered_by        <- by
    skeleton_pipeline$scattered_value     <- get_unique_values(getElement(z, elements[1]), by)[1]
    skeleton_pipeline$checkpoint_filename <- paste(skeleton_pipeline$checkpoint_filename,
                                                   skeleton_pipeline$scattered_by,
                                                   skeleton_pipeline$scattered_value, sep='-')
    skeleton_pipeline$temp_file <- paste0(skeleton_pipeline$checkpoint_filename, '.temp')
    skeleton_pipeline$lock_file <- paste0(skeleton_pipeline$checkpoint_filename, '.lock')

    return (structure(skeleton_pipeline, class='pipeline'))

  })

  return (new_pipelines)

}

#' @title Gather data into one pipeline
#' @description If a "workflow" contains several pipelines, combine into a single pipeline. This pipeline has attribiutes from the
#' first pipeline of the input, except the \code{elements}, which are row-bound together.
#' @param x Object of class "workflow".
#' @param elements Elements of \code{data} to combine into final pipeline.
#' @param by Not used.
#' @param ... Other arguments.
#' @return A list of class "workflow" containing one "pipeline".
#' @seealso \code{\link{bind}}
#' @export
gather <- function(x, ...) UseMethod('gather')

#' @export
gather.default <- function(x, ...) stop('Cannot gather ', class(x))

#' @export
gather.workflow <- function(x, elements, by, ...) {

  if ( length(x$pipelines) > 1 ) {

    x$pipelines <- structure(list(Reduce(function(z1, z2) bind(z1, z2, elements=elements), x$pipelines)), class='pipeline')

  }

  x$pipelines[[1]]$scattered_by    <- 'all'
  x$pipelines[[1]]$scattered_value <- ''

  return ( x )

}

#' @title Combine two pipelines into one
#' @description Combine two pipelines into a single pipeline. This pipeline has attribiutes from the
#' first pipeline of the input, except the \code{elements}, which are row-bound together.
#' @param x Object of class "pipeline".
#' @param elements Elements of \code{data} attribute to combine.
#' @param by Not used.
#' @param ... Other arguments.
#' @return A list of class "pipeline".
#' @export
bind <- function(x, ...) UseMethod('bind')

#' @export
bind.default <- function(x, ...) stop('Cannot bind ', class(x))

#' @importFrom magrittr %>% %<>%
#' @export
bind.pipeline <- function(x1, x2, elements, ...) {

  original_class <- class(x1$data)

  skeleton_pipeline <- x1#[-which(names(x1) == 'data')]

  new_pipeline_data <- lapply(setNames(names(x1$data), names(x1$data)), function(n) {

    #print(n)

    if ( n %in% elements ) {

      if ('data.frame' %in% class(getElement(x1$data, n))) {

        df1 <- getElement(x1$data, n)
        df2 <- getElement(x2$data, n) %>% dplyr::mutate_(.dots=setNames(paste0('"', x2$scattered_value, '"'),
                                                                 x2$scattered_by))

        #print(tail(df1))
        #print(head(df2))
        if ( ! x1$scattered_by %in% names(df1) )
          df1 <- df1 %>% dplyr::mutate_(.dots=setNames(paste0('"', x1$scattered_value, '"'), x1$scattered_by))

        df1_classes <- sapply(df1, class)
        df1_factors <- names(df1_classes)[df1_classes == 'factor']
        df1_factors <- intersect(names(df2), df1_factors)

        #print(df1_factors)

        if ( length(df1_factors) > 0 ) df2 <- df2 %>% dplyr::mutate_(.dots=paste0('factor(', df1_factors, ')') %>% setNames(df1_factors))

        new_df <- dplyr::bind_rows(df1, df2)


        #print(new_df)

        return ( new_df )

      } else {

        return ( c(getElement(x1$data, n), getElement(x2$data, n)) )

      }


    } else {

      return ( getElement(x1$data, n) )

    }

  }) %>% structure(class=original_class)

  #print(str(new_pipeline_data))

  skeleton_pipeline$data <- new_pipeline_data

  skeleton_pipeline$checkpoint_filename <- file.path(skeleton_pipeline$checkpoint_directory,
                                                   paste0('checkpoint', skeleton_pipeline$checkpoint_id))
  skeleton_pipeline$temp_file <- paste0(skeleton_pipeline$checkpoint_filename, '.temp')
  skeleton_pipeline$lock_file <- paste0(skeleton_pipeline$checkpoint_filename, '.lock')

  return ( structure(skeleton_pipeline, class='pipeline') )

}
