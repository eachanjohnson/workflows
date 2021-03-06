% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/workflow-manipulation-methods.R
\name{gather}
\alias{gather}
\alias{gather.default}
\alias{gather.workflow}
\title{Gather data into one pipeline}
\usage{
gather(x, ...)

\method{gather}{default}(x, ...)

\method{gather}{workflow}(x, elements, by, ...)
}
\arguments{
\item{x}{Object of class "workflow".}

\item{...}{Other arguments.}

\item{elements}{Elements of \code{data} to combine into final pipeline.}

\item{by}{Not used.}
}
\value{
A list of class "workflow" containing one "pipeline".
}
\description{
If a "workflow" contains several pipelines, combine into a single pipeline. This pipeline has attribiutes from the
first pipeline of the input, except the \code{elements}, which are row-bound together.
}
\seealso{
\code{\link{bind}}
}
