% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/workflow-construction-methods.R
\name{is_built}
\alias{is_built}
\alias{is_built.default}
\alias{is_built.target}
\title{Check if Target is built}
\usage{
is_built(x, ...)

\method{is_built}{default}(x, ...)

\method{is_built}{target}(x, ...)
}
\arguments{
\item{x}{List of class "target".}

\item{...}{Other arguments.}
}
\value{
\code{TRUE} or \code{FALSE} depending on whether target has been built.
}
\description{
Returns \code{TRUE} or \code{FALSE} depending on whether target has been built.
}
