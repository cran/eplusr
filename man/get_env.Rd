% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{get_env}
\alias{get_env}
\alias{get_self_env}
\alias{get_priv_env}
\alias{get_super_env}
\title{Get the enclosed environment of an R6 object}
\usage{
get_self_env(x)

get_priv_env(x)

get_super_env(x)
}
\arguments{
\item{x}{An R6 object.}
}
\value{
An environment.
}
\description{
Get the enclosed environment of an R6 object
}
\details{
\code{get_super_env()} returns the \code{super} enclosed environment of an \code{\link[R6:R6Class]{R6::R6Class()}}
object.

\code{get_self_env()} returns the \code{self} enclosed environment of an \code{\link[R6:R6Class]{R6::R6Class()}}
object.

\code{get_priv_env()} returns the \code{private} enclosed environment of an \code{\link[R6:R6Class]{R6::R6Class()}}
object.
}
\keyword{internal}
