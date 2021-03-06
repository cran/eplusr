% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/impl-idf.R
\name{get_idf_value}
\alias{get_idf_value}
\title{Get value data}
\usage{
get_idf_value(
  idd_env,
  idf_env,
  class = NULL,
  object = NULL,
  field = NULL,
  property = NULL,
  underscore = FALSE,
  ignore_case = FALSE,
  align = FALSE,
  complete = FALSE,
  all = FALSE
)
}
\arguments{
\item{idd_env}{An environment or list contains IDD tables including class,
field, and reference.}

\item{idf_env}{An environment or list contains IDF tables including object,
value, and reference.}

\item{class}{An integer vector of valid class indexes or a character vector
of valid class names. Default: \code{NULL}.}

\item{object}{An integer vector of valid object IDs or a character vector
of valid object names. Default: \code{NULL}.}

\item{field}{An integer vector of valid field indexes or a character
vector of valid field names (can be in in underscore style). \code{class}
and \code{field} should have the same length.}

\item{property}{A character vector of column names in field table to return.}

\item{underscore}{If \code{TRUE}, input class name will be converted into
underscore style name first and column \code{class_name_us} will be used
for matching. Default: \code{FALSE}.}

\item{ignore_case}{If \code{TRUE}, input object name will be converted into lower
case and column \code{object_name_lower} will be used for matching.
converted into underscore style name first and column \code{class_name_us}
and \code{field_name_us} will be used for matching. Default: \code{FALSE}.}

\item{align}{If \code{TRUE}, all objects in the same class will have the same
field number. The number of fields is the same as the object that have
the most fields among objects specified.  Default: \code{FALSE}.}

\item{complete}{If \code{TRUE}, at least fields till the current whole extensible
group will be returned. A new column named "matched_rleid" will be
created (when \code{property} is NULL) indicating if given field has been
matched or not.}

\item{all}{If \code{TRUE}, all available fields defined in IDD for the class that
objects belong to will be returned. Default: \code{FALSE}.}
}
\value{
A data.table containing specified columns.
}
\description{
Get value data
}
\keyword{internal}
