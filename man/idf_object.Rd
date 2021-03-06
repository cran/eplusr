% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/idfobj.R
\name{idf_object}
\alias{idf_object}
\title{Create an \code{IdfObject} object.}
\usage{
idf_object(parent, object = NULL, class = NULL)
}
\arguments{
\item{parent}{An \link{Idf} object.}

\item{object}{A valid object ID (an integer) or name (a string). If \code{NULL}
and \code{class} is not \code{NULL}, an empty \link{IdfObject} is created with all fields
fill with default values if possible. Default: \code{NULL}.}

\item{class}{A valid class name (a string). If \code{object} is not \code{NULL},
\code{class} is used to further specify what class is the target object belongs
to. If \code{object} is \code{NULL}, an empty \link{IdfObject} of \code{class} is created.}
}
\value{
An \link{IdfObject} object.
}
\description{
\code{idf_object()} takes a parent \code{Idf} object, an object name or class name, and
returns a corresponding \link{IdfObject}.
}
\details{
If \code{object} is not given, an empty \link{IdfObject} of specified class is created,
with all field values filled with defaults, if possible.  Note that
validation is performed when creating, which means that an error may occur if
current \link[=level_checks]{validate level} does not allow empty required fields.

The empty \link{IdfObject} is directly added into the parent \link{Idf} object. It is
recommended to use \verb{$validate()} method in \link{IdfObject} to see what kinds of
further modifications are needed for those empty fields and use \verb{$set()}
method to set field values.
}
\examples{
\dontrun{
model <- read_idf(system.file("extdata/1ZoneUncontrolled.idf", package = "eplusr"))

# get an IdfObject using object ID
idf_object(model, 14)

# get an IdfObject using object name (case-insensitive)
idf_object(model, "zone one")

# `class` argument is useful when there are objects with same name in
# different class
idf_object(model, "zone one", "Zone")

# create a new zone
eplusr_option(validate_level = "draft")
zone <- idf_object(model, class = "Zone")
zone
eplusr_option(validate_level = "final")
zone$validate()
}
}
