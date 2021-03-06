% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/GisdkClass.R
\name{$.GisdkClass}
\alias{$.GisdkClass}
\title{S3 method for calling \code{GisdkClass} object methods}
\usage{
\method{$}{GisdkClass}(x, name, ..., process_result = TRUE)
}
\arguments{
\item{x}{A \code{GisdkClass} object}

\item{name}{The method to dispatch}

\item{...}{The arguments to pass to the method.}

\item{process_result}{\code{boolean} Whether to attempt to process the
result into a native R format.}
}
\description{
Makes \code{GisdkClass} objects smarter about whether you are calling a
method from the R object or the underlying GISDK library.
}
\details{
If \code{name} is a method of the R object, then it is executed. Otherwise,
it tries to execute a GISDK function of the same name.
}
