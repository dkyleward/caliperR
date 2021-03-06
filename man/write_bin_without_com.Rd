% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/convert_bin.R
\name{write_bin_without_com}
\alias{write_bin_without_com}
\title{Write a FFB (.bin) file without a COM connection}
\usage{
write_bin_without_com(binData, binFilename, dcbKey)
}
\arguments{
\item{binData}{\code{data.frame} The data table to write}

\item{binFilename}{\code{string} The file in which to write}

\item{dcbKey}{The FFB table structure. This is create by \code{\link{write_bin}}}
}
\description{
This function allows R to write directly to a FFB file even if Caliper
software is not installed on the machine. Helper function for
\code{\link{write_bin}}.
}
\keyword{internal}
