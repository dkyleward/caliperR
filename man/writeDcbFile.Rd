% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/convert_bin.R
\name{writeDcbFile}
\alias{writeDcbFile}
\title{Write data table's dcb file}
\usage{
writeDcbFile(dcbKey, dcbFilename, description = "", dnames = NA)
}
\arguments{
\item{dcbKey}{The dcb data table to write in dcbFilename}

\item{description}{\code{string} A description of the binData}

\item{dnames}{\code{Named vector} of display names where each name is a
column name and each value is the display name to use. For example:
\code{c("hh" = "Households")} will assign the display name "Households" to
column "hh".}
}
\description{
To write the data table into a TransCAD binary file (.bin),
we must first write a key in a .dcb file
}
\keyword{internal}
