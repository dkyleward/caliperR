% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/objects.R
\name{CaliperClass}
\alias{CaliperClass}
\title{R class representing GISDK objects}
\description{
R class representing GISDK objects

R class representing GISDK objects
}
\details{
This class simplifies working with GISDK objects over COM from R.

When created, an object of this class contains a COM pointer to a sister
object in Caliper software. In addition to this reference, contained in the
\code{ref} field, there are several other fields and methods to facilitate
object manipulation. Finally, the \code{$} operator has been overloaded for
this class to make code easier to write.
}
\examples{
\dontrun{
# object creation (two options)
obj <- CreateObject("NLM.Model")
obj <- CaliperClass$new("NLM.Model")

# get info about the GISDK object fields and methods
obj$info

# set an attribute/field label of the GISDK object
# (Does not modify the R object in any way)
obj$Label <- "a logit model"

# run one of the GISDK object's methods: Clear()
# (Does not modify the R object in any way)
obj$Clear()

# object methods can be chained
obj$
 Clear()$
 Read()
}
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{g_class_name}}{The class name of the underlying GISDK object
(e.g. "NLM.Model").}

\item{\code{ref}}{This is a COM pointer and represents the object created in a
Caliper program.}

\item{\code{info}}{This is a simple R list that can be used to see the fields
and methods of the underlying GISDK object.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-new}{\code{CaliperClass$new()}}
\item \href{#method-apply_gisdk_method}{\code{CaliperClass$apply_gisdk_method()}}
\item \href{#method-get_gisdk_attribute}{\code{CaliperClass$get_gisdk_attribute()}}
\item \href{#method-set_gisdk_attribute}{\code{CaliperClass$set_gisdk_attribute()}}
\item \href{#method-clone}{\code{CaliperClass$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-new"></a>}}
\if{latex}{\out{\hypertarget{method-new}{}}}
\subsection{Method \code{new()}}{
Create a new \code{CaliperClass}
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{CaliperClass$new(class_name, ...)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{class_name}}{\code{string} The name of the GISDK class to create.}

\item{\code{...}}{Additional arguments to be supplied to GISDK object creation.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A new \code{CaliperClass} object.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-apply_gisdk_method"></a>}}
\if{latex}{\out{\hypertarget{method-apply_gisdk_method}{}}}
\subsection{Method \code{apply_gisdk_method()}}{
Runs a method of the associated GISDK object.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{CaliperClass$apply_gisdk_method(method, ...)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{method}}{\code{string} The name of the method to apply.}

\item{\code{...}}{Additional arguments supplied to the GISDK method.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-get_gisdk_attribute"></a>}}
\if{latex}{\out{\hypertarget{method-get_gisdk_attribute}{}}}
\subsection{Method \code{get_gisdk_attribute()}}{
Retrieve an attribute from the GISDK object.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{CaliperClass$get_gisdk_attribute(attribute)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{attribute}}{\code{string} The name of the GISDK attribute to get.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
The GISDK attribute value.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-set_gisdk_attribute"></a>}}
\if{latex}{\out{\hypertarget{method-set_gisdk_attribute}{}}}
\subsection{Method \code{set_gisdk_attribute()}}{
Set an attribute of the GISDK object.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{CaliperClass$set_gisdk_attribute(attribute, value)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{attribute}}{\code{string} The name of the GISDK attribute to get.}

\item{\code{value}}{The value to assign to the attribute.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-clone"></a>}}
\if{latex}{\out{\hypertarget{method-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{CaliperClass$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
