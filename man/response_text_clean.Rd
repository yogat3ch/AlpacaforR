% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/internal.R
\name{response_text_clean}
\alias{response_text_clean}
\title{Clean API responses}
\usage{
response_text_clean(x)
}
\arguments{
\item{x}{The response from our server GET request}
}
\value{
The response in a readable format as a list.
}
\description{
Clean the response text (usually in unreadable json) and convert to a readable format using \link[jsonlite]{fromJSON}.
}
\keyword{internal}
