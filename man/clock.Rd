% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Clock.R
\name{clock}
\alias{clock}
\title{Get Clock function}
\usage{
clock(tz = Sys.getenv("TZ"))
}
\arguments{
\item{tz}{\code{(character)} A timezone (See \code{\link[base]{timezones}} or use \code{OlsonNames()})  to determine how the market hours compare to the \code{tz} hours. If no \code{tz} argument is provided, and the local timezone differs from "America/New_York", \code{clock} will automatically provide the local conversion and offset.}
}
\value{
Clock \code{(list)} A \href{https://alpaca.markets/docs/api-documentation/api-v2/clock/#clock-entity}{Clock} Object:
\itemize{
\item{\code{timestamp}}{ \code{(POSIXct)} Current timestamp.}
\item{\code{is_open}}{ \code{(logical)} Whether or not the market is open as a boolean.}
\item{\code{next_open}}{ \code{(POSIXct)} Next market open timestamp as a string.}
\item{\code{next_close}}{ \code{(POSIXct)} Next market close timestamp as a string.}
}
}
\description{
The clock API serves the current market timestamp, whether or not the market is currently open, as well as the times of the next market open and close. See \href{https://alpaca.markets/docs/api-documentation/api-v2/clock/}{Clock} Endpoint for details. \emph{Additionally, if your current timezone is different than that of the market} or if a \code{tz} is specified, a \code{local} POSIXct will be appended with corresponding local time (or \code{tz} time) and it's \code{offset} from the market for each of the following outputs: \code{timestamp}, \code{next_open}, and \code{next_close}.
}
\examples{
\dontrun{
clock()
}
}
\concept{Clock}
