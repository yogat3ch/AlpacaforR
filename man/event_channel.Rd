% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Websockets.R
\name{event_channel}
\alias{event_channel}
\title{Get the event channel}
\usage{
event_channel(msg)
}
\arguments{
\item{msg}{\code{(msg/tbl)} The \code{msg} tibble object returned from \href{../../AlpacaforR/html/msg.html}{\code{msg()}} which consists of the following:
\itemize{
\item{\code{ts}}{ \code{(POSIXct)} timestamp}
\item{\code{socket}}{ \code{(character)} the socket over which the message arrived}
\item{\code{channel}}{ \code{(character)} the channel over which the message arrived}
\item{\code{...}}{ \code{(varies)} all fields encoded in the event data JSON}
}}
}
\value{
\code{(character)} the channel
}
\description{
Given a \code{msg} object return the channel from which it came
}
\keyword{Internal}
