% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Market_Data.R
\name{bars_getv2}
\alias{bars_getv2}
\title{GET url with the v2 & polygon API}
\usage{
bars_getv2(
  .url,
  v,
  evar = get0("evar", mode = "environment", envir = rlang::caller_env())
)
}
\arguments{
\item{.url}{\code{(character)} The url to retrieve}

\item{v}{\code{(integer)} The API version number.
\itemize{
\item{\code{1}}{ the \code{v1} \href{https://alpaca.markets/docs/api-documentation/api-v2/market-data/#endpoint}{IEX/Alpaca API}: data.alpaca.markets/v1 will be used}
\item{\code{2}}{ the \code{v2} \href{https://alpaca.markets/docs/api-documentation/api-v2/market-data/alpaca-data-api-v2/}{Alpaca V2 API}: data.alpaca.markets/v2 endpoint will be used.}
\item{\code{"p"/"polygon"}}{ the \href{https://polygon.io/docs/#get_v2_aggs_ticker__ticker__range__multiplier___timespan___from___to__anchor}{Aggregates Endpoint}: api.polygon.io/v2/aggs will be used. }
}}

\item{evar}{An environment that holds variable constants necessary for sub-functions.}
}
\value{
\code{(tibble)} data after retrieving all pages
}
\description{
GETs the API query for Polygon and Alpaca version 2 API's
}
\keyword{Internal}
