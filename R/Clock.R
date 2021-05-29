
# clock ----
# Wed Apr 22 20:41:52 2020
#' @family Clock
#' @title Get Clock function
#' 
#' @description The clock API serves the current market timestamp, whether or not the market is currently open, as well as the times of the next market open and close. See [Clock](https://alpaca.markets/docs/api-documentation/api-v2/clock/) Endpoint for details. *Additionally, if your current timezone is different than that of the market* or if a `tz` is specified, a `local` POSIXct will be appended with corresponding local time (or `tz` time) and it's `offset` from the market for each of the following outputs: `timestamp`, `next_open`, and `next_close`.
#' @param tz `(character)` A timezone (See \code{\link[base]{timezones}} or use `OlsonNames()`)  to determine how the market hours compare to the `tz` hours. If no `tz` argument is provided, and the local timezone differs from "America/New_York", `clock` will automatically provide the local conversion and offset. 
#' @return Clock \code{(list)} A [Clock](https://alpaca.markets/docs/api-documentation/api-v2/clock/#clock-entity) Object:
#' \itemize{
#'  \item{\code{timestamp}}{ \code{(POSIXct)} Current timestamp.}
#'  \item{\code{is_open}}{ \code{(logical)} Whether or not the market is open as a boolean.}
#'  \item{\code{next_open}}{ \code{(POSIXct)} Next market open timestamp as a string.}
#'  \item{\code{next_close}}{ \code{(POSIXct)} Next market close timestamp as a string.}
#'  }
#' @examples 
#' clock()
#' @importFrom httr GET
#' @importFrom lubridate as_datetime with_tz is.POSIXct force_tz `.__T__-:base`
#' @importFrom purrr map_if
#' @export
clock <- function(tz = NULL){
  .tz <- ifelse(is.null(tz), Sys.timezone(), tz)
  #Set URL & Headers
  
  headers = get_headers()
  `-` <- lubridate::`.__T__-:base`
  #Send Request
  cl = httr::GET(url = get_url("clock"), headers)
  cl = response_text_clean(cl)
  suppressMessages({
  cl <- purrr::map_if(cl, is.character, lubridate::as_datetime, tz = "America/New_York")
  if (!grepl("New_York", .tz))
  cl <- purrr::map_if(cl, lubridate::is.POSIXct, ~{
    out <- list(market = .x, local = lubridate::with_tz(.x, tzone = .tz))
    out$offset <-  lubridate::hours(out$market - lubridate::force_tz(out$local, "America/New_York"))
    return(out)
    })
  })
  return(cl)
}


