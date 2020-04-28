
# clock ----
# Wed Apr 22 20:41:52 2020
#' @family Clock
#' @title Get Clock function
#' 
#' @description The clock API serves the current market timestamp, whether or not the market is currently open, as well as the times of the next market open and close. See [Clock](https://alpaca.markets/docs/api-documentation/api-v2/clock/) Endpoint for details.
#' @inheritParams account
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
#' @importFrom lubridate as_datetime
#' @importFrom purrr map_if
#' @export
clock <- function(v = 2){
  #Set URL & Headers
  url = get_url()
  headers = get_headers()
  
  #Send Request
  cl = httr::GET(url = paste0(url,"/",paste0("v",v),"/clock"), headers)
  cl = response_text_clean(cl)
  cl <- purrr::map_if(cl, is.character, lubridate::as_datetime, tz = Sys.timezone())
  return(cl)
}
#----------------------------------------------------------------------------------------------
#' @family Clock
#' @rdname clock
#' @title get_clock (Deprecated)
#' @description `get_clock` is deprecated, use \code{\link[AlpacaforR]{clock}} instead.
#' @examples get_clock()
#' @export
get_clock <- clock

