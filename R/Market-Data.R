
# market_data ----
# Wed Apr 22 21:40:57 2020
#' @family Market-Data
#' @title Get Market Data
#' 
#' @description The bars API provides time-aggregated price and volume data for a single stock or multiple. **The `'v2'` (Polygon) API is only available for live accounts and accepts the `from`, `to`, `timeframe`, `multiplier`, and `unadjusted` arguments.**
#' @param ticker \code{(character)} The stock or stocks (in vector format) that you want.
#' @param v \code{(integer)} The API version number. If `1`, the `'v1'` \href{https://alpaca.markets/docs/api-documentation/api-v2/market-data/#endpoint}{IEX/Alpaca API}: data.alpaca.markets/v1 will be used, if `2`, the `'v2'` \href{https://alpaca.markets/docs/api-documentation/api-v2/market-data/#polygon-integration}{Polygon/Alpaca API}: api.polygon.io/v2/aggs \href{https://polygon.io/docs/#get_v2_aggs_ticker__ticker__range__multiplier___timespan___from___to__anchor}{Aggregates Endpoint}  will be used. 
#' @param timeframe \code{(character)} For the `'v1'` API, one of
#' \itemize{
#'  \item{`'m'`/`'min'`/`'minute'`}{ (`multiplier` can be `1`/`5`/`15`)}
#'  \\item{`'d'`/`'day'`}{ (`multiplier` will be `1`)}
#' } 
#' Not case-sensitive.
#' For the `'v2'` API, `multiplier` can be any `integer` for any one of the following `timeframe`'s:
#' \itemize{
#'  \item{`'m'`/`'min'`/`'minute'`}
#'  \item{`'h'`/`'hour'`}
#'  \item{`'d'`/`'day'`}
#'  \item{`'w'`/`'week'`}
#'  \item{`'M'`/`'mo'`/`'month'`}{ (*Note* capitalized M for month)}
#'  \item{`'q'`/`'quarter'`}
#'  \item{`'y'`/`'year'`} 
#' } 
#' Case-sensitive
#' @param multiplier For the `'v1'` API, with `'minute'` `timeframe` one of `1`/`5`/`15`. Otherwise, defaults to `1`.
#' For the `'v2'` API, this can be any `integer`, also defaults to `1`.
#' @param from (equivalent to `start` in `'v1'`) \code{(Date/POSIXlt/Datetime(POSIXct)/character)} See Details for formatting guidelines. Return data *equal to or after* this time. Default is 7 days ago. 
#' @param to (equivalent to `end` in `'v1'`) \code{(Date/POSIXlt/Datetime(POSIXct)/character)} See Details for formatting guidelines. Return data *equal to or before* this time. Default is today's date.
#' @param after *`'v1'` only* \code{(Date/POSIXlt/Datetime(POSIXct)/character)} See Details for formatting guidelines. Return data *after* this time. Default is 7 days ago. *Cannot be used with \code{from}*
#' @param until *`'v1'` only* \code{(Date/POSIXlt/Datetime(POSIXct)/character)} See Details for formatting guidelines. Return data *before* this time. Default is today's date. *Cannot be used with \code{from}*
#' @param limit *v1 only* \code{(integer)} The amount of bars to return per ticker. This can range from 1 to 1000. Defaults to 1000. *Note:* If \code{full} is set to T, this parameter is ignored and forced to 1000. 
#' @param full \code{(logical)} If TRUE, the function will attempt to return the entire expected dataset based on the range of dates provided and perform a data completeness check. If the requested from, to dates/times exceed that which can be returned in a single call, the API will be called repeatedly to return the **full** dataset. If FALSE, the request will be submitted to the API as is. *Note:* The `'v1'` API has a call limit of 1000 bars and a rate limit of 200 requests per minute. If the rate limit is reached, queries will pause for 1 minute. Defaults to FALSE.
#' @param unadjusted *'v2' only* \code{(logical)} Set to `TRUE` if the results should **NOT** be adjusted for splits. Defaults to `FALSE`.
#' @details All \code{(Date/POSIXlt)} must be in `YYYY-MM-DD` \href{https://www.iso.org/iso-8601-date-and-time-format.html}{RFC 3339} format. If `(Datetime/POSIXct)`, `YYYY-MM-DD HH:MM` \href{https://www.iso.org/iso-8601-date-and-time-format.html}{ISO8601} format. All Dates/Datetimes are forced to America/New York timezone (See \code{\link[lubridate]{force_tz}}) in which the NYSE operates. This means that if \code{\link[lubridate]{lubridate::now}} is used to specify 3PM in the local timezone, it will be forced 3PM in the "America/New_York timezone. This eliminates needing to consistently account for timezone conversions when providing inputs. The `'v2'` API only accepts Dates in YYYY-MM-DD format, any arguments passed to `start` or `end` will be coerced to Date automatically if using `'v2'`. For the `'v2'` API, queries with `timeframe`: `'year'` use `12/31` as an aggregate date for each year. Arguments passed to `from` & `to` will be coerced to their yearly \code{\link[lubridate]{round_date}} and \code{\link[lubridate]{ceiling_date}} respectively.
#' @return \code{list} object for each ticker symbol containing a \code{data.frame} with the following columns:
#' \itemize{
#'  \item{\code{time}}{  the time of the bar as \code{POSIXct} in yyyy-mm-dd for timeframe = day, and yyyy-mm-dd hh:mm:ss for timeframes < day}
#'  \item{\code{open}}{ \code{(numeric)} open price}
#'  \item{\code{high}}{ \code{(numeric)} high price}
#'  \item{\code{low}}{ \code{(numeric)} low price}
#'  \item{\code{close}}{ \code{(numeric)} close price}
#'  \item{\code{volume}}{ \code{(numeric)} volume (in millions)}
#'  \item{\code{n *'v2' only*}}{ \code{(numeric)} Number of items in aggregate window}
#' }
#' Additionally, a "query" attribute is attached to each ticker's `data.frame` with the "query" data as a `list` for each of the calls required to return it. This is accessed via \code{\link[base]{attr}}. Each call is a list with the following items:
#' \itemize{
#'  \item{\code{ticker}}{ Ticker symbol requested.}
#'  \item{\code{status}}{ The API status code for the response in English}
#'  \item{\code{queryCount}}{ Number of aggregate ( min or day ) used to generate the response.}
#'  \item{\code{resultsCount}}{ Total number of results generated}
#'  \item{\code{adjusted}}{ If this response was adjusted for splits}
#'  \item{\code{status_code}}{ The API status code as an integer.}
#' }
#' @examples
#' # Getting one or more tickers from the v1 API: 
#' market_data(ticker = c("INTC","MSFT"))
#' # Getting price data with specific date ranges and timeframes, by also limiting the amount of bars returned for each ticker.
#' market_data(ticker = c("INTC","MSFT"), from = "2019-03-20", to = "2019-04-01", multiplier = 15, timeframe = "min", limit = 175)
#' # Get Year-to-Date hourly data for Beyond Meat from the v2 Polygon API when full = F:
#' bars <- market_data(ticker = c("BYND"), v = 2, from = lubridate::floor_date(lubridate::now(), "year"), multiplier = 1, timeframe = "h")
#' # Returns successfully
#' purrr::map(bars, nrow) # Note the number of rows
#' purrr::map(bars, ~range(.x$time)) # Note that it appears all the data is present
#' # Let's take a closer look:
#' plot(open ~ time, bars$BYND, xaxt = "n", type = "l")
#' axis(1, bars$BYND$time, format(bars$BYND$time, "%b %d"), cex.axis = .7)
#' # Only about 10% of the requested data is present. Does the query status metadata indicate anything is wrong?
#' attr(bars$BYND, "query") # everything checks out, so how do we get all the data we requested?
#' # Set full = T
#' bars <- market_data(ticker = c("BYND"), v = 2, from = lubridate::floor_date(lubridate::now(), "year"), multiplier = 1, timeframe = "h", full = TRUE)
#' purrr::map(bars, ~nrow) # A big difference in the number of rows
#' purrr::map(bars, ~range(.x$time)) # The range appears the same
#' # A closer look:
#' plot(open ~ time, bars$BYND, xaxt = "n", type = "l")
#' axis(1, bars$BYND$time, format(bars$BYND$time, "%b %d"), cex.axis = .7)
#' # That's more like it!
#' @importFrom stringr str_detect str_extract regex
#' @importFrom purrr map_chr map_lgl keep
#' @importFrom rlang abort warn
#' @importFrom magrittr `%>%`
#' @export

# Generate the list in the docs:
# list(m = c("m","min","minute"), h = c("h", "hour"),d = c("d", "day"), w = c("w", "week"), m = c("M", "mo", "month"), q = c("q", "quarter"), y = c("y", "year")) %>% purrr::map(~{paste0("\\item{",paste0(paste0("`'",.x,"'`"), collapse = '/'),"}")}) %>% do.call(c,.) %>% cat(collapse = "\n")
# For DEBUG


market_data <- function(ticker, v = 1, timeframe = "day", multiplier = 1, from = Sys.Date()-7, to = Sys.Date(), after = NULL, until = NULL, limit = NULL, full = F, unadjusted =  F){
  `%>%` <- magrittr::`%>%`
  #param check:  Thu Mar 26 08:34:13 2020 ----
  if((is.null(from) || is.null(to)) && (is.null(start) || is.null(end))){
    stop("Please enter either 'from' & 'to' or 'after' & 'until' arguments.")
  }
  if (!any(v %in% 1:2)) stop("Version 'v' must be either 1 or 2")
  if (stringr::str_detect(timeframe, "^\\d")) {
    # Account for old argument style to timeframe
    multiplier <- as.numeric(stringr::str_extract(timeframe, "^\\d+"))
    timeframe <- tolower(stringr::str_extract(timeframe, "\\w+"))
  }
  #quick detection of timespan abbreviations:  Thu Mar 26 08:34:00 2020 ----
  .tf_opts <- list(m = c("m","min","minute"), h = c("h", "hour"), d = c("d", "day"), w = c("w", "week"), M = c("M", "mo", "month"), q = c("q", "quarter"), y = c("y", "year"))
  # Create ordered factor or timeframe options
  .tf_order <- purrr::map_chr(.tf_opts, tail, 1) %>% {factor(., levels = .)}
  
  if (v == 1) {
    # Get the timeframe
    timeframe <- tail(.tf_opts[c(1,3)][[grep(stringr::regex(timeframe, ignore_case = T), .tf_opts[c(1,3)], ignore.case = T)]], 1)
    
    # Check args
    if (timeframe == "minute" && !any(multiplier %in% c(1,5,15))) {
      rlang::abort("The v1 API only accepts multipliers of 1,5,15 for the minute timeframe")
    } else if (timeframe == "day" && multiplier != 1) {
      rlang::warn("The v1 API only accepts 1 as a `multiplier` for the day timeframe. One day bars will be returned.", class = "warning")
      multiplier <- 1
    }
    
  } else if (v == 2){
    timeframe <- tail(.tf_opts[[grep(timeframe, .tf_opts, ignore.case = F)[1]]], 1)
  }
  # Get the timeframe as a numeric
  .tf_num <- which(.tf_order %in% timeframe)
  # Handle date bounds:  Thu Mar 26 08:40:24 2020 ----
  .bounds <- bars_bounds(from = from, to = to, after = after, until = until)
  
  message(paste0("Floor/Ceiling dates are necessary to retrieve inclusive aggregates\n'from' coerced to ", .bounds[[1]], "\n'to' coerced to ", .bounds[[2]]))
  # Stop if malformed date argument with informative message
  if (any(purrr::map_lgl(.bounds, is.na))) rlang::abort(paste0("Check the following argument(s) format: `", names(purrr::keep(.bounds, ~{is.null(.x)||is.na(.x)})), "`"))
  
  if (full) {
    # Form the URL
    url <- bars_url(.bounds = .bounds, ticker = ticker)
    # retrieve the data
    bars <- bars_get(url)
    .expected <- bars_expected(bars)
    .missing <- bars_missing(bars)
    bars <- bars_complete(bars, .missing = .missing)
  } else {
    # Submit request as is for full = F:  Tue Mar 17 21:35:54 2020 ----
    # Coerce to ISO8601 for call
    # build the URL
    url <- bars_url(ticker)
    # retrieve the data
    bars <- bars_get(url)
    # End case where full = F ---- Fri Mar 27 11:19:05 2020
  }
  return(bars)
}
#----------------------------------------------------------------------------------------------
#market_data(ticker = "DBX")

#' @family Market-Data
#' @rdname market_data
#' @title get_bars
#' @description `get_bars` is deprecated, use \code{\link[AlpacaforR]{market_data}} instead.
#' @examples get_bars("AAPL")
#' @export

get_bars <- market_data

