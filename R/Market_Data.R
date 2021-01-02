
# market_data ----
# Wed Apr 22 21:40:57 2020
#' @family Market-Data
#' @title Get Market Data
#' 
#' @description The bars API provides time-aggregated price and volume data for a single stock or multiple. **The `v2` (Polygon) API is only available for live accounts and accepts the `from`, `to`, `timeframe`, `multiplier`, and `unadjusted` arguments.**
#' @param ticker \code{(character)} The stock or stocks (in vector format) for which data will be retrieved. Non case-sensitive.
#' @param v \code{(integer)} The API version number. If `1`, the `v1` \href{https://alpaca.markets/docs/api-documentation/api-v2/market-data/#endpoint}{IEX/Alpaca API}: data.alpaca.markets/v1 will be used, if `2`, the `v2` \href{https://alpaca.markets/docs/api-documentation/api-v2/market-data/#polygon-integration}{Polygon/Alpaca API}: api.polygon.io/v2/aggs \href{https://polygon.io/docs/#get_v2_aggs_ticker__ticker__range__multiplier___timespan___from___to__anchor}{Aggregates Endpoint}  will be used. 
#' @param timeframe \code{(character)} For the `v1` API, one of
#' \itemize{
#'  \item{`'t'/'lt'/'trade'`}{ For the last trade price. See \href{https://alpaca.markets/docs/api-documentation/api-v2/market-data/last-trade/}{Last Trade}}
#'  \item{`'q'/'lq'/'quote'`}{ For the last quote price. See \href{https://alpaca.markets/docs/api-documentation/api-v2/market-data/last-quote/}{Last Quote}}
#'  \item{`'m'`/`'min'`/`'minute'`}{ (`multiplier` can be `1`/`5`/`15`)}
#'  \item{`'d'`/`'day'`}{ (`multiplier` will be `1`)}
#' } 
#' Not case-sensitive.
#' For the `v2` API, `multiplier` can be any positive `integer` for any one of the following `timeframe`'s:
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
#' @param multiplier For the `v1` API, with `'minute'` `timeframe` one of `1`/`5`/`15`. Otherwise, defaults to `1`.
#' For the `v2` API, this can be any `integer`, also defaults to `1`.
#' @param from (equivalent to `start` in `v1`) \code{(Date/POSIXlt/Datetime(POSIXct)/character)} See Details for formatting guidelines. Return data *equal to or after* this time. Default is 7 days ago. 
#' @param to (equivalent to `end` in `v1`) \code{(Date/POSIXlt/Datetime(POSIXct)/character)} See Details for formatting guidelines. Return data *equal to or before* this time. Default is today's date.
#' @param after *`v1` only* \code{(Date/POSIXlt/Datetime(POSIXct)/character)} See Details for formatting guidelines. Return data *after* this time. Default is 7 days ago. *Cannot be used with \code{from}*
#' @param until *`v1` only* \code{(Date/POSIXlt/Datetime(POSIXct)/character)} See Details for formatting guidelines. Return data *before* this time. Default is today's date. *Cannot be used with \code{from}*
#' @param limit *`v1` only* \code{(integer)} The amount of bars to return per ticker. This can range from 1 to 1000. Defaults to 1000. *Note:* If \code{full} is set to T, this parameter is ignored and forced to 1000. 
#' @param full \code{(logical)} If TRUE, the function will attempt to return the entire expected dataset based on the range of dates provided and perform a data completeness check. If the requested from, to dates/times exceed that which can be returned in a single call, the API will be called repeatedly to return the **full** dataset. If FALSE, the request will be submitted to the API as is. *Note:* The `v1` API has a call limit of 1000 bars and a rate limit of 200 requests per minute. If the rate limit is reached, queries will pause for 1 minute. Defaults to FALSE.
#' @param unadjusted *v2 only* \code{(logical)} Set to `TRUE` if the results should **NOT** be adjusted for splits. Defaults to `FALSE`.
#' @details All \code{(Date/POSIXlt)} will parse correctly if in `YYYY-MM-DD` \href{https://www.iso.org/iso-8601-date-and-time-format.html}{RFC 3339} format or `(Datetime/POSIXct)`, `YYYY-MM-DD HH:MM` \href{https://www.iso.org/iso-8601-date-and-time-format.html}{ISO8601} format. Other formats will often work, but are not guaranteed to parse correctly. All Dates/Datetimes are forced to America/New York timezone (See \code{\link[lubridate]{force_tz}}) in which the NYSE operates. This means that if \code{\link[lubridate]{now}} is used to specify 3PM in the local timezone, it will be forced 3PM in the "America/New_York timezone. This eliminates needing to consistently account for timezone conversions when providing inputs. The `v2` API only accepts Dates in YYYY-MM-DD format, any arguments passed to `start` or `end` will be coerced to Date automatically if using `v2`. For the `v2` API, queries with `timeframe`: `'year'` use `12/31` as an aggregate date for each year. Arguments passed to `from` & `to` will be coerced to their yearly \code{\link[lubridate]{round_date}} and \code{\link[lubridate]{ceiling_date}} respectively. 
#' For a full overview of how the v2 Polygon.io Aggregates endpoint functions, check out \href{Aggregate Data API Improvements}{https://polygon.io/blog/aggs-api-updates/}.
#' @return \code{list} object for each ticker symbol containing a \code{data.frame} with the following columns:
#' \itemize{
#'  \item{\code{time}}{  the time of the bar as \code{POSIXct} in yyyy-mm-dd for timeframe = day, and yyyy-mm-dd hh:mm:ss for timeframes < day}
#'  \item{\code{open}}{ \code{(numeric)} open price}
#'  \item{\code{high}}{ \code{(numeric)} high price}
#'  \item{\code{low}}{ \code{(numeric)} low price}
#'  \item{\code{close}}{ \code{(numeric)} close price}
#'  \item{\code{volume}}{ \code{(numeric)} volume (in millions)}
#'  \item{\code{n *v2 only*}}{ \code{(numeric)} Number of items in aggregate window}
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
#' # Get three months of hourly data for Beyond Meat from the v2 Polygon API when full = F:
#' bars <- market_data(ticker = c("BYND"), v = 2, from = "2020-02-20", to = "2020-03-22", multiplier = 1, timeframe = "h")
#' # Returns successfully
#' purrr::map(bars, nrow) # Note the number of rows
#' purrr::map(bars, ~range(.x$time)) # Note that Some data is missing
#' # Let's take a closer look:
#' plot(open ~ time, bars$BYND, xaxt = "n", type = "l")
#' axis(1, bars$BYND$time, format(bars$BYND$time, "%b %d"), cex.axis = .7)
#' # Only about 10% of the requested data is present. Does the query status metadata indicate anything is wrong?
#' attr(bars$BYND, "query") # everything checks out, so how do we get all the data we requested?
#' # Set full = T
#' bars <- market_data(ticker = c("BYND"), v = 2, from = "2020-02-20", to = "2020-03-22", multiplier = 1, timeframe = "h", full = TRUE)
#' purrr::map(bars, nrow) # A big difference in the number of rows
#' purrr::map(bars, ~range(.x$time)) # The range appears the same
#' # A closer look:
#' plot(open ~ time, bars$BYND, xaxt = "n", type = "l")
#' axis(1, bars$BYND$time, format(bars$BYND$time, "%b %d"), cex.axis = .7)
#' # That's more like it!
#' # Get the last quote for a ticker (or multiple):
#' market_data("TWTR", timeframe = "q")
#' # or the last trade for multiple tickers:
#' market_data(c("TWTR","AAPL","BYND"), timeframe = "t")
#' @importFrom stringr str_detect str_extract regex
#' @importFrom purrr map_chr map_lgl keep
#' @importFrom rlang abort warn
#' @importFrom dplyr `%>%`
#' @export

# Generate the list in the docs:
# list(m = c("m","min","minute"), h = c("h", "hour"),d = c("d", "day"), w = c("w", "week"), m = c("M", "mo", "month"), q = c("q", "quarter"), y = c("y", "year")) %>% purrr::map(~{paste0("\\item{",paste0(paste0("`'",.x,"'`"), collapse = '/'),"}")}) %>% do.call(c,.) %>% cat(collapse = "\n")
# For DEBUG

# rlang::env_bind(environment(), ticker = "LOOP", v = 2, timeframe = "minute", multiplier = 5, from = lubridate::floor_date(Sys.Date(), "year"), after = NULL, until = NULL, limit = NULL, to = NULL, full = TRUE, unadjusted = FALSE)

market_data <- function(ticker, v = 1, timeframe = "day", multiplier = 1, from = NULL, to = NULL, after = NULL, until = NULL, limit = NULL, full = FALSE, unadjusted =  FALSE) {
  evar <- environment()
  evar$.vn = list(
    ticker = "character",
    v = c("integer", "numeric"),
    timeframe = c("factor", "character"),
    tf_num = c("integer", "numeric"),
    tf_order = "factor",
    multiplier = c("integer", "numeric"),
    from = c("character", "POSIXct", "Datetime", "Date", "NULL"),
    to = c("character", "POSIXct", "Datetime", "Date", "NULL"),
    after = c("character", "POSIXct", "Datetime", "Date", "NULL"),
    until = c("character", "POSIXct", "Datetime", "Date", "NULL"),
    limit = c("integer", "numeric", "NULL"),
    full = "logical",
    unadjusted = "logical",
    bounds = c("Date", "Datetime", "POSIXct"),
    cal = c("data.frame", "tibble")
  )
  # Last trade and quote handling  ----
  if (grepl("^(?:lt)$|^t$|(?:trade)|^q$|^(?:lq)$|(?:quote)", timeframe, ignore.case = TRUE)) {
    timeframe <- ifelse(grepl("^lt$|^t$|trade", timeframe, ignore.case = TRUE), "t", "q")
    .url <- bars_url(ticker = ticker, timeframe = timeframe)
    .out <- bars_get(.url, timeframe)
    return(.out)
  }
  # Sun Jun 14 15:58:33 2020
  #param check:  Thu Mar 26 08:34:13 2020 ----
  if((is.null(from) || is.null(to)) && (is.null(start) || is.null(end))){
    stop("Please enter either 'from' & 'to' or 'after' & 'until' arguments.")
  }
  if (!any(v %in% 1:2)) stop("Version 'v' must be either 1 or 2")
  if (stringr::str_detect(timeframe, "^\\d")) {
    # Account for old argument style to timeframe
    evar$multiplier <- multiplier <- as.numeric(stringr::str_extract(timeframe, "^\\d+"))
    evar$timeframe <- timeframe <- tolower(stringr::str_extract(timeframe, "\\w+"))
  }
  
  # Handle date bounds:  Thu Mar 26 08:40:24 2020 ----
  tf_num(timeframe, multiplier) # returns tf_num & tf_order to environment
  evar$tf_num <- tf_num 
  evar$tf_order <- tf_order
  evar$bounds <- bounds <- bars_bounds(from = from, to = to, after = after, until = until, timeframe = timeframe, multiplier = multiplier, fc = TRUE)
  # Stop if malformed date argument with informative message
  if (any(purrr::map_lgl(bounds, is.na))) rlang::abort(paste0("Check the following argument(s) format: `", names(purrr::keep(bounds, ~{is.null(.x)||is.na(.x)})), "`"))
  
  evar$cal <- rlang::exec(calendar, !!!unname(bounds))
  
  if (full) {
    # Form the URL
    .url <- bars_url(ticker, timeframe, multiplier, bounds)
    # retrieve the data
    bars <- bars_get(.url, timeframe)
    bars <- bars_complete(bars, timeframe = timeframe, multiplier = multiplier)
  } else {
    # Submit request as is for full = F:  Tue Mar 17 21:35:54 2020 ----
    # Coerce to ISO8601 for call
    # build the URL
    .url <- bars_url(ticker, timeframe, multiplier, bounds)
    # retrieve the data
    bars <- bars_get(.url, timeframe)
    # End case where full = F ---- Fri Mar 27 11:19:05 2020
  }
  return(bars)
}


#' @title Filter a `tsybble` for market hours
#' @description Filter for specific windows of the day. Useful for returning market hours, or market hours and after hours.
#' @param x \code{(tsybble)}
#' @param hours \code{(numeric)} hours of the day to include. *Default* \code{FALSE}

market_filter <- function(.x, cal, hours = 9:16) {
  .ind <- tsibble::index(.x)
  .int <- tsibble::interval(.x)
  if (.int < tsibble::new_interval(day = 1)) {
    out <- dplyr::mutate(.x, 
                         h = lubridate::hour(!!.ind),
                         d = lubridate::as_date(!!.ind))
    out <- dplyr::filter(out,
                         h %in% hours & d %in% cal$date
    )
    out <- dplyr::select(out, - h, - d)
  } else if (.int < tsibble::new_interval(week = 1)) {
    out <- dplyr::filter(.x,
                         !!.ind %in% cal$date)
  } else {
    out <- .x
  }
  return(out)
}





#' @title bars_missing
#' @keywords Internal
#' @description A wrapper for \code{\link[tsibble]{count_gaps}}
#' @param x \code{(tsymble)}
#' @inheritParams market_data
#' @inheritDotParams market_data

bars_missing <- function(x, timeframe, ..., evar = get0("evar", mode = "environment", envir = rlang::caller_env())) {
  fetch_vars(evar$.vn[c("cal", "v")], ...)
  # get the index
  .ind <- tsibble::index_var(x)
  # get the expected boundaries relative to the timeframe
  if (timeframe %in% c("minute", "hour")) {
    .begin <- lubridate::int_start(utils::head(cal, 1)$day)
    .end <- lubridate::int_end(utils::tail(cal, 1)$day)
  } else {
    .begin <- utils::head(cal, 1)$date
    .end <- utils::tail(cal, 1)$date
  }

  
  # add the beginning..
  x <- suppressMessages(add_row(x, !!.ind := try_date(.begin, timeframe), .before = 1))
  # and end
  x <- suppressMessages(add_row(x, !!.ind := try_date(.end, timeframe)))
  
  # fill the gaps & filter for market hours
  .args <- list(.data = x,
                .full  = TRUE)
  do.call(tsibble::count_gaps, .args)
}



#' @title What's the largest gap in the calendar?
#' @description Gives the largest gap in the calendar, in interval units of the supplied bars
#' @param .data \code{(tsibble)} 
#' @param cal \code{(calendar)} object

max_gap <- function(.data, cal) {
  .int <- tsibble::interval(.data)
  .gap_idx <- which.max(diff(cal$date))
  if (!rlang::is_empty(.gap_idx)) {
    out <- lubridate::as.duration(difftime(
      lubridate::int_end(cal$day[(.gap_idx + 1)]),
      lubridate::int_start(cal$day[.gap_idx])
    )) / lubridate::as.duration(.int)
  } else {
    # In instances < one day
    out <- lubridate::as.duration(.int)  
  }
  out
} 

#' @title Identify and return a gap in the data
#' @description Identifies the largest gaps in the dataset first, then identifies small gaps nearest the requested dates.
#' @param x \code{(tsymble)} returned from `bars_get`
#' @param cal \code{(tibble)}
#' @inheritDotParams market_data
#' @inheritParams market_data
#' @keywords internal

identify_gap <- function(x, timeframe, ..., evar = get0("evar", mode = "environment", envir = rlang::caller_env())) {
  .max_gap <- max_gap(x, evar$cal)
  # get the largest gap
  .missing <- bars_missing(x, timeframe = timeframe)
  out <- dplyr::slice_max(dplyr::filter(.missing, .n > .max_gap), .n)
  if (NROW(out) == 0) {
    evar$count <- evar$count + 1
    # if no more major gaps, add the tails one at a time
    if (isTRUE(evar$count == 1)) 
      out <- .missing[1,]
    else 
      out <- .missing[NROW(.missing),]
  }
  
  dplyr::filter(out, !is.na(.from))
}


# bars_complete ----
# Wed Apr 01 17:24:47 2020
#' @title Query the API as many times as necessary to fill a market_data query
#'
#' @description This function uses the output from bars_missing and fills the missing data. It's arguments are represented in the first object \code{.vn}. 
#'@details The arguments for all bars_\* functions  will be called from the environment from which the function is called if not specified explicitly. If params need to be specified explicitly, **all params must be named**. 
#' @param \code{bars} Returned by \code{bars_get}
#' @inheritParams market_data
#' @inheritDotParams market_data
#' @keywords internal
bars_complete <- function(bars, timeframe, multiplier, ..., evar = get0("evar", mode = "environment", envir = rlang::caller_env())) {
  
  .vn <- evar$.vn[c("v", "unadjusted", "limit", "full")]
  fetch_vars(.vn, ...)  
  
  if (is.data.frame(bars)) bars <- list(bars)
  .newbars <- purrr::map(bars, ~{
    ext <- rlang::env(
      out = .x,
      index = tsibble::index(.x),
      ticker = get_sym(.x)
    )
    
    # Where are the unexpected gaps
    evar$count <- 0
    .m <- identify_gap(ext$out, timeframe)
    while (isTRUE(NROW(.m) == 1)) {
      .args <- list(
        bounds = setNames(as.list(.m[c(".from", ".to")]), c("from", "to")),
        timeframe = timeframe,
        multiplier = multiplier,
        ticker = ext$ticker
      ) 
      .nd <- bars_get(do.call(bars_url, .args), timeframe = timeframe)
      .out <- bind_rows(ext$out, .nd[[1]])
      .nd <- identical(.out[[ext$index]], ext$out[[ext$index]])
      ext$out <- .out
      # this count comes from identify_gap - delays break until tails are added.
      if (.nd && isTRUE(evar$count > 1)) break
      .m <- identify_gap(ext$out, timeframe)
    }
    # sort added queries
    .query <- get_query(ext$out)
    if (is.null(.query$ts)) .query <- .query[order(purrr::map_dbl(.query, "ts"))]
    attr(ext$out, "query") <- .query
    
    return(arrange(ext$out, !!ext$index))
  })
  return(.newbars)
}




#' @title convert timeframe to numeric `tf_num`
#' @description If `tf_num` is not found in environment, create it from `timeframe`. Throw errors if arguments are unacceptable
#' @inheritParams market_data
#' @param cenv Calling environment in which to assign variables
#' @param ... additional arguments
#' @return tf_num & timeframe in calling environment. 
#' @keywords internal

tf_num <- function(timeframe, multiplier, ..., cenv = rlang::caller_env(), evar = get0("evar", mode = "environment", envir = rlang::caller_env())) {
  fetch_vars(evar$.vn["v"], ...)
  #quick detection of timespan abbreviations:  Thu Mar 26 08:34:00 2020 ----
  .tf_opts <- list(
    m = c("m", "min", "minute"),
    h = c("h", "hour"),
    d = c("d", "day"),
    w = c("w", "week"),
    M = c("M", "mo", "month"),
    q = c("q", "quarter"),
    y = c("y", "year")
  )
  # Create ordered factor or timeframe options
  tf_order <- purrr::map_chr(.tf_opts, utils::tail, 1) %>% {factor(., levels = .)}
  
  if (v == 1) {
    .t <- grep(stringr::regex(timeframe, ignore_case = T), .tf_opts[c(1,3)], ignore.case = T)
    if (length(.t) < 1) {
      stop(paste0("`",timeframe,"` is not a valid timeframe for the v1 API. See ?market_data documentation."))
    }
    # Get the timeframe
    timeframe <- utils::tail(.tf_opts[c(1,3)][[.t]], 1)
    
    # Check args
    if (timeframe == "minute" && !any(multiplier %in% c(1,5,15))) {
      rlang::abort("The v1 API only accepts multipliers of 1,5,15 for the minute timeframe")
    } else if (timeframe == "day" && multiplier != 1) {
      rlang::warn("The v1 API only accepts 1 as a `multiplier` for the day timeframe. One day bars will be returned.", class = "warning")
      multiplier <- 1
    }
    
  } else if (v == 2){
    timeframe <- utils::tail(.tf_opts[[grep(substr(ifelse(timeframe == "month", "Month", as.character(timeframe)), 0 , 1), names(.tf_opts), ignore.case = FALSE)[1]]], 1)
  }
  # Get the timeframe as a numeric
  tf_num <- which(tf_order %in% timeframe)
  rlang::env_bind(cenv, tf_num = tf_num, timeframe = timeframe, multiplier = multiplier, tf_order = tf_order)
}
#' @title Create API amenable boundaries based on user input for market_data

#' @description Internal function for fixing input date boundaries. The function is intended for use in an environment where its parameters already exist as it will use the variables from it's parent environment. See notes on specifying arguments in the *Details* section for \code{\link[AlpacaforR]{bars_complete}}.
#' @inheritParams market_data
#'@param fc Toggle for `first_call` mode that prints all messages to console. 
#'@return \code{(list)} A list with two Date/Datetime items that are boundaries for the date range necessary to call the API with to retrieve the appropriate data from the specified API. Items will have names corresponding to the non-NULL \code{from/to/after/until} arguments in the calling environment or supplied to the function.
#'@keywords internal
bars_bounds <- function(from = NULL, after = NULL, to = NULL, until = NULL, timeframe, multiplier, fc = NULL, evar = get0("evar", mode = "environment", envir = rlang::caller_env()), ...) {
  
  #trickery to get the variables from the calling environment
  fetch_vars(evar$.vn[c("v")], ...)
  tf_num(timeframe, multiplier, ...)
  
  .date_vars <- purrr::keep(rlang::call_args(match.call())[c("from","after","to","until")], ~!is.null(eval(.x)))
  # Remove NULL arguments
  
  bounds <- purrr::map(.date_vars, ~{
    try_date(eval(.x), timeframe)
  })
  # check bounds
  if (length(bounds) != 2) {
    # add a placeholder
    bounds <- append(bounds, switch(names(bounds),
           from = list(to = Sys.Date()),
           after = list(until = Sys.Date()),
           to = list(from = bounds$to - lubridate::weeks(1)),
           until = list(after = bounds$until - lubridate::weeks(1))
           ))
    # sort them into order
    bounds <- bounds[order(names(bounds))]
    # message
    .missing_arg <- names(bounds)[!names(bounds) %in% as.character(names(.date_vars))]
    message("`",.missing_arg, "` omitted. Set to ", bounds[[.missing_arg]])
  }
  
  # Coerce to floor/ceiling dates/datetimes or fail with NA
  bounds <- purrr::imap(bounds, ~{
    # API only takes day format - all timeframes need to be output in day format to create bounds formatted appropriately for the API request.
    .out <- .x 
    
    # Floors or ceilings
    if (timeframe %in% c("minute", "hour")) {
      #To include at least one full day of data, the boundary must be the previous day
      .unit <- ifelse(v == 2, paste(1, "day"), paste(multiplier, timeframe))
    }  else {
      .unit <- paste(multiplier, timeframe)
    }
    
    if (.y == "from" || .y == "after") {
      .oout <- .out
      if (isTRUE(.y == "from")) {
        .call <- rlang::expr(lubridate::floor_date(.out, .unit))
        if (multiplier > 1 && timeframe == "week") {
          .call <- rlang::expr(suppressWarnings(!!.call) + lubridate::weeks(multiplier))
        }
        .out <- rlang::eval_bare(.call)
      }
      
      if (!all.equal(.out,.oout, check.attributes = FALSE) && isTRUE(fc)) {
        message(paste0("`from/after` coerced to ", .out," to retrieve inclusive data."))
      }
      
    } else {
      .oout <- .out
      # To include the ending boundary, we need to add a full cycle of multiplier * timeframe
      if (isTRUE(.y == "to")) {
        .call <- rlang::expr(lubridate::ceiling_date(.out, .unit, change_on_boundary = FALSE))
        if (multiplier > 1 && timeframe == "week") {
          .call <- rlang::expr(suppressWarnings(!!.call) + lubridate::weeks(multiplier))
        }
        .out <- rlang::eval_bare(.call)
      }
      
      if (!all.equal(.out,.oout, check.attributes = FALSE) && isTRUE(fc)) {
        message(paste0("`to/until` coerced to ", .out, " to retrieve inclusive data."))
      }
    }
    # For requests to the v2 API where aggregates are yearly, the floor date needs to be 12/31
    if (timeframe == "year") {
      .out <- .out - lubridate::days(1)
    } else if (timeframe == "quarter") {
      # Polygon uses the 30th of month multiples of 3 for quarters
      .out <- stats::update(.out - lubridate::days(3), days = 30)
    }
    .out
  }) 
  
  # API version specific bounds
  if (v == 1) {
    if (timeframe == "minute" && (lubridate::interval(bounds[[1]], bounds[[2]]) / (60 * 60 * 24)) < 1) {
      if (!any(lubridate::hour(seq.POSIXt(from = bounds[[1]], to = bounds[[2]], by = paste0(multiplier, " ",ifelse(timeframe == "minute","min",timeframe),"s"))) %in% 9:16)) {
        warning("Date range supplied does not include typical market hours, the V1 API is unlikely to return data", call. = FALSE, immediate. = TRUE)
      }
    }
    
    bounds <- purrr::imap(bounds, ~{
      if ((.y == "from" || .y == "after") && inherits(.x, "Date")){
        # If it's the start/after - use the beginning of the trading day
        .dt <- lubridate::as_datetime(paste0(.x, " 07:00"), format = "%Y-%m-%d %H:%M", tz = "America/New_York")
        
      } else if (inherits(.x, "Date")) {
        # If its the end/until - use the end of the trading day
        .dt <- lubridate::as_datetime(paste0(.x, " 19:00"), format = "%Y-%m-%d %H:%M", tz = "America/New_York")
      } else {
        .dt <- .x
      }
      .dt
    })
  } else if (v == 2) {
    bounds <- purrr::map(bounds, ~lubridate::force_tz(lubridate::as_date(.x), "America/New_York"))
    if (all(names(bounds) %in% c("from", "to"))) {
      bounds <- stats::setNames(bounds, c("from", "to"))
    }
  }
  
  return(bounds)
}

# bars_url ----
# Sun Mar 29 16:07:34 2020
#' @title Create URLs for market_data endpoints
#'
#'
#' @description Internal function for generating query urls. The function is intended for use in an environment where its parameters already exist. See notes on specifying arguments in the *Details* section for \code{\link[AlpacaforR]{bars_complete}}.
#' @inheritParams market_data
#' @param bounds Output from \code{\link[AlpacaforR]{bars_bounds}}. 
#' @return \code{(character)} URL to be called with \code{\link[AlpacaforR]{bars_get}}
#' @keywords internal
bars_url <- function(ticker, timeframe, multiplier, bounds, ..., evar = get0("evar", mode = "environment", envir = rlang::caller_env())) {
  
  .vn <- evar$.vn[c(
    "v",
    "unadjusted",
    "limit"
  )]
  fetch_vars(.vn, ...)
  
  # Format ticker:  Thu Mar 26 08:48:03 2020 ----
  if (v == 1) {
    .ticker = ifelse(length(ticker) > 1, paste0(trimws(ticker), collapse = ","), ticker)
  } else if (v == 2) {
    .ticker = ticker
  }
  .ticker <- toupper(.ticker)
  #Limit :  Thu Mar 26 08:50:30 2020 ----
  # If limit is null or full = T OR if limit > 1000 then set at 1000 for v1
  if (v == 1) {
    if (is.null(limit) || isTRUE(limit > 1000)) {
      if (isTRUE(limit > 1000)) message("`limit` cannot exceed 1000, coercing to 1000.")
      limit <- 1000
    }  
  }
  # timeframe is quote or trade ----
  # Sun Jun 14 16:08:07 2020
  if (v == 1 && timeframe %in% c("t", "q")) {

    .url <- purrr::map_chr(setNames(ticker, ticker), ~{
      .url$path <- get_url(list(ifelse(timeframe == "t", "last","last_quote"), "stocks", .x), data = TRUE, v = v)
    })
  } else if (v == 1) {
    # multiplier & timeframe ----
    # Fri May 08 21:26:15 2020
    timeframe <- ifelse(timeframe == "minute", paste0(multiplier, "Min"), "day")
    #full = F Make API requests:  Tue Mar 17 21:37:35 2020 ----
    
    # Coerce to appropriately formatted character strings
    bounds <- purrr::map(bounds, ~{
      # V1 has a bug where offset must be -
      # V1 must have colon in offset
      stringr::str_replace(format(.x, "%Y-%m-%dT%H:%M:%S%z"), "[\\+\\-](\\d{2})(\\d{2})$", "-\\1:\\2")
      
    })

    # Build the url
    .url <- get_url(
      path = list("bars", as.character(timeframe)),
      query = list(symbols = .ticker,
                         limit = limit,
                         start = bounds$from,
                         after = bounds$after,
                         end = bounds$to,
                         until = bounds$until
    ), data = TRUE, v = v)
  } else if (v == 2) {
    .url <- purrr::map_chr(stats::setNames(.ticker, .ticker), ~{
      .url <- get_url(list(
        ep = "aggs",
        "ticker",
        ticker = .x,
        "range",
        multiplier = multiplier,
        timeframe = as.character(timeframe),
        from = as.character(lubridate::as_date(bounds[[1]])),
        to = as.character(lubridate::as_date(bounds[[2]]))
      ),
      query = list(unadjusted = unadjusted,
                   apiKey = Sys.getenv("APCA-LIVE-API-KEY-ID"))
      , data = TRUE, v = v)
    })
  }
  return(.url)
}


# bars_get ----
# Sun Mar 29 16:07:49 2020
#' @title Retrieve market data from the respective API
#'
#' @description Internal function for retrieving data from Alpaca API and outputting data.frame with query attribute. See notes on specifying arguments in the *Details* section for \code{\link[AlpacaforR]{bars_complete}}.
#' @param url \code{(character)} the url rendered by \code{\link[AlpacaforR]{bars_url}}
#' @return \code{(list)} See \code{\link[AlpacaforR]{market_data}} as the output is the same.
#' @keywords internal


bars_get <- function(url, timeframe, ..., evar = get0("evar", mode = "environment", envir = rlang::caller_env())) {
  fetch_vars(evar$.vn[c("v")], ...)
  
  if (timeframe %in% c("t","q")) {
    headers = get_headers()
    e <- rlang::env(query = list())
    bars <- purrr::imap_dfr(url, ~{
      agg_quote = httr::GET(url = .x, headers)
      .out <- bars_transform(agg_quote, timeframe)
      e$query <- append(e$query, list(attr(.out, "query")))
      return(.out)
    })
    attr(bars, "query") <- e$query
  } else if (v == 1) {
    headers = get_headers()
    if (isTRUE(get0(".dbg", .GlobalEnv, "logical", F))) message(paste0("Retrieving\n", url))
    agg_quote = httr::GET(url = url, headers)
    
    bars <- bars_transform(agg_quote, timeframe, ...)
    
    
  } else if (v == 2) {
    # get for v2 API
    bars <- purrr::imap(url, ~{
      .url <- .x
      if (isTRUE(get0(".dbg", .GlobalEnv, "logical", F))) message(paste0("Retrieving ", .y, ":\n", .url))
      #Send Request
      agg_quote = httr::GET(url = .url)
      # Save the query meta-date for appending to the output df
      .quote <- bars_transform(agg_quote, timeframe)
      return(.quote)
    })
    bars <- stats::setNames(unlist(bars, recursive = FALSE, use.names = FALSE), names(bars))
  }
  
  return(bars)
}


# bars_transform ----
# Sun Mar 29 16:09:30 2020
#' @title Transform bars objects
#'
#' 
#' @description Internal function for transforming data from Alpaca API to a human-readable TTR/quantmod compatible format
#' @keywords internal


bars_transform <- function(agg_quote, timeframe, ..., evar = get0("evar", mode = "environment", envir = rlang::caller_env())) {
  
  
  fetch_vars(evar$.vn[c("v")], ...)
  
  .quote = response_text_clean(agg_quote)
  .query <- get_query(.quote)
  if (v == 2) {
    # Mirror V1 response as list with nested symbol data
    .quote <- rlang::list2(!!.query$ticker := .quote)
  } 
  if (!timeframe %in% c("q","t")) {
    # only check output if it's a bars request
    purrr::iwalk(.quote, ~{
      .warn <- try({NROW(.x) > 0})
      if (inherits(.warn, "try-error")) {
        rlang::warn(paste0(.y, " returned no data."))
      }  
    })
  }
  
  if (timeframe %in% c("q","t")) {
    # if last quote or trade
    .sym <- .quote$symbol
    bars <- tibble::as_tibble(.quote[[grep("^last", names(.quote))]])
    bars$symbol <- .sym
    bars <- dplyr::select(bars, symbol, dplyr::everything())
    bars <- dplyr::mutate(bars, timestamp = lubridate::as_datetime(timestamp / 1e9, tz = "America/New_York", origin = lubridate::origin))
    attr(bars, "query") <- .query
  } else {
    bars = purrr::imap(.quote, ~{
      if (!any("data.frame" %in% class(.x))) {
        message(paste0("The symbol ", .y, " returned no data.")) 
        return(NULL)
      } 
      
      
      out <- dplyr::mutate(.x,
                           t = try_date(t, timeframe),
                           dplyr::across(tidyselect::all_of(c(
                             "o", "h", "c", "l", "v"
                           )),
                           ~ as.numeric(.x)))
      
      out <- dplyr::select(
        out,
        time = "t",
        open = "o",
        high = "h",
        low = "l",
        close = "c",
        volume = "v"
      )
    })  
    # Transform to tsymble, make symbol class
    bars <- purrr::imap(bars, ~{
      as_tsymble(.x, index = "time", regular = TRUE, query = .query, symbol = .y)
    })
  }
  
  return(bars)
}


