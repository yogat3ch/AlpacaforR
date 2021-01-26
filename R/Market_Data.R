
# market_data ----
# Wed Apr 22 21:40:57 2020
#' @family Market-Data
#' @title Get Market Data
#' 
#' @description The bars API provides time-aggregated price and volume data for a single stock or multiple. **The `v2` (Polygon) API is only available for live accounts and accepts the `from`, `to`, `timeframe`, `multiplier`, and `unadjusted` arguments.**
#' @param symbol \code{(character)} The stock or stocks (in vector format) for which data will be retrieved. Non case-sensitive.
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
#' @param limit *`v1` only* \code{(integer)} The amount of bars to return per symbol. This can range from 1 to 1000. Defaults to 1000. *Note:* If \code{full} is set to T, this parameter is ignored and forced to 1000. 
#' @param full \code{(logical)} If TRUE, the function will attempt to return the entire expected dataset based on the range of dates provided and perform a data completeness check. If the requested from, to dates/times exceed that which can be returned in a single call, the API will be called repeatedly to return the **full** dataset. If FALSE, the request will be submitted to the API as is. *Note:* The `v1` API has a call limit of 1000 bars and a rate limit of 200 requests per minute. If the rate limit is reached, queries will pause for 1 minute. Defaults to FALSE.
#' @param unadjusted *v2 only* \code{(logical)} Set to `TRUE` if the results should **NOT** be adjusted for splits. Defaults to `FALSE`.
#' @details All values to `from/after/to/until` will parse correctly if a numeric year, in `YYYY-MM-DD` \href{https://www.iso.org/iso-8601-date-and-time-format.html}{RFC 3339} format or `(Datetime/POSIXct)`, `YYYY-MM-DD HH:MM` \href{https://www.iso.org/iso-8601-date-and-time-format.html}{ISO8601} format. Other formats will often work, but are not guaranteed to parse correctly. All Dates/Datetimes are forced to America/New York timezone (See \code{\link[lubridate]{force_tz}}) in which the NYSE operates. This means that if \code{\link[lubridate]{now}} is used to specify 3PM in the local timezone, it will be forced 3PM in the "America/New_York timezone. This eliminates needing to consistently account for timezone conversions when providing inputs. The `v2` API only accepts Dates in YYYY-MM-DD format, any arguments passed to `start` or `end` will be coerced to Date automatically if using `v2`. For the `v2` API, queries with `timeframe`: `'year'` use `12/31` as an aggregate date for each year. Arguments passed to `from` & `to` will be coerced to their yearly \code{\link[lubridate]{round_date}} and \code{\link[lubridate]{ceiling_date}} respectively. 
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
#' Additionally, a "query" attribute is attached to each symbol's `data.frame` with the "query" data as a `list` for each of the calls required to return it. This is accessed via \code{\link[base]{attr}}. Each call is a list with the following items:
#' \itemize{
#'  \item{\code{ticker}}{ Ticker symbol requested.}
#'  \item{\code{status}}{ The API status code for the response in English}
#'  \item{\code{queryCount}}{ Number of aggregate ( min or day ) used to generate the response.}
#'  \item{\code{resultsCount}}{ Total number of results generated}
#'  \item{\code{adjusted}}{ If this response was adjusted for splits}
#'  \item{\code{status_code}}{ The API status code as an integer.}
#' }
#' @examples
#' # Getting one or more symbols from the v1 API: 
#' market_data(symbol = c("INTC","MSFT"))
#' # Getting price data with specific date ranges and timeframes, by also limiting the amount of bars returned for each symbol.
#' market_data(symbol = c("INTC","MSFT"), from = "2019-03-20", to = "2019-04-01", multiplier = 15, timeframe = "min", limit = 175)
#' # Get three months of hourly data for Beyond Meat from the v2 Polygon API when full = F:
#' bars <- market_data(symbol = c("BYND"), v = 2, from = "2020-02-20", to = "2020-03-22", multiplier = 1, timeframe = "h")
#' # Returns successfully
#' purrr::map(bars, nrow) # Note the number of rows
#' purrr::map(bars, ~range(.x$time)) # Note that Some data is missing
#' # Let's take a closer look:
#' plot(open ~ time, bars$BYND, xaxt = "n", type = "l")
#' axis(1, bars$BYND$time, format(bars$BYND$time, "%b %d"), cex.axis = .7)
#' # Only about 10% of the requested data is present. Does the query status metadata indicate anything is wrong?
#' attr(bars$BYND, "query") # everything checks out, so how do we get all the data we requested?
#' # Set full = T
#' bars <- market_data(symbol = c("BYND"), v = 2, from = "2020-02-20", to = "2020-03-22", multiplier = 1, timeframe = "h", full = TRUE)
#' purrr::map(bars, nrow) # A big difference in the number of rows
#' purrr::map(bars, ~range(.x$time)) # The range appears the same
#' # A closer look:
#' plot(open ~ time, bars$BYND, xaxt = "n", type = "l")
#' axis(1, bars$BYND$time, format(bars$BYND$time, "%b %d"), cex.axis = .7)
#' # That's more like it!
#' # Get the last quote for a symbol (or multiple):
#' market_data("TWTR", timeframe = "q")
#' # or the last trade for multiple symbols:
#' market_data(c("TWTR","AAPL","BYND"), timeframe = "t")
#' @importFrom stringr str_detect str_extract regex
#' @importFrom purrr map_chr map_lgl keep
#' @importFrom rlang abort warn
#' @importFrom dplyr `%>%`
#' @export

# Generate the list in the docs:
# list(m = c("m","min","minute"), h = c("h", "hour"),d = c("d", "day"), w = c("w", "week"), m = c("M", "mo", "month"), q = c("q", "quarter"), y = c("y", "year")) %>% purrr::map(~{paste0("\\item{",paste0(paste0("`'",.x,"'`"), collapse = '/'),"}")}) %>% do.call(c,.) %>% cat(collapse = "\n")
# For DEBUG

# rlang::env_bind(environment(), symbol = "LOOP", v = 2, timeframe = "minute", multiplier = 5, from = lubridate::floor_date(Sys.Date(), "year"), after = NULL, until = NULL, limit = NULL, to = NULL, full = TRUE, unadjusted = FALSE)

market_data <- function(symbol, v = 1, timeframe = "day", multiplier = 1, from = NULL, to = NULL, after = NULL, until = NULL, limit = NULL, full = FALSE, unadjusted =  FALSE) {
  evar <- environment()
  evar$.vn = list(
    symbol = "character",
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
    bounds = c("list"),
    cal = c("data.frame", "tibble")
  )
  # Last trade and quote handling  ----
  if (grepl("^(?:lt)$|^t$|(?:trade)|^q$|^(?:lq)$|(?:quote)", timeframe, ignore.case = TRUE)) {
    timeframe <- ifelse(grepl("^lt$|^t$|trade", timeframe, ignore.case = TRUE), "t", "q")
    .url <- bars_url(symbol = symbol, timeframe = timeframe)
    .out <- bars_get(.url, timeframe = timeframe)
    return(.out)
  }
  
  
  
  # Process & Bind important variables:  Thu Mar 26 08:40:24 2020 ----
  evar_bind() 
  
  
  
  
  
  if (full) {
    # Form the URL
    .url <- bars_url(symbol)
    # retrieve the data
    bars <- bars_get(.url)
    bars <- bars_complete(bars)
  } else {
    # Submit request as is for full = F:  Tue Mar 17 21:35:54 2020 ----
    # Coerce to ISO8601 for call
    # build the URL
    .url <- bars_url(symbol)
    # retrieve the data
    bars <- bars_get(.url)
    # End case where full = F ---- Fri Mar 27 11:19:05 2020
  }
  return(bars)
}


#' @title Create API amenable boundaries based on user input for market_data

#' @description Internal function for fixing input date boundaries. The function is intended for use in an environment where its parameters already exist as it will use the variables from it's parent environment. See notes on specifying arguments in the *Details* section for \code{\link[AlpacaforR]{bars_complete}}.
#' @inheritDotParams market_data
#' @param evar An environment that holds variable constants necessary for sub-functions.
#'@return \code{(list)} A list with two Date/Datetime items that are boundaries for the date range necessary to call the API with to retrieve the appropriate data from the specified API. Items will have names corresponding to the non-NULL \code{from/to/after/until} arguments in the calling environment or supplied to the function.
#'@keywords internal
bars_bounds <- function(..., evar = get0("evar", mode = "environment", envir = rlang::caller_env())) {
  
  #trickery to get the variables from the calling environment
  fetch_vars(evar$.vn[c("v", "from", "to", "after", "until", "timeframe", "multiplier")], ...)
  
  .date_vars <- purrr::keep(list(from = from, after = after, to = to, until = until), ~!is.null(.x))
  # Remove NULL arguments
  
  bounds <- purrr::map(.date_vars, ~{
    #TODO Test with V = 2 to ensure API requests are properly formed
    try_date(.x, timeframe)
  })
  # check bounds
  if (length(bounds) != 2) {
    # add a placeholder
    .subtract <- switch(as.character(timeframe), 
                        minute = ,
                        hour = ,
                        day = ,
                        week = ,
                        month = lubridate::duration(multiplier, timeframe),
                        quarter = lubridate::duration(3, "months"),
                        year = lubridate::duration(multiplier, timeframe))
    .fills <- Sys.Date() %>% 
    rlang::list2(to = .,
                 until = .,
                 from =  (bounds$to %||% .) - .subtract,
                 after = (bounds$until %||% .) - .subtract)
    
    bounds <- append(bounds, 
                     purrr::when(names(bounds),
                     rlang::is_empty(.) ~ .fills[c("from", "to")],
                     . == "from" ~ .fills["to"],
                     . == "after" ~ .fills["until"],
                     . == "to" ~ .fills["from"],
                     . == "until" ~ .fills["after"]
           ))
    # sort them into order
    bounds <- bounds[order(names(bounds))]
    # message
    .missing_arg <- names(bounds)[!names(bounds) %in% as.character(names(.date_vars))]
    purrr::walk(.missing_arg, ~message(glue::glue("`{.x}` omitted. Set to {bounds[[.x]]}")))
  }
  
  # Coerce to floor/ceiling dates/datetimes or fail with NA
  bounds <- purrr::imap(bounds, ~{
    
    
    # Floors or ceilings
    if (timeframe %in% c("minute", "hour", "day")) {
      #To include at least one full day of data, the boundary must be the previous day
      .unit <- ifelse(v == 2, paste(1, "day"), paste(multiplier, timeframe))
      .call <- switch(.y,
                      from = ,
                      after = rlang::expr(lubridate::floor_date(.x, .unit)),
                      to = ,
                      until = rlang::expr(lubridate::ceiling_date(.x, .unit, change_on_boundary = FALSE)))
      .out <- rlang::eval_bare(.call)
    }  else {
      .out <- .x
    }

   
    
    
    if (!identical(.x,.out)) {
      message(paste0("`",.y,"` coerced to ", .out," to retrieve inclusive data."))
    }
    # # For requests to the v2 API where aggregates are yearly, the floor date needs to be 12/31
    # if (timeframe == "year") {
    #   .out <- .out - lubridate::days(1)
    # } else if (timeframe == "quarter") {
    #   # Polygon uses the 30th of month multiples of 3 for quarters
    #   .out <- stats::update(.out - lubridate::days(3), days = 30)
    # }
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
      .dt <- purrr::when(
        .y,
        # If it's the start/after - use the beginning of the trading day
        (. == "from" || . == "after") && inherits(.x, "Date") ~ lubridate::as_datetime(paste0(.x, " 07:00"), format = "%Y-%m-%d %H:%M", tz = "America/New_York"),
        # If its the end/until - use the end of the trading day
        inherits(.x, "Date") ~ lubridate::as_datetime(paste0(.x, " 19:00"), format = "%Y-%m-%d %H:%M", tz = "America/New_York"),
        ~.x
      )
    })
  } else if (v == 2) {
    bounds <- purrr::map(stats::setNames(bounds, c("from", "to")), ~lubridate::force_tz(lubridate::as_date(.x), "America/New_York"))
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
#' @param symbol \code{(character)} Ticker symbol
#' @inheritDotParams market_data
#' @keywords internal
bars_url <- function(symbol, ..., evar = get0("evar", mode = "environment", envir = rlang::caller_env())) {
  
  .vn <- evar$.vn[c(
    "v",
    "unadjusted",
    "limit", 
    "timeframe",
    "multiplier",
    "bounds"
  )]
  fetch_vars(.vn, ...)
  
  # Format symbol:  Thu Mar 26 08:48:03 2020 ----
  if (v == 1) {
    .symbol = ifelse(length(symbol) > 1, paste0(trimws(symbol), collapse = ","), symbol)
  } else if (v == 2) {
    .symbol = symbol
  }
  .symbol <- toupper(.symbol)
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

    .url <- purrr::map_chr(setNames(symbol, symbol), ~{
      .url <- get_url(list(ifelse(timeframe == "t", "last","last_quote"), "stocks", .x), data = TRUE, v = v)
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
      query = list(symbols = .symbol,
                         limit = limit,
                         start = bounds$from,
                         after = bounds$after,
                         end = bounds$to,
                         until = bounds$until
    ), data = TRUE, v = v)
  } else if (v == 2) {
    .url <- purrr::map_chr(stats::setNames(.symbol, .symbol), ~{
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


bars_get <- function(url, ..., evar = get0("evar", mode = "environment", envir = rlang::caller_env())) {
  fetch_vars(evar$.vn[c("v", "timeframe")], ...)
  
  if (timeframe %in% c("t","q")) {
    headers = get_headers()
    e <- rlang::env(query = list())
    bars <- purrr::imap_dfr(url, ~{
      agg_quote = httr::GET(url = .x, headers)
      .out <- bars_transform(agg_quote, ...)
      e$query <- append(e$query, list(attr(.out, "query")))
      return(.out)
    })
    attr(bars, "query") <- e$query
  } else if (v == 1) {
    headers = get_headers()
    if (isTRUE(get0(".dbg", .GlobalEnv, "logical", F))) message(paste0("Retrieving\n", url))
    agg_quote = httr::GET(url = url, headers)
    
    bars <- bars_transform(agg_quote, ...)
    
    
  } else if (v == 2) {
    # get for v2 API
    bars <- purrr::imap(url, ~{
      .url <- .x
      if (isTRUE(get0(".dbg", .GlobalEnv, "logical", F))) message(paste0("Retrieving ", .y, ":\n", .url))
      #Send Request
      agg_quote = httr::GET(url = .url)
      # Save the query meta-date for appending to the output df
      .quote <- bars_transform(agg_quote) # timeframe must be passed explicitly since ... does not carry into the purrr function from bars_get
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


bars_transform <- function(agg_quote, ..., evar = get0("evar", mode = "environment", envir = rlang::caller_env())) {
  
  
  fetch_vars(evar$.vn[c("v", "timeframe", "multiplier")], ...)
  
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
        return(tibble::tibble())
      } 

      .t <- switch(timeframe,
             minute = ,
             hour = rlang::expr(lubridate::round_date(try_date(t, timeframe, tz = "America/New_York"), timeframe)),
             day = ,
             week = ,
             month = ,
             quarter = ,
             year = rlang::expr(try_date(t, timeframe, tz = "UTC"))
             )
      .cols <- c(time = "t",
      open = "o",
      high = "h",
      low = "l",
      close = "c",
      volume = "v",
      n = "n",
      vw = "vw")
      
      out <- dplyr::mutate(.x,
        t = !!.t,
        dplyr::across(
          tidyselect::any_of(unname(.cols[-1])),
          ~ as.numeric(.x)
        )
      )
      
      out <- dplyr::select(
        out,
        tidyselect::any_of(.cols)
      )
      
    })  
    
    # Transform to tsymble
    bars <- purrr::imap(bars, ~{
      if (NROW(.x) > 0) { # handle empty
        as_tsymble(.x, index = "time", query = .query, symbol = .y, interval = do.call(tsibble::new_interval, rlang::list2(!!timeframe := multiplier)))
      } else {
        .x
      }
    })
  }
  
  return(bars)
}



#' @title Expand calendar to contain each timepoint in the set of market hours
#' @description A full set of expanded timepoints for timeframes of minute, hour, day.
#' @param cal \code{(calendar)}
#' @inheritParams market_data
#' @keywords internal

expand_calendar <- function(cal, timeframe, multiplier) {
  force(timeframe)
  force(multiplier)
  force(cal)
  # args to seq.POSIXct
  out <- list(
    from = lubridate::int_start(cal$day[1]),
    to = lubridate::int_end(cal$day[length(cal$day)]),
    by = paste(multiplier, ifelse(timeframe == "minute", "min", timeframe))
  ) %>% 
    purrr::map_if(~!is.character(.x), lubridate::floor_date, timeframe)
  # floor date so start and end of sequence is correct
  
  out <- tsibble::tsibble(
  time = do.call(seq.POSIXt, out),
  index = "time"
) %>%
  dplyr::mutate(
    d = lubridate::as_date(time),
    fd = lubridate::floor_date(time, "day"),
    dur = lubridate::as.duration(time - fd)
  ) %>%
  dplyr::filter(
    suppressWarnings(dplyr::between(dur, lubridate::dhours(9) + lubridate::dminutes(30), lubridate::dhours(16) + lubridate::dminutes(30))) & d %in% cal$date
  )

  return(out)
}



#' @title bars_missing
#' @keywords Internal
#' @description A wrapper for \code{\link[tsibble]{count_gaps}}
#' @param x \code{(tsymble)}
#' @inheritParams market_data
#' @inheritDotParams market_data

bars_missing <- function(x, ..., evar = get0("evar", mode = "environment", envir = rlang::caller_env())) {
  fetch_vars(evar$.vn[c("v", "timeframe")], ...)
  # get the index
  .ind <- tsibble::index_var(x)
  # get the expected boundaries relative to the timeframe
  
  # must handle conversion to numeric for year, and coerce to tz UTC to match returned data
  .tail <- switch(as.character(timeframe),
         minute = ,
         hour = list(begin = lubridate::int_start(utils::head(evar$cal, 1)$day),
                     end = lubridate::int_end(utils::tail(evar$cal, 1)$day)),
         day = ,
         week = ,
         month = ,
         quarter = list(
           begin = try_date(evar$bounds[[1]], timeframe, tz = "UTC"),
           end = try_date(evar$bounds[[2]], timeframe, tz = "UTC")),
         year = list(
           begin = lubridate::year(evar$bounds[[1]]),
           begin = lubridate::year(evar$bounds[[2]])
         )
         )
  
  # add the beginning..
  x <- suppressMessages(add_row(x, !!.ind := .tail[[1]], .before = 1))
  # and end
  x <- suppressMessages(add_row(x, !!.ind := .tail[[2]]))
  
  # fill the gaps & filter for market hours
  .args <- list(.data = x,
                .full  = TRUE)
  do.call(tsibble::count_gaps, .args)
}


range0 <- function(..., na.rm = FALSE) {
  .range <- suppressWarnings(range(..., na.rm = na.rm))
}

#' @title memorize the span of data that the API returns each time
#' @param x \code{(tsymble)} returned from API call
#' @param ext \code{(environment)} with cyclically modified variables in `bars_complete`
#' @return \code{(\link[lubridate]{Interval})} of the time span returned by the API. Saved to the `ext` environment.
#' @keywords Internal

bars_span <- function(x, ext, timeframe) {
  if (inherits(x, "tsymble")) {
    .idx <- tsibble::index(x)
    .range <- switch(as.character(timeframe),
                     minute = ,
                     hour = ,
                     day = range0(x[[.idx]]),
                     week = ,
                     month = ,
                     quarter = lubridate::as_date(range0(x[[.idx]]), timeframe),
                     year = try_date(x, "day")
    )
    if (all(!is.na(.range))) {
      ext$span <- suppressWarnings(c(ext$span %||% lubridate::as.duration(difftime(.range[2], .range[1]))))
    }
  }
}

#' @title What's the largest gap in the calendar?
#' @description Gives the largest gap in the calendar, in interval units of the supplied bars
#' @param .data \code{(tsibble)} 
#' @inheritParams bars_span
#' @inheritParams market_data
#' @inheritParams bars_bounds

gap_max <- function(ext, timeframe, evar = get0("evar", mode = "environment", envir = rlang::caller_env())) {
# the max gap only needs to be determined once.
  evar$max_gap <- evar$max_gap %||% {
    .int <- tsibble::interval(ext$out)
    .gap_idx <- which.max(diff(evar$cal$date))
    if (!rlang::is_empty(.gap_idx) && !timeframe %in% c("minute", "hour")) {
      .max <- lubridate::as.duration(difftime(
        lubridate::int_end(evar$cal$day[(.gap_idx + 1)]),
        lubridate::int_start(evar$cal$day[.gap_idx])
      )) / lubridate::as.duration(.int)
    } else {
      .max <- lubridate::as.duration(.int)
    }
    .max
  }
  ext$missing <- bars_missing(ext$out, timeframe = timeframe)
  dplyr::slice_max(dplyr::filter(ext$missing, .n > evar$max_gap), .n)
} 

#' @title ensure the gap returned from `gap_identify` is valid
#' @param x \code{(tibble)} gap data from `.from` and `.to` columns
#' @inheritParams bars_span
#' @return \code{(logical)} indicating whether `gap_identify` should progress to the next form of gap identification because the gap type provided is invalid. `TRUE` means all gaps of type have been identified, indicated by invalid dates in `.from` & `to` or the gap object being identical to the previous round. `FALSE` means gap exists, and proceed filling gaps.
#' @keywords Internal

gap_check <- function(x, ext) {
  if (is.logical(x)) {
    .out <- TRUE
  } else {
    .out <- dplyr::filter(x, !is.na(.from) & !is.na(.to) & !is.infinite(.from) & !is.infinite(.to))
    if (NROW(.out) > 1) stop("Invalid gap")
    .out <- NROW(.out) == 0 || identical(ext$gap, x) || isTRUE(.out$.n < 2)
  }
  .out
}



#' @title Identify and return a gap in the data
#' @description Identifies the largest gaps in the dataset first, then identifies small gaps based on expected dates from calendar (if `timeframe = minute/hour/day`). Finally adds the bounds to ensure any end gaps are filled.
#' @inheritParams bars_span
#' @inheritDotParams market_data
#' @inheritParams market_data
#' @keywords internal

gap_identify <- function(ext, timeframe, ..., evar = get0("evar", mode = "environment", envir = rlang::caller_env())) {
  .idx <- tsibble::index(ext$out)
  # get the largest gap
  if (ext$max %||% TRUE) {
    out <- gap_max(ext, timeframe)
  } else {
    out <- NA
  }
  
  # if no major gaps, and out is not identical to previous
  .advance <- gap_check(out, ext)
  # Once we advance, invalidate gap_max search
  if (.advance) ext$max <- FALSE
  # advance
  if (.advance && timeframe %in% c("minute", "hour", "day") && ext$small_gap %||% TRUE) {
    
    # check for small gaps. Only needs to be computed once
    ext$expected <- ext$expected %||% {
      if (timeframe == "day") {
        .expected <- evar$cal$date[!evar$cal$date %in% ext$out[[.idx]]]
      } else { # minute/hour
        .expected <- evar$cal_exp$time[!evar$cal_exp$time %in% ext$out[[.idx]]]
      }
      .expected
    }
    
    # get the mean duration encompassed by an API call. Only computed once
    ext$span_duration <- ext$span_duration %||% {if (!is.null(ext$span)) mean(ext$span) else ext$span} 
    
    # determine the range of missing expected values that can be filled with another order
    .within <- lubridate::`%within%`(ext$expected, lubridate::interval(ext$expected[1], ext$expected[1] + ext$span_duration))
    .range <- range0(ext$expected[.within])
    
      # if there are still values left
    out <- suppressWarnings(purrr::when(!rlang::is_empty(.within) && isTRUE(all(!is.na(.range))),
                                        # make a tibble from the range
                                        . ~ tibble::tibble(.from = .range[1], .to = .range[2], .n = 2),
                                        # if not make a tibble with NA
                                        ~ tibble::tibble(.from = NA, .to = NA, .n = 1)))
    
    
    # remove those values from the gaps
    ext$expected <- ext$expected[!.within]
    # when an attempt has been made to fill each gap
    if (rlang::is_empty(ext$expected)) {
      # reset expected (such that it will be generated again)
      ext$expected <- NULL
      # Once we advance, add a counter to small_gap search (3 full revolutions will complete)
      ext$small_cycle <- (ext$small_cycle %||% 0) + 1
    }
  }
   
  
  if ((ext$small_cycle %||% 0) > 2) ext$small_gap <- FALSE 
  .advance <- gap_check(out, ext)
  # advance
  if (.advance && isTRUE(NROW(ext$missing) > 0) && ext$begin %||% TRUE) {
    out <- ext$missing[1,]
  } 
  .advance <- gap_check(out, ext)
  # Once we advance, invalidate begin tail search
  if (.advance) ext$begin <- FALSE  
  #browser(expr = .advance)
  if (.advance && isTRUE(NROW(ext$missing) > 0)) {
    out <- ext$missing[NROW(ext$missing),]
  }
      
  .advance <- gap_check(out, ext)  
    
  ext$gap <- out
  .advance
}



group_by_rle <- function(x) {
  # get the runs of clusters of missing data that are less than the mean (in this instance large gaps mean the data has already been retrieved)
  .rle <- rle(x < mean(x))
  e <- rlang::env(ids = seq_along(.rle$lengths[.rle$lengths > 1]))
  do.call(c,purrr::map(.rle$lengths, ~{
    if (.x > 1) {
      out <- rep(e$ids[1], .x)
      e$ids <- e$ids[-1]
    } else {
      out <- 0
    }
    out
  }))
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
bars_complete <- function(bars, ..., evar = get0("evar", mode = "environment", envir = rlang::caller_env())) {
  
  .vn <- evar$.vn[c("v", "unadjusted", "limit", "full", "timeframe", "multiplier")]
  fetch_vars(.vn, ...)  
  
  if (is.data.frame(bars)) bars <- list(bars)
  .newbars <- purrr::map(bars, ~{
    ext <- rlang::env(
      out = .x,
      index = tsibble::index(.x),
      symbol = get_sym(.x),
      count = 0
    )
    
    
    
    if (timeframe %in% c("minute", "hour")) 
      evar$cal_exp <- expand_calendar(evar$cal, timeframe, multiplier)
    
    .go <- gap_identify(ext, timeframe)# adds missing to ext
    
    while (isFALSE(.go)) {
    
      # only fetch data if there is data to fetch
    .args <- list(
      bounds = setNames(as.list(ext$gap[c(".from", ".to")]), c("from", "to")),
      timeframe = timeframe,
      multiplier = multiplier,
      symbol = ext$symbol
    ) 
    .nd <- suppressMessages(bars_get(do.call(bars_url, .args), timeframe = timeframe, multiplier = multiplier, ...))
    bars_span(.nd[[1]], ext, timeframe)
    ext$out <- bind_rows(ext$out, .nd[[1]])
    
      # this count comes from identify_gap - delays break until tails are added.
      .go <- gap_identify(ext, timeframe)
    }
    # sort added queries
    .query <- get_query(ext$out)
    if (is.null(.query$ts)) .query <- .query[order(purrr::map_dbl(.query, "ts"))]
    attr(ext$out, "query") <- .query
    
    return(arrange(ext$out, !!ext$index))
  })
  return(.newbars)
}




#' @title Construct environment that stores top-level user-supplied variables for `market_data` 
#' @description \lifecycle{stable} bind top-level user-supplied and derived variables in `evar` environment for availability in internal functions
#' @inheritDotParams market_data
#' @param ... additional arguments to assign
#' @param cenv Calling environment in which to assign variables
#' @keywords internal

evar_bind <- function(..., cenv = rlang::caller_env(), evar = get0("evar", mode = "environment", envir = rlang::caller_env())) {
  .vars <- rlang::dots_list(...)
  fetch_vars(evar$.vn, ...)
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
  rlang::env_bind(evar, timeframe = timeframe, multiplier = multiplier, tf_num = tf_num, tf_order = tf_order)
  if (!"bounds" %in% names(.vars)) {
    bounds <- bars_bounds(from = from, to = to, after = after, until = until)
  }
  
  
  
  if (timeframe %in% c("minute", "hour", "day") && !"cal" %in% names(.vars)) {
    # calendar is only necessary when timeframe is less than week
    cal <- rlang::exec(calendar, !!!unname(bounds))
  }
  # Stop if malformed date argument with informative message
  if (any(purrr::map_lgl(bounds, is.na))) rlang::abort(paste0("Check the following argument(s) format: `", names(purrr::keep(bounds, ~{is.null(.x)||is.na(.x)})), "`"))
  
  rlang::env_bind(evar, bounds = bounds, cal = cal)
}
