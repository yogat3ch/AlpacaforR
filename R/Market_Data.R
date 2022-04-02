
# market_data ----
# Wed Apr 22 21:40:57 2020
#' @family Market-Data
#' @title Get Market Data
#'
#' @description The bars API provides time-aggregated price and volume data for a single stock or multiple. **The `v2` (Polygon) API is only available for live accounts and accepts the `from`, `to`, `timeframe`, `multiplier`, and `unadjusted` arguments.**
#' @param symbol \code{(character)} The stock or stocks (in vector format) for which data will be retrieved. Non case-sensitive.
#' @param v \code{(integer)} The API version number.
#' \itemize{
#'   \item{\code{1}}{ the `v1` \href{https://alpaca.markets/docs/api-documentation/api-v2/market-data/#endpoint}{IEX/Alpaca API}: data.alpaca.markets/v1 will be used}
#'   \item{\code{2}}{ the `v2` \href{https://alpaca.markets/docs/api-documentation/api-v2/market-data/alpaca-data-api-v2/}{Alpaca V2 API}: data.alpaca.markets/v2 endpoint will be used.}
#'   \item{\code{"p"/"polygon"}}{ the \href{https://polygon.io/docs/#get_v2_aggs_ticker__ticker__range__multiplier___timespan___from___to__anchor}{Aggregates Endpoint}: api.polygon.io/v2/aggs will be used. }
#' }

#' @param timeframe \code{(character)} For the `v1` API, one of
#' \itemize{
#'  \item{\code{'tr'/'lt'/'trade'/'last_trade'}}{ For the last trade price. See \href{https://alpaca.markets/docs/api-documentation/api-v2/market-data/last-trade/}{Last Trade}}
#'  \item{\code{'qu'/'lq'/'quote'/'last_quote'}}{ For the last quote price. See \href{https://alpaca.markets/docs/api-documentation/api-v2/market-data/last-quote/}{Last Quote}}
#'  \item{\code{'m'/'min'`/`'minute'}}{ (`multiplier` can be `1`/`5`/`15`)}
#'  \item{\code{'d'/'day'}}{ (`multiplier` will be `1`)}
#' }
#' For the `v2` \href{https://alpaca.markets/docs/api-documentation/api-v2/market-data/alpaca-data-api-v2/}{IEX/SIP API} the following timeframes are supported:
#' \itemize{
#'  \item{\code{'lt'/'last_trade'}}{ The Latest trade API provides the latest trade data for a given ticker symbol. See \href{https://alpaca.markets/docs/api-documentation/api-v2/market-data/alpaca-data-api-v2/historical/#latest-trade}{Latest Trades}}
#'  \item{\code{'tr'/'trade'}}{ For historical trade data for a given ticker symbol on a specified date *`from/to` required*. See \href{https://alpaca.markets/docs/api-documentation/api-v2/market-data/alpaca-data-api-v2/historical/#trades}{Trades}. **WARNING** The server stores data by the nanosecond. Limit the time span requested otherwise this could take a very long time. For example: a request spanning two days had 68 pages and took ~ 3m to retrieve.}
#'  \item{\code{'lq'/'last_quote'}}{The Latest quote API provides the latest quote data for a given ticker symbol. See \href{https://alpaca.markets/docs/api-documentation/api-v2/market-data/alpaca-data-api-v2/historical/#latest-quote}{Latest Quote}}
#'  \item{\code{'qu'/'quote'}}{ The Quotes endpoint provides NBBO quotes for a given ticker symbol at a specified date *`from/to` required*. See \href{https://alpaca.markets/docs/api-documentation/api-v2/market-data/alpaca-data-api-v2/historical/#quotes}{Quotes}, **WARNING** The server stores data by the nanosecond. Limit the time span requested otherwise this could take a very long time. For example: a request spanning a single day (ie 5/26-5/27) can take ~ 3m to retrieve.}
#'   \item{\code{"ss"/"snapshot"}}{ The Snapshot endpoint provides the latest trade, latest quote, minute bar daily bar and previous daily bar data for a symbol or a set of symbols. See \href{Snapshot - Multipler Tickers}{https://alpaca.markets/docs/api-documentation/api-v2/market-data/alpaca-data-api-v2/historical/#snapshot---multiple-tickers} & \href{Snapshot - Ticker}{https://alpaca.markets/docs/api-documentation/api-v2/market-data/alpaca-data-api-v2/historical/#snapshot---ticker}}
#'  \item{`'m'`/`'min'`/`'minute'`}
#'  \item{`'h'`/`'hour'`}
#'  \item{`'d'`/`'day'`}
#'  }
#' For the `"p"/"polygon"` API, `multiplier` can be any positive `integer` for any one of the following `timeframe`'s:
#' \itemize{
#'  \item{`'m'`/`'min'`/`'minute'`}
#'  \item{`'h'`/`'hour'`}
#'  \item{`'d'`/`'day'`}
#'  \item{`'w'`/`'week'`}
#'  \item{`'M'`/`'mo'`/`'month'`}{ (*Note* capitalize M for month)}
#'  \item{`'q'`/`'quarter'`}
#'  \item{`'y'`/`'year'`}
#' }
#' @param multiplier For the `v1` API, with `'minute'` `timeframe` one of `1`/`5`/`15`. Otherwise, defaults to `1`.
#' For the `v2` API, `multiplier` can only be `1`.
#' For the `"polygon"` API, this can be any positive `integer`. **Default** `1`.
#' @param from (equivalent to `start` in `v1`) \code{(Date/POSIXlt/Datetime(POSIXct)/character)} See Details for formatting guidelines. Return data *equal to or after* this time. Default is 7 days ago.
#' @param to (equivalent to `end` in `v1`) \code{(Date/POSIXlt/Datetime(POSIXct)/character)} See Details for formatting guidelines. Return data *equal to or before* this time. Default is today's date.
#' @param after *`v1` only* \code{(Date/POSIXlt/Datetime(POSIXct)/character)} See Details for formatting guidelines. Return data *after* this time. Default is 7 days ago. *Cannot be used with \code{to}*
#' @param until *`v1` only* \code{(Date/POSIXlt/Datetime(POSIXct)/character)} See Details for formatting guidelines. Return data *before* this time. Default is today's date. *Cannot be used with \code{from}*
#' @param limit *v1 & v2* \code{(integer)} The amount of bars to return per symbol. This can range from `1` to `1000` for `v1` and `1` to `10000` for `v2`. **Default** 1000 (`v1`), 10000 (`v2`), 50000 (`p`).
#' @param full \code{(logical)} If TRUE, the function will attempt to return the entire expected dataset based on the range of dates provided and perform a data completeness check. If the requested from, to dates/times exceed that which can be returned in a single call, the API will be called repeatedly to return the **full** dataset. If FALSE, the request will be submitted to the API as is. *Note on rate limits:* The `v1` API has a call limit of 1000 bars and a rate limit of 200 requests per minute. If the rate limit is reached, queries will pause for 1 minute. The `polygon` API free tier has a call limit of 5 requests per minute, if the rate limit is reached, queries will pause for 1 minute. **Default** FALSE.
#' @param unadjusted *polygon only* \code{(logical)} Set to `TRUE` if the results should **NOT** be adjusted for splits. **Default** `FALSE`.
#' @param adjustment *Deprecated* \code{(character)} Specifies the corporate action adjustment for the stocks. Possible values: 'raw', 'split', 'dividend' or 'all'. **Default** `'raw'`.
#' @details All values to `from/after/to/until` will parse correctly if a numeric year, in `YYYY-MM-DD` \href{https://www.iso.org/iso-8601-date-and-time-format.html}{RFC 3339} format or `(Datetime/POSIXct)`, `YYYY-MM-DD HH:MM` \href{https://www.iso.org/iso-8601-date-and-time-format.html}{ISO8601} format. Other formats will often work, but are not guaranteed to parse correctly. All Dates/Datetimes are forced to America/New York timezone (See \code{\link[lubridate]{force_tz}}) in which the NYSE operates. This means that if \code{\link[lubridate]{now}} is used to specify 3PM in the local timezone, it will be forced 3PM in the "America/New_York timezone. This eliminates needing to consistently account for timezone conversions when providing inputs. The `polygon` API only accepts Dates in YYYY-MM-DD format, any arguments passed to `start` or `end` will be coerced to Date automatically if using `polygon`. For the `polygon` API, queries with `timeframe`: `'year'` use `12/31` as an aggregate date for each year. Arguments passed to `from` & `to` will be coerced to their yearly \code{\link[lubridate]{round_date}} and \code{\link[lubridate]{ceiling_date}} respectively.
#' For a full overview of how the Polygon.io Aggregates endpoint behaves, check out \href{Aggregate Data API Improvements}{https://polygon.io/blog/aggs-api-updates/}.
#' @return \code{tsymble} object if just one symbol is requested, otherwise a `named list` of `tsymble` objects for each ticker symbol.
#' A \code{tsymble} is a `tsibble` object with the following columns:
#' \itemize{
#'  \item{\code{time}}{ the time of the bar as \code{POSIXct} in yyyy-mm-dd for timeframe = day, and yyyy-mm-dd hh:mm:ss for timeframes < day}
#'  \item{\code{open}}{ \code{(numeric)} open price}
#'  \item{\code{high}}{ \code{(numeric)} high price}
#'  \item{\code{low}}{ \code{(numeric)} low price}
#'  \item{\code{close}}{ \code{(numeric)} close price}
#'  \item{\code{volume}}{ \code{(numeric)} volume (in millions)}
#'  \item{\code{vw *polygon only*}}{ \code{(numeric)} the volume weighted average price}
#'  \item{\code{n *polygon only*}}{ \code{(numeric)} Number of items in aggregate window}
#' }
#' Each `tsymble` has a "query" attribute with the "query" data as a `list` for each of the calls required to return it and a "symbol" attribute with the ticker symbol for the data. These attributes are easily accessed via `get_query` and `get_sym` respectively.
#' @examples
#' \dontrun{
#' # Getting data for the last week from the v1 API:
#' market_data(symbol = c("INTC","MSFT"), v = 1, from = Sys.Date() - lubridate::weeks(1))
#' # Getting price data with specific date ranges and timeframes with limit on the number of bars returned
#' market_data(symbol = c("INTC","MSFT"), from = "2019-03-20", to = "2019-04-01", multiplier = 15, timeframe = "min", limit = 175, v = 1)
#' # Get the last trade for a symbol (or multiple):
#' market_data("TWTR", timeframe = "lt")
#' # or the last quote for multiple symbols:
#' market_data(c("TWTR","AAPL","BYND"), timeframe = "lq")
#' }
#' @export

# Generate the list in the docs:
# list(m = c("m","min","minute"), h = c("h", "hour"),d = c("d", "day"), w = c("w", "week"), m = c("M", "mo", "month"), q = c("q", "quarter"), y = c("y", "year")) %>% purrr::map(~{paste0("\\item{",paste0(paste0("`'",.x,"'`"), collapse = '/'),"}")}) %>% do.call(c,.) %>% cat(collapse = "\n")
# For DEBUG

# rlang::env_bind(environment(), symbol = "LOOP", v = 2, timeframe = "minute", multiplier = 5, from = lubridate::floor_date(Sys.Date(), "year"), after = NULL, until = NULL, limit = NULL, to = NULL, full = TRUE, unadjusted = FALSE)

market_data <- function(symbol, v = 2, timeframe = "day", multiplier = 1, from = NULL, to = NULL, after = NULL, until = NULL, limit = NULL, full = FALSE, unadjusted =  FALSE) {
  evar <- environment()
  evar$.vn = list(
    symbol = "character",
    v = c("integer", "numeric", "character"),
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
    cal = c("data.frame", "tibble"),
    tqs = "character",
    is_tqs = "logical"
  )

  evar$tqs <- c("lt", "tr", "lq", "qu", "ss")
  evar$is_tqs <- FALSE


  # Process & Bind important variables:  Thu Mar 26 08:40:24 2020 ----
  evar_bind()

  if (full && v != 2) {
    # V2 has pagination - so the cobbling together of data with bars_complete is not necessary with this API.

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
  # Return single tibble when only one symbol is requested
  bars <- purrr::when(length(symbol) == 1 && v == 1,
              isTRUE(.) ~ get_tibble(bars),
              ~ bars)
  return(bars)
}


#' @title Create API amenable boundaries based on user input for market_data
#' @description Internal function for fixing input date boundaries. The function is intended for use in an environment where its parameters already exist as it will use the variables from it's parent environment. See notes on specifying arguments in the *Details* section for \code{\link[AlpacaforR]{bars_complete}}.
#' @inheritDotParams market_data
#' @param evar An environment that holds variable constants necessary for sub-functions.
#'@return \code{(list)} A list with two Date/Datetime items that are boundaries for the date range necessary to call the API with to retrieve the appropriate data from the specified API. Items will have names corresponding to the non-NULL \code{from/to/after/until} arguments in the calling environment or supplied to the function.
#'@keywords internal
bars_bounds <- function(..., evar = get0("evar", mode = "environment", envir = rlang::caller_env())) {
  force(evar)
  #trickery to get the variables from the calling environment
  fetch_vars(evar$.vn[c("v", "from", "to", "after", "until", "timeframe", "multiplier", "is_tqs")], ...)

  .date_vars <- purrr::keep(list(from = from, after = after, to = to, until = until), ~!is.null(.x))
  # Remove NULL arguments

  bounds <- purrr::map(.date_vars, ~{
    try_date(.x, ifelse(is_tqs, "minute", timeframe))
  })
  # check bounds
  if (length(bounds) != 2) {
    # add a placeholder
    .tf <- switch(as.character(timeframe),
                        minute = ,
                        hour = lubridate::duration(1, "days"),
                        day = ,
                        week = ,
                        month = lubridate::duration(multiplier, timeframe),
                        quarter = lubridate::duration(3, "months"),
                        year = lubridate::duration(multiplier, timeframe),
                        lt = ,
                        tr = ,
                        lq = ,
                        qu = ,
                        ss = lubridate::duration(1, "day"))
    .f <- switch(as.character(timeframe),
                  minute = ,
                  hour = lubridate::as_datetime,
                  day = ,
                  week = ,
                  month = ,
                  quarter = ,
                  year = ,
                  lt = ,
                  tr = ,
                  lq = ,
                  qu = ,
                  ss = lubridate::as_date)

    .to <- rlang::exec(.f, (bounds$from %||% bounds$after %||% Sys.Date()))
    .from <- rlang::exec(.f, (bounds$to %||% bounds$until %||% Sys.Date()))

    .fills <- rlang::list2(
      to = .to + .tf,
      until = .to + .tf,
      from =  .from - .tf,
      after = .from - .tf
    )

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
    if (!(v == 1 && is_tqs))
      purrr::walk(.missing_arg, ~message(glue::glue("`{.x}` omitted. Set to {bounds[[.x]]}")))
  }

  # Coerce to floor/ceiling dates/datetimes or fail with NA
  bounds <- purrr::imap(bounds, ~{
    # Floors or ceilings
    if (timeframe %in% c("minute", "hour", "day")) {
      #To include at least one full day of data, the boundary must be the previous day
      .unit <- ifelse(v == "p", paste(1, "day"), paste(multiplier, timeframe))
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
  if (v %in% 1:2) {
    if (timeframe == "minute" && (lubridate::interval(bounds[[1]], bounds[[2]]) / (60 * 60 * 24)) < 1) {
      if (!any(lubridate::hour(seq.POSIXt(from = bounds[[1]], to = bounds[[2]], by = paste0(multiplier, " ",ifelse(timeframe == "minute","min",timeframe),"s"))) %in% 9:16)) {
        warning("Date range supplied does not include typical market hours, the V1 API is unlikely to return data", call. = FALSE, immediate. = TRUE)
      }
    }

    bounds <- purrr::imap(bounds, ~{
      .dt <- purrr::when(
        .y,
        # If it's the start/after - use the beginning of the trading day
        (. == "from" || . == "after") && inherits(.x, "Date") && timeframe %in% c("minute", "hour") ~ lubridate::as_datetime(paste0(.x, " 07:00"), format = "%Y-%m-%d %H:%M", tz = "America/New_York"),
        # If its the end/until - use the end of the trading day
        inherits(.x, "Date") && v != 2 ~ lubridate::as_datetime(paste0(.x, " 19:00"), format = "%Y-%m-%d %H:%M", tz = "America/New_York"),
        ~ inherits(.x, "Date") && v == 2 && lubridate::as_date(.x) == Sys.Date() ~ lubridate::floor_date(.x, "day"),
        ~.x
      )
    })
  } else if (v == "p") {
    bounds <- purrr::map(stats::setNames(bounds, c("from", "to")), ~lubridate::force_tz(lubridate::as_date(.x), "America/New_York"))
  }

  return(bounds)
}
bounds_format <- function(bounds) {
  bounds <- purrr::map(bounds, ~{
    # V2 does not allow requests beyond present time
    if (.x > Sys.time()) .x <- lubridate::as_datetime(lubridate::floor_date(Sys.time(), "day"))
    # V1 has a bug where offset must be -
    # V1 must have colon in offset
    stringr::str_replace(format(.x, "%Y-%m-%dT%H:%M:%S%z"), "[\\+\\-](\\d{2})(\\d{2})$", "-\\1:\\2")
  })
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
  force(evar)
  .vn <- evar$.vn[c(
    "v",
    "unadjusted",

    "limit",
    "timeframe",
    "multiplier",
    "bounds",
    "is_tqs",
    "tqs"
  )]
  fetch_vars(.vn, ...)

  # Format symbol:  Thu Mar 26 08:48:03 2020 ----
  if (((v == 1 && !timeframe %in% c("qu", "tr")) || timeframe == "ss") && length(symbol) > 1) {
    .symbol = paste0(trimws(symbol), collapse = ",")
  } else {
    .symbol = symbol
  }
  .symbol <- toupper(.symbol)


  #Limit :  Thu Mar 26 08:50:30 2020 ----
  # If limit is null OR if limit > 1000 then set at 1000 for v1
  .limit = c(v1 = 1000, v2 = 10000, p = 50000)
  .limit <- purrr::when(limit,
              is.null(.) ~ .limit[v],
              . > .limit[v] ~ .limit[v],
              ~ limit)
  if (!identical(.limit,limit)) {
    message("`limit` coerced to ",.limit)
    limit <- .limit
  }

  if (v %in% 1:2) {
    # multiplier & timeframe ----
    # Fri May 08 21:26:15 2020
    timeframe <- purrr::when(timeframe,
           . == "minute" ~ "Min",
           . == "hour" && v == 2 ~ "Hour",
           . == "day" && v == 2 ~ "Day",
           . == "day" ~ "D",
           ~ timeframe)
    .path <- purrr::when(timeframe,
                              v == 1 && . %in% tqs[1:2]  ~ list("last","stocks", symbol = .symbol),
                              v == 1 && . %in% tqs[3:4] ~ list("last_quote","stocks", symbol = .symbol),
                              v == 2 && . == "lt" ~ list("stocks", symbol = .symbol, "trades", "latest"),
                              v == 2 && . == "tr" ~ list("stocks", symbol = .symbol, "trades"),
                              v == 2 && . == "lq" ~ list("stocks", symbol = .symbol, "quotes", "latest"),
                              v == 2 && . == "qu" ~ list("stocks", symbol = .symbol, "quotes"),
                              v == 2 && . == "ss" && length(symbol) == 1 ~ list("stocks", symbol = .symbol,"snapshot"),
                              v == 2 && . == "ss" ~ list("stocks","snapshots"),
                              v == 1 && !. %in% tqs ~  list("bars", paste0(multiplier, timeframe)),
                              v == 2 && !. %in% tqs ~ list("stocks", symbol = .symbol, "bars"))

    #full = F Make API requests:  Tue Mar 17 21:37:35 2020 ----

    # Coerce to appropriately formatted character strings
    bounds <- bounds_format(bounds)
    # Path for v1 or v2
    # Query for v1 or v2
    .query <- purrr::compact(purrr::when(
      v,
      . == 1 && !is_tqs ~ list(
        symbols = .symbol,
        limit = limit,
        start = bounds$from,
        after = bounds$after,
        end = bounds$to,
        until = bounds$until
      ),
      . == 2 && timeframe == "ss" && length(symbol) > 1 ~ list(symbols = .symbol),
      . == 2 ~ list(
        start = bounds$from %||% bounds$after,
        end = bounds$to %||% bounds$until,
        limit = limit,
        page_token = rlang::dots_list(...)$page_token,
        timeframe = paste0(multiplier, timeframe)
      )
    ))
    if (v == 2 && timeframe %in% tqs) {
      .query$timeframe = NULL
    }
    # Build the url
    if (isTRUE(length(.symbol) == 1)) {
      .args <- list(path = .path,
                    data = TRUE,
                    v = v)
      if (!timeframe %in% c("lq","lt") || (timeframe %in% "ss" && length(evar$symbol) > 1))
        .args$query = .query
      .url <- do.call(get_url, .args) %>%
        stats::setNames(nm = .symbol)
    } else if (isTRUE(length(.symbol) > 1)) {
      .url <- purrr::map_chr(stats::setNames(.path$symbol, .path$symbol), ~{
        .url <- get_url(
          path = purrr::list_modify(.path, symbol = .x),
          query = .query
          , data = TRUE, v = v)
      })
    }

  } else if (v == "p") {
    .args <- list(path = list(
      ep = "aggs",
      "ticker",
      ticker = .symbol,
      "range",
      multiplier = multiplier,
      timeframe = as.character(timeframe),
      from = as.character(lubridate::as_date(bounds[[1]])),
      to = as.character(lubridate::as_date(bounds[[2]]))
    ),
    query = list(unadjusted = unadjusted)
    , api = "api", poly = TRUE, data = TRUE, v = 2)
    .url <- purrr::when(length(.symbol) == 1,
                isTRUE(.) ~ rlang::exec(get_url, !!!.args) %>%
                  stats::setNames(nm = .symbol),
                ~ purrr::map_chr(stats::setNames(.symbol, .symbol), ~{
                  .args$path <- purrr::list_modify(.args$path, ticker = .x)
                  rlang::exec(get_url, !!!.args)
                }))

  }
  return(.url)
}

#' @title Find object with `nm`
#' @description Recurse into the top level list until an object that has a name matching `nm` is found
#' @param x \code{(list)}
#' @param nm \code{(character)} The name to recursively search for
#' @param depth \code{(logical)} Whether return the object (`FALSE`) *default*, or an integer for the depth at which the object was found (`TRUE`)
#' @return \code{(numeric/tibble)}
#' @keywords Internal

get_tibble <- function(x, nm = "time", depth = FALSE) {
  if (is_legit(x)) {
    i <- 0
    .o <- x
    while (!nm %in% names(.o) && purrr::vec_depth(.o) > 1 && is_legit(x)) {
      .o <- .o[[1]]
      i <- i + 1
    }
    if (depth)
      .o <- i

  } else {
    .o <- x
  }
  .o
}


# bars_pagination ----
# Mon Mar 22 11:25:31 2021
bars_pagination <- function(bar, ..., evar = get0("evar", mode = "environment", envir = rlang::caller_env())) {
  force(evar)
  fetch_vars(evar$.vn[c("multiplier", "timeframe")])
  .bar <- get_tibble(bar)
  .query <- get_query(.bar)
  # if this is not the first query
  if (is.null(names(.query))) {
    .q <- tail(.q, 1) # get the last
  } else
    .q <- .query

  i <- 1
  bars <- list()
  if (is.character(.q$next_page_token %||% tail(.q, 1)$next_page_token)) {
    .spin <- cli::make_spinner("dots10", template = glue::glue("{spin} {{cli::col_grey('Fetching page: ')}}", .open = "{{", .close = "}}"))
  }

  while (is.character(.q$next_page_token %||% tail(.q, 1)$next_page_token)) {
    .url <- httr::parse_url(.q$url)
    .url$query <- purrr::list_modify(.url$query, page_token = .q$next_page_token)
    # Save the query meta-date for appending to the output df
    bars[[i]] <- bars_transform(httr::GET(httr::build_url(.url), get_headers()))
    bars[[i]] <- get_tibble(bars[[i]], "time")

    .query <- append(
      purrr::when(
        identical(.query, .q),
        isTRUE(.) ~ list(.query),
        ~ .query
      ), list(get_query(bars[[i]]))
    )
    .q <- get_query(bars[[i]])
    if (i > 1 && isTRUE(identical(bars[[i]], bars[[i-1]])))
      break
    i <- i + 1
    .spin$spin()
    cat(cli::col_blue(i))
    Sys.sleep(.1)
  }
  .spin$finish()
  cli::cat_line(cli::col_grey("Binding ",i + 1," pages..."))
  out <- rlang::exec(bind_rows, get_tibble(bar), !!!bars)
  .interval <- do.call(tsibble::new_interval,
          purrr::when(evar$is_tqs,
                      . ~ time_interval(out) %>%
                        {rlang::list2(!!.$timeframe := .$multiplier)},
                      rlang::list2(!!timeframe := multiplier)
          )
  )
  as_tsymble(
    out,
    index = "time",
    query = .query,
    symbol = get_sym(.bar),
    interval = .interval
  )
}


# bars_get ----
# Sun Mar 29 16:07:49 2020
#' @title Retrieve market data from the respective API
#'
#' @description Internal function for retrieving data from Alpaca API and outputting data.frame with query attribute. See notes on specifying arguments in the *Details* section for \code{\link[AlpacaforR]{bars_complete}}.
#' @param url \code{(character)} the url rendered by \code{\link[AlpacaforR]{bars_url}}
#' @return \code{(list)} See \code{\link[AlpacaforR]{market_data}} as the output is the same.
#' @keywords internal


bars_get <- function(url, ..., evar = get0("evar", mode = "environment", envir = rlang::caller_env()), just_tibble = FALSE) {
  force(evar)
  fetch_vars(evar$.vn[c("v", "timeframe", "is_tqs", "tqs")], ...)

  if (is_tqs && v == 1) {
    headers = get_headers()
    e <- rlang::env(query = list())
    bars <- purrr::imap_dfr(url, ~{
      agg_quote = httr::GET(url = .x, headers)
      .out <- bars_transform(agg_quote, ...)
      e$query <- append(e$query, list(attr(.out, "query")))
      return(.out)
    }) %>%
      tibble::add_column(symbol = names(url), .before = 0)
    attr(bars, "query") <- e$query
    attr(bars, "symbol") <- bars$symbol
  } else if (v == 1) {
    agg_quote = httr::GET(url = url, get_headers())
    bars <- bars_transform(agg_quote, ...)
  } else if (v == "p" || v == 2) {
    # get for v2 API
    bars <- purrr::when(length(url),
                . == 1 ~ bars_getv2(url, v = v, evar = evar),
                ~ purrr::map(url, bars_getv2, v = v, evar = evar))
    # Remove excess depth
    .depth <- get_tibble(bars, depth = TRUE)
    if (isTRUE(.depth > 2)) {
      for (i in 1:(.depth - ifelse(timeframe == "ss", 2, 1))) {
        bars <- unlist(bars, recursive = FALSE, use.names = ifelse(timeframe == "ss", TRUE, FALSE))
      }
      .nms <- do.call(c,purrr::map(bars, get_sym)) %||% names(bars)
      bars <- stats::setNames(bars, .nms)
    }
  }
  # If just the tibble should be returned and not a nested list.
  if (isTRUE(just_tibble)) {
    bars <- get_tibble(bars)
  }
  return(bars)
}

#' @title GET url with the v2 & polygon API
#' @description GETs the API query for Polygon and Alpaca version 2 API's
#' @param .url \code{(character)} The url to retrieve
#' @inheritParams bars_bounds
#' @inheritParams market_data
#' @return \code{(tibble)} data after retrieving all pages
#' @keywords Internal

bars_getv2 <- function(.url, v, evar = get0("evar", mode = "environment", envir = rlang::caller_env())) {
  #Send Request
  agg_quote = httr::GET(url = .url, if (v == 2) get_headers())
  if (v == "p" && agg_quote$status_code == 429) {
    # Handle "Too Many Requests" error for Polygon
    message("Too many requests to Polygon, waiting 60s...")
    Sys.sleep(60)
    agg_quote = httr::GET(url = .url, if (v == 2) get_headers())
  }
  # Save the query meta-date for appending to the output df
  out <- bars_transform(agg_quote) # timeframe must be passed explicitly since ... does not carry into the purrr function from bars_get
  .q <- get_query(get_tibble(out))
  if (v == 2 && is.character(.q$next_page_token %||% tail(.q, 1)$next_page_token))
    out <- bars_pagination(out)
  return(out)
}

# bars_transform ----
# Sun Mar 29 16:09:30 2020
#' @title Transform bars objects
#'
#'
#' @description Internal function for transforming data from Alpaca API to a human-readable TTR/quantmod compatible format
#' @keywords internal


bars_transform <- function(agg_quote, ..., evar = get0("evar", mode = "environment", envir = rlang::caller_env())) {
  force(evar)

  fetch_vars(evar$.vn[c("v", "timeframe", "multiplier", "is_tqs", "symbol")], ...)

  .quote = response_text_clean(agg_quote)
  .query <- get_query(.quote)
  check_response(.quote, .query)
  .is_list <- inherits(.quote, "list")

  if (!is_tqs) {
    # only check output if it's a bars request
    purrr::when(.is_list,
                isTRUE(.) ~ purrr::walk(.quote, check_response, query = .query),
                ~ check_response(.quote))

  }
  if (timeframe == "ss") {
    bars <- purrr::map_depth(.quote[!names(.quote) %in% "symbol"], get_tibble(.quote, "t", depth = T), bars_tidy, timeframe = timeframe, evar = evar)
    attr(bars, "query") <- .query[c("status_code", "url", "ts")]
    attr(bars, "symbol") <- .query$symbol
  } else if (is_tqs) {

    # if last quote or trade
    .sym <- .quote$symbol
    .obj_nm <- grep("(?:^last)|(?:^quote)|(?:^trade)", names(.quote), value = TRUE)
    bars <- tibble::as_tibble(.quote[[.obj_nm]])

    # return raw quote if zero length
    if (rlang::is_empty(bars)) {
      cli::cli_alert_warning(paste0("The symbol ", .sym, " returned no data."))
      bars <- NULL
    } else {
      if (v == 2) {
        bars <- dplyr::select(bars, time = "t", everything())
        bars <- suppressMessages(dplyr::mutate(bars, time = lubridate::as_datetime(time, tz = "America/New_York")))
      } else {
        bars <- dplyr::mutate(bars, timestamp = lubridate::as_datetime(timestamp / 1e9, tz = "America/New_York", origin = lubridate::origin))
      }
      attr(bars, "query") <- .query[!names(.query) %in% .obj_nm]
      attr(bars, "symbol") <- .sym
    }

  } else {
    bars <- purrr::when(.is_list,
                isTRUE(.) ~ purrr::imap(.quote, bars_tidy, timeframe = timeframe),
                ~ bars_tidy(.quote, .query$symbol %||% .query$ticker, timeframe))
    bars <- purrr::when(.is_list,
                        isTRUE(.) ~ purrr::imap(bars, to_tsymble, query = .query, interval = rlang::list2(!!timeframe := multiplier)),
                        ~ to_tsymble(bars, query = .query, interval = rlang::list2(!!timeframe := multiplier)))
  }

  return(bars)
}


to_tsymble <- function(quote, symbol = NULL, query, interval) {
  if (NROW(quote) > 0 || !rlang::is_empty(quote)) { # handle empty
    as_tsymble(quote, index = "time", query = query, symbol = symbol %||% query$symbol %||% query$ticker, interval = do.call(tsibble::new_interval, interval))
  } else {
    quote
  }
}

bars_tidy <- function(.x, .y, timeframe, evar = get0("evar", mode = "environment", envir = rlang::caller_env())) {
  force(evar)
  if (!inherits(.x, c("data.frame", "list")) || rlang::is_empty(.x)) {
    message(paste0("The symbol ", .y, " returned no data."))
    return(NULL)
  }
  timeframe <- ifelse(timeframe %in% evar$tqs, "minute", timeframe)
  if (inherits(.x, "list"))
    .x <- tibble::as_tibble(.x)
  .t <- switch(
    timeframe,
    minute = ,
    hour = rlang::expr(lubridate::round_date(
      try_date(t, timeframe, tz = "America/New_York"), timeframe
    )),
    day = ,
    week = ,
    month = ,
    quarter = ,
    year = rlang::expr(try_date(t, timeframe, tz = "UTC")),

  )


  .cols <- c(time = "t",
             open = "o",
             high = "h",
             low = "l",
             close = "c",
             volume = "v",
             n = "n",
             vw = "vw"
             )
  if (sum(names(.x) %in% .cols) < 3)
    .cols <- c(time = "t")

  out <- dplyr::mutate(.x,
                       t = !!.t,
                       dplyr::across(
                         tidyselect::any_of(unname(.cols[-1])),
                         ~ as.numeric(.x)
                       )
  )

  out <- dplyr::select(
    out,
    tidyselect::any_of(.cols),
    everything()
  )
}


#' @title bars_missing
#' @keywords Internal
#' @description A wrapper for \code{\link[tsibble]{count_gaps}}
#' @param x \code{(tsymble)}
#' @inheritParams market_data
#' @inheritParams bars_bounds
#' @inheritDotParams market_data

bars_missing <- function(x, ..., evar = get0("evar", mode = "environment", envir = rlang::caller_env())) {
  force(evar)
  fetch_vars(evar$.vn[c("v", "timeframe")], ...)
  # get the index
  .ind <- tsibble::index_var(x)
  # get the expected boundaries relative to the timeframe

  # must handle conversion to numeric for year, and coerce to tz UTC to match returned data
  .tail <- switch(as.character(timeframe),
         minute = ,
         hour = list(begin = lubridate::floor_date(evar$bounds[[1]], timeframe),
                     end = lubridate::ceiling_date(evar$bounds[[2]], timeframe)),
         day = ,
         week = ,
         month = ,
         quarter = list(
           begin = try_date(evar$bounds[[1]], timeframe, tz = "UTC"),
           end = try_date(evar$bounds[[2]], timeframe, tz = "UTC")),
         year = list(
           begin = lubridate::year(evar$bounds[[1]]),
           end = lubridate::year(evar$bounds[[2]])
         )
         )

  # add the beginning if not present..
  if (!.tail$begin %in% x[[.ind]])
    x <- suppressMessages(add_row(x, !!.ind := .tail[[1]], .before = 1))
  # and end if not present...
  if (!.tail$end %in% x[[.ind]])
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
#' @inheritParams market_data
#' @return \code{(\link[lubridate]{interval})} of the time span returned by the API. Saved to the `ext` environment.
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
#' @inheritParams bars_span
#' @inheritParams market_data
#' @inheritParams bars_bounds

gap_max <- function(ext, timeframe, evar = get0("evar", mode = "environment", envir = rlang::caller_env())) {
  force(evar)
# the max gap only needs to be determined once.
  evar$max_gap <- evar$max_gap %||% {
    .int <- time_interval(ext$out)
    if (.int$timeframe == "quarter") {
      .int <- lubridate::duration(3, "months") * .int$multiplier
    } else {
      .int <- do.call(lubridate::duration, unname(.int))
    }

    .gap_idx <- which.max(diff(evar$cal$date))
    if (!rlang::is_empty(.gap_idx) && !timeframe %in% c("minute", "hour")) {
      .max <- lubridate::as.duration(difftime(
        lubridate::int_end(evar$cal$day[(.gap_idx + 1)]),
        lubridate::int_start(evar$cal$day[.gap_idx])
      )) / .int
    } else {
      .max <- .int
    }
    .max
  }
  ext$missing <- bars_missing(ext$out, timeframe = timeframe)
  dplyr::slice_max(dplyr::filter(ext$missing, lubridate::duration(.n, timeframe) > evar$max_gap), .n, n = 1)
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
  force(evar)
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
      evar[[ifelse(timeframe == "day", "cal", "cal_exp")]][[ifelse(timeframe == "day", "date", "time")]]  %>%
        # the time points not already present
        `[`(!. %in% ext$out[[.idx]]) %>%
        # within the bounds
        `[`(lubridate::`%within%`(., do.call(lubridate::interval, unname(evar$bounds))))
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
  force(evar)
  .vn <- evar$.vn[c("v", "unadjusted", "limit", "full", "timeframe", "multiplier")]
  fetch_vars(.vn, ...)
  # If no data was returned, no new data is going to be returned. Return NULL
  if (is.null(get_tibble(bars))) return(NULL)
  if (is.data.frame(bars)) bars <- list(bars)
  .newbars <- purrr::map(bars, ~{
    ext <- rlang::env(
      out = .x,
      index = tsibble::index(.x),
      symbol = get_sym(.x),
      count = 0
    )

    #} else {

      .go <- gap_identify(ext, timeframe)# adds missing to ext

      while (isFALSE(.go)) {

        # only fetch data if there is data to fetch
        .args <- list(
          bounds = setNames(as.list(ext$gap[c(".from", ".to")]), c("from", "to")),
          timeframe = timeframe,
          multiplier = multiplier,
          symbol = ext$symbol
        )
        .nd <- suppressMessages(bars_get(do.call(bars_url, .args), timeframe = timeframe, multiplier = multiplier, ..., just_tibble = TRUE))
        bars_span(.nd, ext, timeframe)
        ext$out <- bind_rows(ext$out, .nd)

        # this count comes from identify_gap - delays break until tails are added.
        .go <- gap_identify(ext, timeframe)
      }
      # sort added queries
      .query <- get_query(ext$out)
      if (is.null(.query$ts)) .query <- .query[order(purrr::map_dbl(.query, "ts"))]
      attr(ext$out, "query") <- .query
    #}
    return(dplyr::arrange(ext$out, !!ext$index))
  })
  if (length(.newbars) == 1)
    .newbars <- get_tibble(.newbars)
  return(.newbars)
}




#' @title Construct environment that stores top-level user-supplied variables for `market_data`
#' @description \lifecycle{stable} bind top-level user-supplied and derived variables in `evar` environment for availability in internal functions
#' @inheritDotParams market_data
#' @param ... additional arguments to assign
#' @param cenv Calling environment in which to assign variables
#' @keywords internal

evar_bind <- function(..., evar = get0("evar", mode = "environment", envir = rlang::caller_env())) {
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


  # if v = "polygon", or if v comes in as character number
  if (!is.na(suppressWarnings(as.numeric(v))))
    v <- as.numeric(v)
  else
    v <- "p"

  .idx <- list(v1 = c(1,3), v2 = 1:3, p = 1:7)

  if (v %in% 1:2) {
    # Last trade and quote handling  ----
    if (grepl("(?:^lt$)|(?:^tr$)|(?:^t$)|(?:^trade$)|(?:^last_trade$)|((?:^q$))|(?:^qu$)|(?:^lq$)|(?:^quote$)|(?:^last_quote$)|(?:^sn$)|(?:^snapshot$)|(?:^ss$)", timeframe, ignore.case = TRUE)) {
      is_tqs <- TRUE
      timeframe <- switch(tolower(timeframe),
                          lt = ,
                          last_trade = "lt",
                          t = ,
                          tr = ,
                          trade = "tr",
                          lq = ,
                          last_quote = "lq",
                          q = ,
                          qu = ,
                          quote = "qu",
                          sn = ,
                          ss = ,
                          snapshot = "ss",
      )

      # .url <- bars_url(symbol = symbol, timeframe = timeframe, from = from, to = to, after = after, until = until)
      # .out <- bars_get(.url, timeframe = timeframe)
      # return(.out)
    } else {

      .t <- match_letters(timeframe, .tf_opts[.idx[[v]]], ignore.case = TRUE, multiple = FALSE)
      if (rlang::is_empty(.t) || is.na(.t)) {
        stop(paste0("`",timeframe,"` is not a valid timeframe for the v",v," API.\nSee ?market_data documentation."))
      }
      # Get the timeframe
      timeframe <- utils::tail(.tf_opts[.idx[[v]]][[.t]], 1)

      # Check args
      if (v == 1) {
        if (timeframe == "minute" && !any(multiplier %in% c(1,5,15))) {
          rlang::abort("The v1 API only accepts multipliers of 1,5,15 for the minute timeframe")
        } else if (timeframe == "day" && multiplier != 1) {
          rlang::warn("The v1 API only accepts 1 as a `multiplier` for the day timeframe. One day bars will be returned.", class = "warning")
          multiplier <- 1
        }
      } else if (v == 2) {
        if (multiplier != 1) {
          multiplier <- 1
          rlang::warn("The v2 API only accepts 1 as a `multiplier` for all timeframes. `multiplier` coerced to 1.")
        }
      }
    }

  } else if (v == "p"){
    timeframe <- utils::tail(.tf_opts[[grep(substr(ifelse(timeframe %in% c("M", "month"), "Month", as.character(timeframe)), 0 , 1), names(.tf_opts), ignore.case = FALSE)[1]]], 1)
  }
  # Get the timeframe as a numeric
  tf_num <- which(tf_order %in% timeframe)
  rlang::env_bind(evar, timeframe = timeframe, multiplier = multiplier, tf_num = tf_num, tf_order = tf_order, v = v, is_tqs = is_tqs)
  # If bounds not created, create, if passed as variable, bind.
  if (!"bounds" %in% names(.vars)) {
    bounds <- bars_bounds(from = from, to = to, after = after, until = until)
  }

  rlang::env_bind(evar, bounds = bounds)



  if (timeframe %in% c("minute", "hour", "day") && !"cal" %in% ls(evar)) {
    # calendar is only necessary when timeframe is less than week
    .args <- list(.env = evar,
         cal = rlang::exec(calendar, !!!unname(bounds)))
    if (timeframe %in% c("minute", "hour"))
      .args$cal_exp <- expand_calendar(.args$cal, timeframe, multiplier)
    do.call(rlang::env_bind, .args)
  }
  # Stop if malformed date argument with informative message
  if (any(purrr::map_lgl(bounds, is.na))) rlang::abort(paste0("Check the following argument(s) format: `", names(purrr::keep(bounds, ~{is.null(.x)||is.na(.x)})), "`"))

}
