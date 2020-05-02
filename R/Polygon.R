

# PACKAGE FUNCTIONS #
#===================================================================================================
# Regex for changing return to itemized lists.
#\@return\s\"(\w+)\"\s+([\/\w\s\(\)\,\.\-\+]+)
#\item{`$1` `(character)` $2}

#Exported Functions:  Sun Jan 12 10:20:51 2020 ----

#Functions to interact with Alpaca API have been moved to R files named according to their respective endpoints.

#Select Polygon Integration functions - Live Brokerage accounts are the only accounts with access to Polygon.

#----------------------------------------------------------------------------------------------


#Best function to use if you want to see a little more than just pricing data for your company. 
#My favorite are the analyst estimates and news endpoints. I am not 100% sure how quickly the news links are updated, but I am very interested and will try to reach out regarding this. 
#I decided to integrate all endpoints into one function and the user can either call a specific endpoint, or call none.


.ep <- list(
  t = list(nm = "Tickers",
           desc = "Query all ticker symbols which are supported by Polygon.io. This API includes Indices, Crypto, FX, and Stocks/Equities.",
           href = "https://polygon.io/docs/#get_v2_reference_tickers_anchor",
           url = "/v2/reference/tickers",
           params = list(sort = c("ticker", "-ticker", "type"), type = list(NULL, "etp", "cs"), market = list(NULL, "stocks", "indices"), locale = list(NULL, "us", "g"), search = list(NULL, "microsoft"), perpage = 50, page = 1, active = list(NULL, T, F))),
  tt = list(nm = "Ticker Types",
            desc = "Get the mapping of ticker types to descriptions / long names",
            href = "https://polygon.io/docs/#get_v2_reference_types_anchor",
            url = "/v2/reference/types"),
  td = list(nm = "Ticker Details",
            desc = "Get the details of the symbol company/entity. These are important details which offer an overview of the entity. Things like name, sector, description, logo and similar companies.",
            href = "https://polygon.io/docs/#get_v1_meta_symbols__symbol__company_anchor",
            url = "/v1/meta/symbols/{symbol}/company",
            params = list(symbol = "AAPL")),
  tn = list(nm = "Ticker News",
            desc = "Get news articles for this ticker.",
            href = "https://polygon.io/docs/#get_v1_meta_symbols__symbol__news_anchor",
            url = "/v1/meta/symbols/{symbol}/news",
            params = list(symbol = "AAPL", perpage = 50, page = 1)),
  m = list(nm = "Markets",
           desc = "Get the list of currently supported markets",
           href = "https://polygon.io/docs/#get_v2_reference_markets_anchor",
           url = "/v2/reference/markets"),
  l = list(nm = "Locales",
           desc = "Get the list of currently supported locales",
           href = "https://polygon.io/docs/#get_v2_reference_locales_anchor",
           url = "/v2/reference/locales"),
  ss = list(nm = "Stock Splits",
            desc = "Get the historical splits for this symbol.",
            href = "https://polygon.io/docs/#get_v2_reference_splits__symbol__anchor",
            url = "/v2/reference/splits/{symbol}",
            params = list(symbol = "AAPL")),
  sd = list(nm = "Stock Dividends",
            desc = "Get the historical dividends for this ticker.",
            href = "https://polygon.io/docs/#get_v2_reference_dividends__symbol__anchor",
            url = "/v2/reference/dividends/{symbol}",
            params = list(symbol = "AAPL")),
  sf = list(nm = "Stock Financials",
            desc = "Current status of each market",
            href = "https://polygon.io/docs/#get_v2_reference_financials__symbol__anchor",
            url = "/v2/reference/financials/{symbol}",
            params = list(symbol = "AAPL", limit = 5, type = list(NULL, "Y", "YA", "Q", "QA", "T", "TA"), sort = list(NULL, "reportPeriod", "-reportPeriod", "calendarDate", "-calendarDate"))),
  ms = list(nm = "Market Status",
            desc = "Current status of each market",
            href = "https://polygon.io/docs/#get_v1_marketstatus_now_anchor",
            url = "/v1/marketstatus/now"),
  mh = list(nm = "Market Holidays",
            desc = "Get upcoming market holidays and their open/close times",
            href = "https://polygon.io/docs/#get_v1_marketstatus_upcoming_anchor",
            url = "/v1/marketstatus/upcoming"),
  e = list(nm = "Exchanges", 
           desc = "List of stock exchanges which are supported by Polygon.io",
           href = "https://polygon.io/docs/#get_v1_meta_exchanges_anchor",
           url = "/v1/meta/exchanges"),
  ht = list(nm = "Historic Trades",
            desc = "Get historic NBBO quotes for a ticker.",
            href = "https://polygon.io/docs/#get_v2_ticks_stocks_trades__ticker___date__anchor",
            url = "/v2/ticks/stocks/trades/{ticker}/{date}",
            params = list(ticker = "AAPL", date = lubridate::as_date("2018-02-02"), timestamp = list(NULL, 1), timestampLimit = list(NULL,1), reverse = list(NULL, T, F), limit = c(10, 50000))),
  hq = list(nm = "Historic Quotes (NBBO)",
            desc = "Get historic NBBO quotes for a ticker.",
            href = "https://polygon.io/docs/#get_v2_ticks_stocks_nbbo__ticker___date__anchor",
            url = "/v2/ticks/stocks/nbbo/{ticker}/{date}",
            params = list(ticker = "AAPL", date = lubridate::as_date("2018-02-02"), timestamp = list(NULL, 1), timestampLimit = list(NULL,1), reverse = list(NULL, T, F), limit = c(10, 50000))),
  lt = list(nm = "Last Trade for a Symbol",
            desc = "Get the last trade for a given stock.",
            href = "https://polygon.io/docs/#get_v1_last_stocks__symbol__anchor",
            url = "/v1/last/stocks/{symbol}",
            params = list(symbol = "AAPL")),
  lq = list(nm = "Last Quote for a Symbol",
            desc = "Get the last quote tick for a given stock.",
            href = "https://polygon.io/docs/#get_v1_last_quote_stocks__symbol__anchor",
            url = "/v1/last_quote/stocks/{symbol}",
            params = list(symbol = "AAPL")),
  do = list(nm = "Daily Open/Close",
            desc = "Get the open, close and afterhours prices of a symbol on a certain date.",
            href = "https://polygon.io/docs/#get_v1_open-close__symbol___date__anchor",
            url = "/v1/open-close/{symbol}/{date}",
            params = list(symbol = "AAPL", date = lubridate::floor_date(Sys.Date(), unit = "week", 1))),
  cm = list(nm = "Condition Mappings",
            desc = "The mappings for conditions on trades and quotes.",
            href = "https://polygon.io/docs/#get_v1_meta_conditions__ticktype__anchor",
            url = "/v1/meta/conditions/{ticktype}",
            params = list(ticktype = c("trades", "quotes"))),
  sa = list(nm = "Snapshot - All tickers",
            desc = "Snapshot allows you to see all tickers current minute aggregate, daily aggregate and last trade. As well as previous days aggregate and calculated change for today.",
            href = "https://polygon.io/docs/#get_v2_snapshot_locale_us_markets_stocks_tickers_anchor",
            url = "/v2/snapshot/locale/us/markets/stocks/tickers"),
  st = list(nm = "Snapshot - Single Ticker",
            desc = "See the current snapshot of a single ticker",
            href = "https://polygon.io/docs/#get_v2_snapshot_locale_us_markets_stocks_tickers__ticker__anchor",
            url = "/v2/snapshot/locale/us/markets/stocks/tickers/{ticker}",
            params = list(ticker = "AAPL")),
  sg = list(nm = "Snapshot - Gainers/Losers",
            desc = "See the current snapshot of the top 20 gainers or losers of the day at the moment.",
            href = "https://polygon.io/docs/#get_v2_snapshot_locale_us_markets_stocks__direction__anchor",
            url = "/v2/snapshot/locale/us/markets/stocks/{direction}",
            params = list(direction = c("gainers", "losers"))),
  pc = list(nm = "Previous Close",
            desc = "Get the previous day close for the specified ticker",
            href = "https://polygon.io/docs/#get_v2_aggs_ticker__ticker__prev_anchor",
            url = "/v2/aggs/ticker/{ticker}/prev",
          params = list(ticker = "AAPL", unadjusted = list(NULL, T, F))),
  a = list(nm = "Aggregates (Bars)",
           desc = "See ?market_data"),
  gd = list(nm = "Grouped Daily (Bars)",
            desc = "Get the daily OHLC for entire markets.",
            href = "https://polygon.io/docs/#get_v2_aggs_grouped_locale__locale__market__market___date__anchor",
            url = "/v2/aggs/grouped/locale/{locale}/market/{market}/{date}",
            params = list(locale = c("US", "See Locales Endpoint"), market = c("STOCKS", "See Markets Endpoint"), date = lubridate::as_date("2019-02-01"), unadjusted = list(NULL, T, F))),
  fht = list(nm = "Historic Forex Ticks",
             desc = "Get historic ticks for a currency pair. Example for USD/JPY the from would be USD and to would be JPY. The date formatted like 2017-6-22",
            href = "https://polygon.io/docs/#get_v1_historic_forex__from___to___date__anchor",
            url = "/v1/historic/forex/{from}/{to}/{date}",
            params = list(from = c("USD"), to = c("JPY"), date = lubridate::as_date("2019-02-01"), offset = list(NULL, 1), limit = c(100, 10000))),
  cc = list(nm = "Real-time Currency Conversion",
            desc = "Convert currencies using the latest market conversion rates. Note than you can convert in both directions. For example USD->CAD or CAD->USD.",
            href = "https://polygon.io/docs/#get_v1_conversion__from___to__anchor",
            url = "/v1/conversion/{from}/{to}",
            params = list(from = "USD", to = "CAD", amount = 100, precision = 2)),
  lq = list(nm = "Last Quote for a Currency Pair",
            desc = "Get Last Quote Tick for a Currency Pair.",
            href = "https://polygon.io/docs/#get_v1_last_quote_currencies__from___to__anchor",
            url = "/v1/last_quote/currencies/{from}/{to}",
            params = list(from = "USD", to = "CAD")),
  fsa = list(nm = "Forex: Snapshot - All Tickers",
             desc = "Snapshot allows you to see all tickers current minute aggregate, daily aggregate and last trade. As well as previous days aggregate and calculated change for today.",
             href = "https://polygon.io/docs/#get_v2_snapshot_locale_global_markets_forex_tickers_anchor",
             url = "/v2/snapshot/locale/global/markets/forex/tickers"),
  fsg = list(nm = "Forex: Snapshot - Gainers/Losers",
             desc = "See the current snapshot of the top 20 gainers or losers of the day at the moment.",
             href = "https://polygon.io/docs/#get_v2_snapshot_locale_global_markets_forex__direction__anchor",
             url = "/v2/snapshot/locale/global/markets/forex/{direction}",
             params = c("gainers", "losers")),
  ce = list(nm = "Crypto Exchanges",
            desc = "List of crypto currency exchanges which are supported by Polygon.io",
            href = "https://polygon.io/docs/#get_v1_meta_crypto-exchanges_anchor",
            url = "/v1/meta/crypto-exchanges"),
  clt = list(nm = "Crypto: Last Trade for a Crypto Pair",
             desc = "Get Last Trade Tick for a Currency Pair.",
             href = "https://polygon.io/docs/#get_v1_last_crypto__from___to__anchor",
             url = "/v1/last/crypto/{from}/{to}",
             params = list(from = "BTC", to = "USD")),
  cdo = list(nm = "Crypto: Daily Open/Close",
             desc = "Get the open, close prices of a symbol on a certain day.",
             href = "https://polygon.io/docs/#get_v1_open-close_crypto__from___to___date__anchor",
             url = "/v1/open-close/crypto/{from}/{to}/{date}",
             params = list(from = "BTC", to = "USD", date = lubridate::floor_date(Sys.Date(), "weeks", 1))),
  cht = list(nm = "Crypto: Historic Crypto Trades",
             desc = "Get historic trade ticks for a crypto pair.",
             href = "https://polygon.io/docs/#get_v1_historic_crypto__from___to___date__anchor",
             url = "/v1/historic/crypto/{from}/{to}/{date}",
             params = list(from = "BTC", to = "USD", date = lubridate::floor_date(Sys.Date(), "weeks", 1), offset = list(NULL, 1), limit = c(100, 10000))),
  csa = list(nm = "Crypto: Snapshot - All Tickers",
             desc = "Snapshot allows you to see all tickers current minute aggregate, daily aggregate and last trade. As well as previous days aggregate and calculated change for today.",
             href = "https://polygon.io/docs/#get_v2_snapshot_locale_global_markets_crypto_tickers_anchor",
             url = "/v2/snapshot/locale/global/markets/crypto/tickers"),
  cst = list(nm = "Crypto: Snapshot - Single Ticker",
             desc = "See the current snapshot of a single ticker",
             href = "https://polygon.io/docs/#get_v2_snapshot_locale_global_markets_crypto_tickers__ticker__anchor",
             url = "/v2/snapshot/locale/global/markets/crypto/tickers/{ticker}",
             params = list(ticker = "~BTCUSD")),
  cst2 = list(nm = "Crypto: Snapshot - Single Ticker Full Book (L2)",
              desc = "See the current level 2 book of a single ticker. This is the combined book from all the exchanges.",
              href = "https://polygon.io/docs/#get_v2_snapshot_locale_global_markets_crypto_tickers__ticker__book_anchor",
              url = "/v2/snapshot/locale/global/markets/crypto/tickers/{ticker}/book",
              params = list(ticker = "~BTCUSD")),
  csg = list(nm = "Crypto: Snapshot - Gainers/Losers",
             desc = "See the current snapshot of the top 20 gainers or losers of the day at the moment.",
             href = "https://polygon.io/docs/#get_v2_snapshot_locale_global_markets_crypto__direction__anchor",
             url = "/v2/snapshot/locale/global/markets/crypto/{direction}",
             params = list(direction = c("gainers", "losers")))
  
)

# Generate help text
# purrr::imap_chr(.ep, ~{
#   if (.y == "a") {
#     return(paste0("#' \\item{`a`}{Aggregates - see ?market-data}"))
#   }
#     
#   .out <- glue::glue("#' \\item{`{{.y}}`}{[{{.x$nm}}]({{.x$href}}):\n", .open = "{{", .close = "}}")
#   .out <- paste(.out, .x$desc)
#   if (!is.null(.x$params)) {
#     if (stringr::str_detect(.x$url, '\\{')) {
#       .r <- stringr::str_match_all(.x$url, '\\{(\\w+)\\}')[[1]][,2]
#     }
#     .params <- .x[["params"]]
#     .out <- paste(.out, glue::glue(" | Arguments: {{paste0(purrr::imap_chr(.params, ~{
#     .c <- ifelse(is.list(.x), class(.x[[2]]), class(.x))
#     .r <- get0('.r')
#     if (.y %in% .r) .y <- paste0('**',.y,'**')
#     .out <- paste0(.y,glue::glue(' = `({stringr::str_sub(.c,0,3)})` '), paste0(purrr::map_chr(.x, ~{ifelse(is.null(.x), '*Optional* NULL', as.character(.x))}), collapse = ', '))}), collapse = ', ')}}", .open = "{{", .close = "}}"))
#   }
#   paste0(.out, "}")
#   }) %>% cat(sep = "\n") 
# polygon ----
# Thu Apr 23 14:53:56 2020

#' @title Access all Polygon.io Endpoints
#' @family Polygon
#' 
#' @description  This function calls all Endpoints from the [Polygon API docs](https://polygon.io/docs).
#' @param ep `(character)` Non-case-sensitive reference to the endpoint to be called as an abbreviation or by name. See Details for full endpoint details. Use the following keywords to quick-view abbreviations and full names for endpoints while programming (add a '+' to any keyword to return a list with full details: abbv, name, url, description, parameters etc):
#' \itemize{
#'   \item{\code{'all'}}{ View all endpoints that Alpaca users have access to.}
#'  \item{\code{'ref'/'reference'}}{ View all reference endpoints.}
#'  \item{\code{'sto'/'stocks'}}{ View all stock endpoints}
#' }
#' Alternatively, if you know how to call the endpoint but would like a list of it's details, add '+' to the end of it (ie 'tt+' or 'ticker types+'.
#'@details Optional query parameters are mentioned if present. See the [Polygon API docs](https://polygon.io/docs) for details on parameters and response data. Required path parameters are in **bold**. The first option for each required parameter is used as a default if none are specified. Endpoints are:
#' \itemize{
#' \item{`t`}{[Tickers](https://polygon.io/docs/#get_v2_reference_tickers_anchor): Query all ticker symbols which are supported by Polygon.io. This API includes Indices, Crypto, FX, and Stocks/Equities. | Arguments: sort = `(cha)` ticker, -ticker, type, type = `(cha)` *Optional* NULL, etp, cs, market = `(cha)` *Optional* NULL, stocks, indices, locale = `(cha)` *Optional* NULL, us, g, search = `(cha)` *Optional* NULL, microsoft, perpage = `(num)` 50, page = `(num)` 1, active = `(log)` *Optional* NULL, TRUE, FALSE}
#' \item{`tt`}{[Ticker Types](https://polygon.io/docs/#get_v2_reference_types_anchor): Get the mapping of ticker types to descriptions / long names}
#' \item{`td`}{[Ticker Details](https://polygon.io/docs/#get_v1_meta_symbols__symbol__company_anchor): Get the details of the symbol company/entity. These are important details which offer an overview of the entity. Things like name, sector, description, logo and similar companies. | Arguments: **symbol** = `(cha)` AAPL}
#' \item{`tn`}{[Ticker News](https://polygon.io/docs/#get_v1_meta_symbols__symbol__news_anchor): Get news articles for this ticker. | Arguments: **symbol** = `(cha)` AAPL, perpage = `(num)` 50, page = `(num)` 1}
#' \item{`m`}{[Markets](https://polygon.io/docs/#get_v2_reference_markets_anchor): Get the list of currently supported markets}
#' \item{`l`}{[Locales](https://polygon.io/docs/#get_v2_reference_locales_anchor): Get the list of currently supported locales}
#' \item{`ss`}{[Stock Splits](https://polygon.io/docs/#get_v2_reference_splits__symbol__anchor): Get the historical splits for this symbol. | Arguments: **symbol** = `(cha)` AAPL}
#' \item{`sd`}{[Stock Dividends](https://polygon.io/docs/#get_v2_reference_dividends__symbol__anchor): Get the historical dividends for this ticker. | Arguments: **symbol** = `(cha)` AAPL}
#' \item{`sf`}{[Stock Financials](https://polygon.io/docs/#get_v2_reference_financials__symbol__anchor): Current status of each market | Arguments: **symbol** = `(cha)` AAPL, limit = `(num)` 5, type = `(cha)` *Optional* NULL, Y, YA, Q, QA, T, TA, sort = `(cha)` *Optional* NULL, reportPeriod, -reportPeriod, calendarDate, -calendarDate}
#' \item{`ms`}{[Market Status](https://polygon.io/docs/#get_v1_marketstatus_now_anchor): Current status of each market}
#' \item{`mh`}{[Market Holidays](https://polygon.io/docs/#get_v1_marketstatus_upcoming_anchor): Get upcoming market holidays and their open/close times}
#' \item{`e`}{[Exchanges](https://polygon.io/docs/#get_v1_meta_exchanges_anchor): List of stock exchanges which are supported by Polygon.io}
#' \item{`ht`}{[Historic Trades](https://polygon.io/docs/#get_v2_ticks_stocks_trades__ticker___date__anchor): Get historic NBBO quotes for a ticker. | Arguments: **ticker** = `(cha)` AAPL, **date** = `(Dat)` 2018-02-02, timestamp = `(num)` *Optional* NULL, 1, timestampLimit = `(num)` *Optional* NULL, 1, reverse = `(log)` *Optional* NULL, TRUE, FALSE, limit = `(num)` 10, 50000}
#' \item{`hq`}{[Historic Quotes (NBBO)](https://polygon.io/docs/#get_v2_ticks_stocks_nbbo__ticker___date__anchor): Get historic NBBO quotes for a ticker. | Arguments: **ticker** = `(cha)` AAPL, **date** = `(Dat)` 2018-02-02, timestamp = `(num)` *Optional* NULL, 1, timestampLimit = `(num)` *Optional* NULL, 1, reverse = `(log)` *Optional* NULL, TRUE, FALSE, limit = `(num)` 10, 50000}
#' \item{`lt`}{[Last Trade for a Symbol](https://polygon.io/docs/#get_v1_last_stocks__symbol__anchor): Get the last trade for a given stock. | Arguments: **symbol** = `(cha)` AAPL}
#' \item{`lq`}{[Last Quote for a Symbol](https://polygon.io/docs/#get_v1_last_quote_stocks__symbol__anchor): Get the last quote tick for a given stock. | Arguments: **symbol** = `(cha)` AAPL}
#' \item{`do`}{[Daily Open/Close](https://polygon.io/docs/#get_v1_open-close__symbol___date__anchor): Get the open, close and afterhours prices of a symbol on a certain date. | Arguments: **symbol** = `(cha)` AAPL,  = `(Dat)` 2020-04-20}
#' \item{`cm`}{[Condition Mappings](https://polygon.io/docs/#get_v1_meta_conditions__ticktype__anchor): The mappings for conditions on trades and quotes. | Arguments: **ticktype** = `(cha)` trades, quotes}
#' \item{`sa`}{[Snapshot - All tickers](https://polygon.io/docs/#get_v2_snapshot_locale_us_markets_stocks_tickers_anchor): Snapshot allows you to see all tickers current minute aggregate, daily aggregate and last trade. As well as previous days aggregate and calculated change for today.}
#' \item{`st`}{[Snapshot - Single Ticker](https://polygon.io/docs/#get_v2_snapshot_locale_us_markets_stocks_tickers__ticker__anchor): See the current snapshot of a single ticker | Arguments: **ticker** = `(cha)` AAPL}
#' \item{`sg`}{[Snapshot - Gainers/Losers](https://polygon.io/docs/#get_v2_snapshot_locale_us_markets_stocks__direction__anchor): See the current snapshot of the top 20 gainers or losers of the day at the moment. | Arguments: **direction** = `(cha)` gainers, losers}
#' \item{`pc`}{[Previous Close](https://polygon.io/docs/#get_v2_aggs_ticker__ticker__prev_anchor): Get the previous day close for the specified ticker | Arguments: **ticker** = `(cha)` AAPL, unadjusted = `(log)` *Optional* NULL, TRUE, FALSE}
#' \item{`a` }{Aggregates - see \code{\link[AlpacaforR]{market_data}}}
#' \item{`gd`}{[Grouped Daily (Bars)](https://polygon.io/docs/#get_v2_aggs_grouped_locale__locale__market__market___date__anchor): Get the daily OHLC for entire markets. | Arguments: **locale** = `(cha)` US, See Locales Endpoint, **market** = `(cha)` STOCKS, See Markets Endpoint, **date** = `(Dat)` 2019-02-01, unadjusted = `(log)` *Optional* NULL, TRUE, FALSE}
#' }
#' @param ... Named arguments specific to the endpoint parameters. These can also be specified in a list via the `params` argument.
#' @param params `(list)` A named list of parameters specific to the endpoint.
#' @return Response `(tibble/list/data.frame)` depending on the endpoint. The core data of the response will be returned as the object. If query data is returned in addition to the core object, it is stored as a `"query"` attribute and accessed via `attr(object, "query")`. If a map object is returned with the response, it will be stored as a `"map"` attribute and accessed via `attr(object, "map")`.
#' @details This function is not vectorized. Only a single endpoint may be called at a time. Thus any endpoints with path parameters (parameters denoted in bold) require that a single combination of path parameters be passed for a given call.
#' @importFrom rlang env_bind current_env `!!!` abort
#' @importFrom glue glue_data
#' @importFrom purrr compact map
#' @importFrom httr GET parse_url build_url
#' @importFrom stringr str_match str_extract regex
#' @export

polygon <- function(ep = NULL, ..., params = NULL){
  if (is.null(ep)) rlang::abort("Endpoint required. See ?AlpacaforR::polygon for details.")
  # get endpoint object
  ep <- tolower(ep)
  # quick-view
  .qv <- substr(ep, 0 ,3)
  .ref <- grepl("\\+", ep)
  ep <- gsub("\\+","",ep)
  if (.qv %in% c("all", "ref", "sto")) {
    qv <- .ep[list(all = c(1:24), ref = c(1:6), sto = c(7:24))[[.qv]]]
    if (.ref) {
      return(qv)
    } else {
      return(data.frame(name = purrr::map_chr(qv, purrr::pluck, "nm")))
    }
  }
  .url <- httr::parse_url(get_url_poly())
  params <- append(list(...), params)
  if (ep %in% names(.ep)) {
    e_p <- .ep[[ep]]
  } else {
    if (stringr::str_detect(ep, stringr::regex("Single Ticker", ignore_case = T))) {
      .suf <- "t"
    } else {
      .suf <- ifelse(is.na(stringr::str_extract(ep, "(?<=\\s)\\w")), "", stringr::str_extract(ep, "(?<=\\s)\\w"))
    }
    ep <- paste0(stringr::str_match(ep, "^\\w"), .suf)
    e_p <- .ep[[ep]]
  }
  if (.ref) {
    return(e_p)
  }
  if (ep == "a") {
    rlang::abort("See ?market_data for aggregates endpoint.")
  } else if (is.null(e_p)) {
    rlang::abort("No matching endpoint for ep argument. See ?polygon.")
  }
  # add defaults for parameters not specified
  params <- append(params, purrr::map(.ep[[ep]]$params[!names(.ep[[ep]]$params) %in% names(params)], `[[`, 1))
  # assign the appropriate path
  .pv <- stringr::str_detect(e_p$url, "\\{")
  if (.pv) {
    # get the path variable names
    .pv <- stringr::str_match_all(e_p$url, "\\{(\\w+)\\}")[[1]][,2]
    # add the variables into the url
    .url$path <- glue::glue_data(purrr::map_chr(params[.pv], as.character), e_p$url)
    # Remove path vars from params
    params <- params[!names(params) %in% .pv]
  } else {
    .url$path <- e_p$url
  }
  
  
  # Set query parameters based on what's provided and the defaults
  if (length(params) > 0) {
    .url$query <- purrr::compact(params)
  }
  .url$query <- append(.url$query, list(apiKey = Sys.getenv("APCA-LIVE-API-KEY-ID")))
  .url <- httr::build_url(.url)
  .resp <- httr::GET(url = .url)
  out <- poly_transform(.resp, ep = ep)
  return(out)
}




#----------------------------------------------------------------------------------------------
#' @family Polygon
#' @title Get Polygon Meta Data (Deprecated)
#' 
#' @description Deprecated. See \code{\link[AlpacaforR]{polygon}}.
#' This function provides more color on your stock through its available meta data endpoints from Polygon. These endpoints are company, analysts, dividends, earnings, and news.
#' @param ticker Specify which symbol you want to call by inserting ticker as a string.
#' @param endpoint Select either company for a company profile, analysts for all kinds of analyst estimates, dividends to view historic and upcoming dividends, earnings for historic and current earning details , or news for news updates from CNBC, Seeking Alpha, etc.
#' @param perpage This is only used if "news" was your selected endpoint. How many articles do you want to see perpage?
#' @param v `(integer)` The Polygon API version number. 
#' @return A list object containing all information the API responds with. 
#' @examples
#' # Getting default meta for AMZN: 
#' get_meta(ticker = "AMZN")
#' # Getting news information on AMZN: 
#' get_meta(ticker = "AMZN", endpoint = "news", perpage = 100)
#' @importFrom httr GET
#' @export
get_meta <- function(ticker = NULL, endpoint = NULL, perpage = NULL, v = 1){
  message(paste0("This function is deprecated, see ?AlpacaforR::polygon"))
  #Set URL 
  path_url = get_url_poly()
  
  
  if(is.null(ticker)){
    stop("Please enter a ticker for the stock that you want.")
  }
  
  #If no endpoint entered, then keep default behavior. If a endpoint was provided, then request a call to that endpoint.
  if(is.null(endpoint)){
    full_path_url = paste0(path_url,"/",version,"/meta/symbols/",ticker,"?apiKey=",Sys.getenv("APCA-LIVE-API-KEY-ID"))
    
  } else if(endpoint == "company"){
    full_path_url = paste0(path_url,"/",version,"/meta/symbols/",ticker,"/company","?apiKey=",Sys.getenv("APCA-LIVE-API-KEY-ID"))
    
  } else if(endpoint == "analysts"){
    full_path_url = paste0(path_url,"/",version,"/meta/symbols/",ticker,"/analysts","?apiKey=",Sys.getenv("APCA-LIVE-API-KEY-ID"))
    
  } else if(endpoint == "dividends"){
    full_path_url = paste0(path_url,"/",version,"/meta/symbols/",ticker,"/dividends","?apiKey=",Sys.getenv("APCA-LIVE-API-KEY-ID"))
    
  } else if(endpoint == "earnings"){
    full_path_url = paste0(path_url,"/",version,"/meta/symbols/",ticker,"/earnings","?apiKey=",Sys.getenv("APCA-LIVE-API-KEY-ID"))
    
  } else if (endpoint == "news"){
    ifelse(is.null(perpage),
           full_path_url <- paste0(path_url,"/",version,"/meta/symbols/",ticker,"/news","?perpage=50","&apiKey=",Sys.getenv("APCA-LIVE-API-KEY-ID")),
           full_path_url <- paste0(path_url,"/",version,"/meta/symbols/",ticker,"/news","?perpage=",perpage,"&apiKey=",Sys.getenv("APCA-LIVE-API-KEY-ID")))
  }
  
  #Send Request
  meta = last_price_details = httr::GET(url = full_path_url)
  meta = response_text_clean(meta)
  return(meta)
}
#----------------------------------------------------------------------------------------------

#get_meta(ticker = "AAPL",endpoint = "news")


#----------------------------------------------------------------------------------------------
#' @family Polygon
#' @title Get Polygon Aggregate Pricing Data (Deprecated)
#' 
#' @description (Deprecated, use \code{\link[AlpacaforR]{market_data}} instead). This function provides aggregate pricing data from Polygon. Polygon provides consolidated market data. Consolidated stock market data is an aggregated reporting of all securities exchanges’ and alternative trading venues’ quote and trade data. It is the most relied upon type of market data, providing investors and traders globally with a unified view of U.S. stock market prices and volumes. It also underpins the National Best Bid and Offer (NBBO), which provides investors with a continuous view of the best available displayed buy and sell prices, and through Rule 611 ensures that investors receive the best available displayed prices on their trades, with a few exceptions. For more info see the [Polygon.io API Documentation for the Aggregate endpoint](https://polygon.io/docs/#get_v2_aggs_ticker__ticker__range__multiplier___timespan___from___to__anchor)
#' @param ticker \code{(character)} Specify which symbol(s) you want to call by inserting ticker(s) as a single string or character vector.
#' @param multiplier \code{(integer)} Size of the timespan multiplier. Defaults to 1.
#' @param timespan \code{(character)} Size of the time window i.e "minute" (m), "hour" (h), "day"(d), "week"(w), month"(M/mo), "quarter"(q), or "year"(y). Defaults to day.
#' @param from \code{(character/Date/POSIXct)} The starting date for the pricing data in the form YYYY-MM-DD. Datetimes/POSIXct will be coerced to Dates. 
#' @param to \code{(character/Date/POSIXct)} The ending date for the pricing data in the form YYYY-MM-DD. Datetimes/POSIXct will be coerced to Dates.
#' @param unadjusted \code{(logical)} Set to true if the results should **NOT** be adjusted for splits. Defaults to FALSE.
#' @return \code{list} object for each ticker symbol containing a \code{data.frame} with the following columns:
#' \itemize{
#'  \item{\code{time}}{  the time of the bar as \code{POSIXct} in yyyy-mm-dd for timeframe = day, and yyyy-mm-dd hh:mm:ss for timeframes < day}
#'  \item{\code{open}}{  open price as a numeric object.}
#'  \item{\code{high}}{  high price as a numeric object.}
#'  \item{\code{low}}{  low price as a numeric object.}
#'  \item{\code{close}}{  close price as a numeric object.}
#'  \item{\code{volume}}{  volume (in millions) as a numeric object.}
#'  \item{\code{vw}}{ weighted volume? *Note:* Polygon does not document what this variable is. }
#'  \item{\code{n}}{ Number of items in aggregate window }
#' }
#' each \code{data.frame} has a "query" attribute with the following metadata for the query returned by the Polygon API (accessed via `attr(df, "query")`):
#' \itemize{
#' \item{\code{ticker}} {\code{(string)} Ticker symbol requested}
#' \item{\code{status}} {\code{(string)} Status of the response}
#' \item{\code{queryCount}} {\code{(integer)} Number of aggregate ( min or day ) used to generate the response}
#' \item{\code{resultsCount}} {\code{(integer)} Total number of results generated}
#' \item{\code{adjusted}} {\code{(logical)} If this response was adjusted for splits}
#' }
#' @examples
#' # Getting default pricing data on AMZN (daily): 
#' get_poly_agg_quote(ticker = "AMZN",from = "2019-04-01", to = "2019-04-12")
#' # Getting minute pricing data on AMZN: 
#' get_poly_agg_quote("AMZN", from = "2019-04-11", to = "2019-04-12", timespan = "minute")
#' # Getting quarterly pricing data on AMZN: 
#' get_poly_agg_quote("AMZN", from = "2018-01-01", to = "2019-04-12", timespan = "quarter")
#' # Getting yearly pricing data on AMZN: 
#' get_poly_agg_quote("AMZN", from = "2015-01-01", to = "2019-12-31", timespan = "year")
#' @importFrom lubridate as_date
#' @importFrom purrr map
#' @importFrom httr GET
#' @export

# For DEBUG
get_poly_agg_quote <- function(ticker = NULL, multiplier = 1, timespan = "day", from = NULL, to = NULL, unadjusted = FALSE){
  message(paste0("get_poly_agg_quote is deprecated as of AlpacaforR 0.3.0. Please use `market_data` with v = 2 instead."))
  if(is.null(ticker)){
    stop("Please enter a stock ticker.")
  }
  if(is.null(from) | is.null(to)){
    stop("Please enter a date in the 'from' or 'to' argument.")
  }
  # quick detection of timespan abbreviations
  if (timespan == "m") {
    timespan <- "minute"
  } else if (timespan == "h") {
    timespan <- "hour"
  } else if (timespan == "d") {
    timespan <- "day"
  } else if (timespan == "w") {
    timespan <- "week"
  } else if (timespan == "M" || timespan == "mo") {
    timespan <- "month"
  } else if (timespan == "q") {
    timespan <- "quarter"
  } else if (timespan == "y") {
    timespan <- "year"
  }
  #Set URL
  path_url = get_url_poly()
  
  # Add names such that output list is labelled
  names(ticker) <- ticker
  
  agg_quotes <- purrr::map(ticker, ~{
    agg_quote <- list(results = list(n = NA))
    .ctr <- 1
    
      message(paste0("Retrieving data for: ", .x))
      full_path_url = paste0(path_url,"/v2/aggs/ticker/",.x,"/range/",multiplier,"/",timespan,"/",lubridate::as_date(from),"/",lubridate::as_date(to),"?unadjusted=",unadjusted,"&apiKey=",Sys.getenv("APCA-LIVE-API-KEY-ID")) 
      message(paste0("Fetching ", full_path_url))
      #Send Request
      agg_quote = httr::GET(url = full_path_url)
      if (agg_quote$status_code != 200) {
        message(paste("Ticker", .x, "returned status code", agg_quote$status_code, "- Returning NULL"))
        return(NULL)
      }
      agg_quote = response_text_clean(agg_quote)
      if (length(agg_quote$results) == 0) {
        message(paste("Ticker", .x, "returned no data", "- Returning response metadata only"))
        return(agg_quote[1:5])
      }
      # Get the query meta-info returned by the API
      query <- agg_quote[1:5]
    # convert from nanoseconds to seconds
    agg_quote$results$t = agg_quote$results$t/1000
    
    agg_quote <- bars_transform(agg_quote["results"])$results
    attr(agg_quote, "query") <- query
    return(agg_quote)
  })
  return(agg_quotes)
}
#----------------------------------------------------------------------------------------------









#----------------------------------------------------------------------------------------------
#' @family Polygon
#' @title Get Polygon Stock Split Information (Deprecated)
#' 
#' @description Deprecated. See \code{\link[AlpacaforR]{polygon}}.
#' This function provides stock split data for the specified ticker from Polygon.
#' @param ticker Specify which symbol you want to call by inserting ticker as a string.
#' @return A list object containing all information the API responds with. 
#' @examples
#' # Getting default pricing data on AMZN (daily): 
#' get_poly_stock_splits(ticker = "AMZN")
#' @importFrom httr GET
#' @export
get_poly_stock_splits <- function(ticker=NULL){
  message(paste0("This function is deprecated, see ?AlpacaforR::polygon"))
  if(is.null(ticker)){
    stop("Please enter the stocks ticker.")
  }
  
  #Set URL
  path_url = get_url_poly()
  full_path_url = paste0(path_url,"/v2/reference/splits/",ticker,"?apiKey=",Sys.getenv("APCA-LIVE-API-KEY-ID"))
  
  #Send Request
  split_info = last_price_details = httr::GET(url = full_path_url)
  split_info = response_text_clean(split_info)
  return(split_info)
}
#----------------------------------------------------------------------------------------------













#----------------------------------------------------------------------------------------------
#' @family Polygon
#' @title Get Polygon Historic Trades Information(Deprecated)
#' 
#' @description Deprecated. See \code{\link[AlpacaforR]{polygon}}.
#' This function provides historic trade data form Polygon on the ticker specified. It returns a list with values such as date, bid size / ask size, the exchanges for bid / ask, latency, each trade/quote listed for that date, time, etc. See the [Polygon.io/docs/](Polygon.io/docs/): [Historic Trades Endpoint](https://polygon.io/docs/#get_v2_ticks_stocks_trades__ticker___date__anchor) & [Historic Quotes Endpoint](https://polygon.io/docs/#get_v2_ticks_stocks_nbbo__ticker___date__anchor)
#' @param ticker `(character)` Specify which symbol you want.
#' @param type `(character)` Get information on either "t/trades" or "q/quotes".
#' @param date `(character/Date/POSIXct)` Specify the date for which you are requesting in YYYY-MM-DD format.
#' @param .ts `(Datetime)` Timestamp offset, used for pagination. This is the offset at which to start the results. Using the timestamp of the last result as the offset will give you the next page of results.
#' @param limit `(integer)` Limit the size of response, Max 50000. Default 10. 
#' @return `(data.frame)` Object containing all information specified in the Properties section of the Endpoint links above with the following attributes:
#' @return `(list)` Accessed via `attr(result, "query")` with query metadata as follows:
#' \itemize{
#'  \item{`success` `(logical)` If this query was executed successfully}
#'  \item{`ticker` `(character)` Ticker symbol that was evaluated from the request}
#'  \item{`results_count` `(integer)` Total number of results in this response}
#'  \item{`db_latency` `(integer)` Milliseconds of latency for the query results from DB}
#' } 
#' @return `(list)` Accessed via `attr(result, "map")`. The `map` of the results (identifiers and classes as returned by the API). `
#' @examples
#' # Getting historic trade data on AMZN: 
#' get_poly_historic_info(ticker = "AMZN", type = "trades", date = "2019-04-05")
#' # Getting historic pricing data on AMZN: 
#' get_poly_historic_info(ticker = "AMZN", type = "quotes", date = "2019-04-05")
#' @importFrom httr GET parse_url build_url
#' @importFrom stringr str_sub
#' @importFrom purrr compact
#' @importFrom lubridate is.POSIXct is.POSIXlt
#' @importFrom dplyr mutate_at rename select_if
#' @importFrom magrittr `%>%` not
#' @export
get_poly_historic_info <- function(ticker = NULL, type = NULL, date = NULL, .ts = NULL, v = 2, limit = 10){
  message(paste0("This function is deprecated, see ?AlpacaforR::polygon"))
  if(is.null(ticker) | is.null(type) | is.null(date)){
    stop("Please enter values for ticker, type, and date.")
  }
  # Check date integrity
  date <- try_date(date)
  if (is.na(date)) {
    stop("Date must be a Date-type class or a string in YYYY-MM-DD format")
  }
  # set the type
  type <- c(t = "trades", q = "nbbo")[tolower(stringr::str_sub(type, 1, 1))]
  # coerce timestamp to nanoseconds
  if (!is.null(.ts) && (lubridate::is.POSIXct(.ts) || lubridate::is.POSIXlt(.ts))) {
    .ts <- as.numeric(.ts) * 1e9
  }
  #Set URL 
  .url = httr::parse_url(get_url_poly())
  .url$path = purrr::compact(list(version = paste0("v",v),
                   "ticks",
                   "stocks",
                   type, 
                   ticker,
                   .ts,
                   as.character(date)))
  .url$query <- list(apiKey = Sys.getenv("APCA-LIVE-API-KEY-ID"),
                     limit = limit)
  .url <- httr::build_url(.url)
  #Send Request
  historic = httr::GET(url = .url)
  historic = response_text_clean(historic)
  # Make the t column human readable
  historic$results <- historic$results %>% 
    dplyr::mutate_at(dplyr::vars(t), ~lubridate::as_datetime({./(1e9)}, origin = lubridate::origin)) %>% 
    dplyr::rename(time = t) %>%
    dplyr::select_if(.p = ~!all(is.na(.)))
  
  # make the additional data into attributes
  attr(historic$results, "query") <- historic[-1]
  attr(historic$results, "map") <- historic$map[c("t",names(historic$results)[-1])]
  #Create a column for date/time
  
  return(historic$results)
}
#----------------------------------------------------------------------------------------------




#----------------------------------------------------------------------------------------------
#' @family Polygon
#' @title Get Polygon Last Price (Deprecated)
#' 
#' @description Deprecated. See \code{\link[AlpacaforR]{polygon}}.
#' This function provides the last listed price from Polygon. A list is returned with values such as the last price, last size, last exchange, and last timestamp.
#' @param ticker Specify which symbol you want to call by inserting ticker as a string.
#' @return A list object containing all information the API responds with. 
#' @examples
#' # Getting the last listed price for AMZN: 
#' get_poly_last_price("AMZN")
#' @importFrom httr GET
#' @export
get_poly_last_price <- function(ticker = NULL){
  message(paste0("This function is deprecated, see ?AlpacaforR::polygon"))
  if(is.null(ticker)){
    stop("Please enter the stocks ticker.")
  }
  
  #Set URL 
  path_url = get_url_poly()
  full_path_url = paste0(path_url,"/v1/last/stocks/",ticker,"?apiKey=",Sys.getenv("APCA-LIVE-API-KEY-ID"))
  
  #Send Request
  last_price_details = httr::GET(url = full_path_url)
  last_price_details = response_text_clean(last_price_details)
  
  #Convert unix epoch timestamp to readable date
  last_price_details$last$timestamp = as.POSIXct(last_price_details$last$timestamp/1000, origin= "1970-01-01")
  return(last_price_details)
}
#----------------------------------------------------------------------------------------------











#----------------------------------------------------------------------------------------------
#' @family Polygon
#' @title Get Polygon Last Trade (Deprecated)
#' 
#' @description Deprecated. See \code{\link[AlpacaforR]{polygon}}.
#' This function provides the last listed trade from Polygon for the ticker specified. A list is returned with values such as the last bid / ask price, bid / ask size, bid / ask exchange, and last trade timestamp.
#' @param ticker Specify which symbol you want to call by inserting ticker as a string.
#' @return A list object containing all information the API responds with. 
#' @examples
#' # Getting the last listed trade for AMZN: 
#' get_poly_last_trade("AMZN")
#' @importFrom httr GET
#' @export
get_poly_last_trade <- function(ticker = NULL){
  message(paste0("This function is deprecated, see ?AlpacaforR::polygon"))
  if(is.null(ticker)){
    stop("Please enter the stocks ticker.")
  }
  
  #Set URL 
  path_url = get_url_poly()
  full_path_url = paste0(path_url,"/v1/last_quote/stocks/",ticker,"?apiKey=",Sys.getenv("APCA-LIVE-API-KEY-ID"))
  
  #Send Request
  last_trade_details = httr::GET(url = full_path_url)
  last_trade_details = response_text_clean(last_trade_details)
  
  #Convert unix epoch timestamp to readable date
  last_trade_details$last$timestamp = as.POSIXct(last_trade_details$last$timestamp/1000, origin= "1970-01-01")
  return(last_trade_details)
}
#----------------------------------------------------------------------------------------------














#----------------------------------------------------------------------------------------------
#' @family Polygon
#' @title Get Polygon Daily OHLCV & After Hours (Deprecated)
#' 
#' @description Deprecated. See \code{\link[AlpacaforR]{polygon}}.
#' This function provides the last OHLCV data, including after hours data from Polygon for the ticker specified. 
#' @param ticker Specify which symbol you want to call by inserting ticker as a string.
#' @param date Specify the date for which you are requesting.
#' @return A list object containing all information the API responds with. 
#' @examples
#' # Getting the last listed trade for AMZN: 
#' get_poly_ohlc("AMZN", date = "2019-03-20")
#' @importFrom httr GET
#' @export
get_poly_ohlc <- function(ticker=NULL, date=NULL){
  message(paste0("This function is deprecated, see ?AlpacaforR::polygon"))
  if(is.null(ticker)){
    stop("Please enter the stocks ticker.")
  } 
  if(is.null(date)){
    date <- Sys.Date()
  }
  
  #Set URL 
  path_url = get_url_poly()
  full_path_url = paste0(path_url,"/v1/open-close/",ticker,"/",date,"?apiKey=",Sys.getenv("APCA-LIVE-API-KEY-ID"))
  
  #Send Request
  open_close = httr::GET(url = full_path_url)
  open_close = response_text_clean(open_close)
  return(open_close)
}
#----------------------------------------------------------------------------------------------







#----------------------------------------------------------------------------------------------
#' @family Polygon
#' @title Get Polygon Previous Day Close for Ticker (Deprecated)
#' 
#' @description Deprecated. See \code{\link[AlpacaforR]{polygon}}.
#' This function provides the Previous Day close data from Polygon for the ticker specified. 
#' @param ticker Specify which symbol you want to call by inserting ticker as a string.
#' @return A list object containing all information the API responds with. 
#' @examples
#' # Getting the last listed trade for AMZN: 
#' get_poly_prev_dayclose("AMZN")
#' @importFrom httr GET
#' @export
get_poly_prev_dayclose <- function(ticker=NULL){
  message(paste0("This function is deprecated, see ?AlpacaforR::polygon"))
  if(is.null(ticker)){
    stop("Please enter the stocks ticker.")
  }
  
  #Set URL
  path_url = get_url_poly()
  full_path_url = paste0(path_url,"/v2/aggs/ticker/",ticker,"/prev","?apiKey=",Sys.getenv("APCA-LIVE-API-KEY-ID"))
  
  #Send Request
  prev_close = httr::GET(url = full_path_url)
  prev_close = response_text_clean(prev_close)
  
  #Convert unix epoch timestamp to readable date
  prev_close$results$t = as.POSIXct(prev_close$results$t/1000, origin = "1970-01-01")
  return(prev_close)
}
#----------------------------------------------------------------------------------------------




# PACKAGE FUNCTIONS END #