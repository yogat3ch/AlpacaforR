

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
           params = list(sort = c("ticker", "-ticker", "type"), type = list(NULL, "etp", "cs"), market = list(NULL, "stocks", "indices"), locale = list(NULL, "us", "g"), search = list(NULL, "microsoft"), perpage = 50, page = 1, active = list(NULL, TRUE, FALSE))),
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
            params = list(ticker = "AAPL", date = lubridate::as_date("2018-02-02"), timestamp = list(NULL, 1), timestampLimit = list(NULL,1), reverse = list(NULL, TRUE, FALSE), limit = c(10, 50000))),
  hq = list(nm = "Historic Quotes (NBBO)",
            desc = "Get historic NBBO quotes for a ticker.",
            href = "https://polygon.io/docs/#get_v2_ticks_stocks_nbbo__ticker___date__anchor",
            url = "/v2/ticks/stocks/nbbo/{ticker}/{date}",
            params = list(ticker = "AAPL", date = lubridate::as_date("2018-02-02"), timestamp = list(NULL, 1), timestampLimit = list(NULL,1), reverse = list(NULL, TRUE, FALSE), limit = c(10, 50000))),
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
            params = list(symbol = "AAPL", date = Sys.Date() - 1)),
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
            params = list(ticker = "AAPL", unadjusted = list(NULL, TRUE, FALSE))),
  a = list(nm = "Aggregates (Bars)",
           desc = "See ?market_data"),
  gd = list(nm = "Grouped Daily (Bars)",
            desc = "Get the daily OHLC for entire markets.",
            href = "https://polygon.io/docs/#get_v2_aggs_grouped_locale__locale__market__market___date__anchor",
            url = "/v2/aggs/grouped/locale/{locale}/market/{market}/{date}",
            params = list(locale = c("US", "See Locales Endpoint"), market = c("STOCKS", "See Markets Endpoint"), date = lubridate::as_date("2019-02-01"), unadjusted = list(NULL, TRUE, FALSE))),
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

#Generate help text
# purrr::imap_chr(.ep[1:24], ~{

#   if (.y == "a") {
#     return(paste0("#' \\item{`a`}{ Aggregates - see ?market-data}"))
#   }
# 
#   .out <- glue::glue("#' \\item{`{{.y}}`}{ [{{.x$nm}}]({{.x$href}}):\n", .open = "{{", .close = "}}")
#   .out <- paste(.out, .x$desc)
#   if (!is.null(.x$params)) {
#     if (stringr::str_detect(.x$url, '\\{')) {
#       .r <- stringr::str_match_all(.x$url, '\\{(\\w+)\\}')[[1]][,2]
#     }
#     .args <- purrr::imap(.x[["params"]], ~{
#       .c <- ifelse(is.list(.x), class(.x[[2]]), class(.x))
#       .r <- get0('.r')
#       if (.y %in% .r) {
#         .y <- paste0('\\strong{\\code{',.y,'}}')
#       } else {
#         .y<- paste0('\\code{"',.y,'"}')
#       }
#       .args <- paste0("#'   \\item{",.y,"}","{",glue::glue(' `({.c})` '), paste0(purrr::map_chr(.x, ~{ifelse(is.null(.x), '*Optional* NULL', as.character(.x))}), collapse = ', '), "}")}) %>%
#       do.call(c,.) %>%
#       paste0(collapse = "\n") %>%
#       {paste0("#'   \\itemize{\n",.,"\n#'  }")} %>%
#       paste0("\n#' Arguments:\n", .)
# 
# 
#   } else  {
#     .args <- NULL
#   }
#   paste0(.out, .args, "\n#' }")
# }) %>% {paste0("#' \\itemize{\n",do.call(paste0, list(., collapse = "\n")),"\n#' }")} %>% clipr::write_clip()
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
#' \item{`t`}{ [Tickers](https://polygon.io/docs/#get_v2_reference_tickers_anchor): Query all ticker symbols which are supported by Polygon.io. This API includes Indices, Crypto, FX, and Stocks/Equities.
#' Arguments:
#'   \itemize{
#'   \item{\code{"sort"}}{ `(character)` ticker, -ticker, type}
#'   \item{\code{"type"}}{ `(character)` *Optional* NULL, etp, cs}
#'   \item{\code{"market"}}{ `(character)` *Optional* NULL, stocks, indices}
#'   \item{\code{"locale"}}{ `(character)` *Optional* NULL, us, g}
#'   \item{\code{"search"}}{ `(character)` *Optional* NULL, microsoft}
#'   \item{\code{"perpage"}}{ `(numeric)` 50}
#'   \item{\code{"page"}}{ `(numeric)` 1}
#'   \item{\code{"active"}}{ `(logical)` *Optional* NULL, TRUE, FALSE}
#'  }
#' }
#' \item{`tt`}{ [Ticker Types](https://polygon.io/docs/#get_v2_reference_types_anchor): Get the mapping of ticker types to descriptions / long names
#' }
#' \item{`td`}{ [Ticker Details](https://polygon.io/docs/#get_v1_meta_symbols__symbol__company_anchor): Get the details of the symbol company/entity. These are important details which offer an overview of the entity. Things like name, sector, description, logo and similar companies.
#' Arguments:
#'   \itemize{
#'   \item{\strong{\code{symbol}}}{ `(character)` AAPL}
#'  }
#' }
#' \item{`tn`}{ [Ticker News](https://polygon.io/docs/#get_v1_meta_symbols__symbol__news_anchor): Get news articles for this ticker.
#' Arguments:
#'   \itemize{
#'   \item{\strong{\code{symbol}}}{ `(character)` AAPL}
#'   \item{\code{"perpage"}}{ `(numeric)` 50}
#'   \item{\code{"page"}}{ `(numeric)` 1}
#'  }
#' }
#' \item{`m`}{ [Markets](https://polygon.io/docs/#get_v2_reference_markets_anchor): Get the list of currently supported markets
#' }
#' \item{`l`}{ [Locales](https://polygon.io/docs/#get_v2_reference_locales_anchor): Get the list of currently supported locales
#' }
#' \item{`ss`}{ [Stock Splits](https://polygon.io/docs/#get_v2_reference_splits__symbol__anchor): Get the historical splits for this symbol.
#' Arguments:
#'   \itemize{
#'   \item{\strong{\code{symbol}}}{ `(character)` AAPL}
#'  }
#' }
#' \item{`sd`}{ [Stock Dividends](https://polygon.io/docs/#get_v2_reference_dividends__symbol__anchor): Get the historical dividends for this ticker.
#' Arguments:
#'   \itemize{
#'   \item{\strong{\code{symbol}}}{ `(character)` AAPL}
#'  }
#' }
#' \item{`sf`}{ [Stock Financials](https://polygon.io/docs/#get_v2_reference_financials__symbol__anchor): Current status of each market
#' Arguments:
#'   \itemize{
#'   \item{\strong{\code{symbol}}}{ `(character)` AAPL}
#'   \item{\code{"limit"}}{ `(numeric)` 5}
#'   \item{\code{"type"}}{ `(character)` *Optional* NULL, Y, YA, Q, QA, T, TA}
#'   \item{\code{"sort"}}{ `(character)` *Optional* NULL, reportPeriod, -reportPeriod, calendarDate, -calendarDate}
#'  }
#' }
#' \item{`ms`}{ [Market Status](https://polygon.io/docs/#get_v1_marketstatus_now_anchor): Current status of each market
#' }
#' \item{`mh`}{ [Market Holidays](https://polygon.io/docs/#get_v1_marketstatus_upcoming_anchor): Get upcoming market holidays and their open/close times
#' }
#' \item{`e`}{ [Exchanges](https://polygon.io/docs/#get_v1_meta_exchanges_anchor): List of stock exchanges which are supported by Polygon.io
#' }
#' \item{`ht`}{ [Historic Trades](https://polygon.io/docs/#get_v2_ticks_stocks_trades__ticker___date__anchor): Get historic NBBO quotes for a ticker.
#' Arguments:
#'   \itemize{
#'   \item{\strong{\code{ticker}}}{ `(character)` AAPL}
#'   \item{\strong{\code{date}}}{ `(Date)` 2018-02-02}
#'   \item{\code{"timestamp"}}{ `(numeric)` *Optional* NULL, 1}
#'   \item{\code{"timestampLimit"}}{ `(numeric)` *Optional* NULL, 1}
#'   \item{\code{"reverse"}}{ `(logical)` *Optional* NULL, TRUE, FALSE}
#'   \item{\code{"limit"}}{ `(numeric)` 10, 50000}
#'  }
#' }
#' \item{`hq`}{ [Historic Quotes (NBBO)](https://polygon.io/docs/#get_v2_ticks_stocks_nbbo__ticker___date__anchor): Get historic NBBO quotes for a ticker.
#' Arguments:
#'   \itemize{
#'   \item{\strong{\code{ticker}}}{ `(character)` AAPL}
#'   \item{\strong{\code{date}}}{ `(Date)` 2018-02-02}
#'   \item{\code{"timestamp"}}{ `(numeric)` *Optional* NULL, 1}
#'   \item{\code{"timestampLimit"}}{ `(numeric)` *Optional* NULL, 1}
#'   \item{\code{"reverse"}}{ `(logical)` *Optional* NULL, TRUE, FALSE}
#'   \item{\code{"limit"}}{ `(numeric)` 10, 50000}
#'  }
#' }
#' \item{`lt`}{ [Last Trade for a Symbol](https://polygon.io/docs/#get_v1_last_stocks__symbol__anchor): Get the last trade for a given stock.
#' Arguments:
#'   \itemize{
#'   \item{\strong{\code{symbol}}}{ `(character)` AAPL}
#'  }
#' }
#' \item{`lq`}{ [Last Quote for a Symbol](https://polygon.io/docs/#get_v1_last_quote_stocks__symbol__anchor): Get the last quote tick for a given stock.
#' Arguments:
#'   \itemize{
#'   \item{\strong{\code{symbol}}}{ `(character)` AAPL}
#'  }
#' }
#' \item{`do`}{ [Daily Open/Close](https://polygon.io/docs/#get_v1_open-close__symbol___date__anchor): Get the open, close and afterhours prices of a symbol on a certain date.
#' Arguments:
#'   \itemize{
#'   \item{\strong{\code{symbol}}}{ `(character)` AAPL}
#'   \item{\strong{\code{date}}}{ `(Date)` 2020-05-04}
#'  }
#' }
#' \item{`cm`}{ [Condition Mappings](https://polygon.io/docs/#get_v1_meta_conditions__ticktype__anchor): The mappings for conditions on trades and quotes.
#' Arguments:
#'   \itemize{
#'   \item{\strong{\code{ticktype}}}{ `(character)` trades, quotes}
#'  }
#' }
#' \item{`sa`}{ [Snapshot - All tickers](https://polygon.io/docs/#get_v2_snapshot_locale_us_markets_stocks_tickers_anchor): Snapshot allows you to see all tickers current minute aggregate, daily aggregate and last trade. As well as previous days aggregate and calculated change for today.
#' }
#' \item{`st`}{ [Snapshot - Single Ticker](https://polygon.io/docs/#get_v2_snapshot_locale_us_markets_stocks_tickers__ticker__anchor): See the current snapshot of a single ticker
#' Arguments:
#'   \itemize{
#'   \item{\strong{\code{ticker}}}{ `(character)` AAPL}
#'  }
#' }
#' \item{`sg`}{ [Snapshot - Gainers/Losers](https://polygon.io/docs/#get_v2_snapshot_locale_us_markets_stocks__direction__anchor): See the current snapshot of the top 20 gainers or losers of the day at the moment.
#' Arguments:
#'   \itemize{
#'   \item{\strong{\code{direction}}}{ `(character)` gainers, losers}
#'  }
#' }
#' \item{`pc`}{ [Previous Close](https://polygon.io/docs/#get_v2_aggs_ticker__ticker__prev_anchor): Get the previous day close for the specified ticker
#' Arguments:
#'   \itemize{
#'   \item{\strong{\code{ticker}}}{ `(character)` AAPL}
#'   \item{\code{"unadjusted"}}{ `(logical)` *Optional* NULL, TRUE, FALSE}
#'  }
#' }
#' \item{`a`}{ Aggregates - see \link[AlpacaforR]{market_data}}
#' \item{`gd`}{ [Grouped Daily (Bars)](https://polygon.io/docs/#get_v2_aggs_grouped_locale__locale__market__market___date__anchor): Get the daily OHLC for entire markets.
#' Arguments:
#'   \itemize{
#'   \item{\strong{\code{locale}}}{ `(character)` US, See Locales Endpoint}
#'   \item{\strong{\code{market}}}{ `(character)` STOCKS, See Markets Endpoint}
#'   \item{\strong{\code{date}}}{ `(Date)` 2019-02-01}
#'   \item{\code{"unadjusted"}}{ `(logical)` *Optional* NULL, TRUE, FALSE}
#'  }
#' }
#' }
#' @param ... Named arguments specific to the endpoint parameters. These can also be specified in a list via the `params` argument.
#' @param params `(list)` A named list of parameters specific to the endpoint.
#' @return Response `(tibble/list/data.frame)` depending on the endpoint. The core data of the response will be returned as the object. If query data is returned in addition to the core object, it is stored as a `"query"` attribute and accessed via `attr(object, "query")`. If a map object is returned with the response, it will be stored as a `"map"` attribute and accessed via `attr(object, "map")`.
#' @details This function is not vectorized. Only a single endpoint may be called at a time. Thus any endpoints with path parameters (parameters denoted in bold) require that a single combination of path parameters be passed for a given call.
#' @export

polygon <- function(ep = NULL, ..., params = NULL) {
  if (is.null(ep) || !is.character(ep)) rlang::abort("Endpoint (char) required. See ?AlpacaforR::polygon for details.")
  #TODO If pkg name changes, change this.
  # Is the user requesting the reference object?
  .ref <- grepl("\\+", ep)
  # get endpoint object
  e_p <- ep_parse(ep, .ref)
  if (.ref) return(e_p)
  
  params <- append(list(...), params)
  # add defaults for parameters not specified
  params <- append(params, purrr::map(e_p$params[!names(e_p$params) %in% names(params)], 1))
  
  # Check date
  if (any(grepl("date", names(params)))) {
    params$date <- try_date(params[["date"]])
  }
  # Check other params
  if (!rlang::is_empty(e_p$params)) {
    if (isFALSE(all(names(params) %in% names(e_p$params)))) {
      # if a parameter is wrongly named, first fix common confusion ticker/symbol
      .wn <- setdiff(names(params), names(e_p$params))
      if (grepl("symbol|ticker", .wn)) {
        message(paste0("Argument is called `", ifelse(.wn == "symbol", "ticker", "symbol"), "` rather than `", .wn, "`"))
        params[ifelse(.wn == "symbol", "ticker", "symbol")] <- params[.wn]
        params[.wn] <- NULL
      }
    }
    if (any(c("ticker", "symbol") %in% names(params))) {
      # capitalize
      .pos <- grep("ticker|symbol", names(params))
      params[[.pos]] <- toupper(params[[.pos]])
    }
    # Check argument classes
    .classes <- purrr::map(purrr::map_depth(e_p$params, 2, class), ~{
      .classes <- unique(unlist(.x))
      if ("Date" %in% .classes) {
        .classes <- c(.classes, "POSIXct")
      }
      return(.classes)
    })
    purrr::pmap(list(params, .classes[names(params)], names(params)), ~{
      if (isFALSE(inherits(.x, .y))) rlang::abort(glue::glue("Argument {..3} is of type {class(..3)}, when {.y} is expected."))
    })
  }
  # assign the appropriate path using params
  if (stringr::str_detect(e_p$url, "\\{")) {
    .path <- glue::glue_data(params, e_p$url)
    # Remove parameters used in path
    params <- params[!names(params) %in% stringr::str_match_all(e_p$url, "\\{(\\w+)\\}")[[1]][,2]]
  } else {
    .path <- e_p$url
  }
  
  .url <- get_url(path = .path,
          query = rlang::list2(!!!params, apiKey = Sys.getenv("APCA-LIVE-API-KEY-ID")),
          data = TRUE,
          v = 2)
  
  
  .resp <- httr::GET(url = .url)
  out <- poly_transform(.resp, e_p)
  return(out)
}

ep_parse <- function(x, .ref) {
  
  ep <- gsub("[^[:alnum:]\\s]+", "", gsub("\\+","", x), perl = TRUE)
  ep <- strsplit(tolower(ep), "\\s")[[1]]
  .lep <- length(ep)
  if (any(nchar(ep) > 2) || .lep > 1) {
    
    ep <- ep[nzchar(ep)]
    ep <- substr(ep, 0, ifelse(.lep > 1, 1, 3))
    # If input is length one and the three letters are one of the reference keywords
    if (isTRUE(ep %in% c("all", "ref", "sto"))) {
      # If details are requested return the objects
      out <- .ep[switch(ep, 
                        all = c(1:24), ref = c(1:11), sto = c(12:24))]
      # Otherwise just the names
      if (!.ref) out <- data.frame(name = purrr::map_chr(out, "nm"))
    } else {
      # Otherwise reduce it to a single letter
      ep <- substr(ep, 0, 1)
    }
  }
  
  ep <- ifelse(.lep > 1, paste0(na.omit(ep[1:2]), collapse = ""), ep)
  # if single ticker which would potentially be mislabelled
  ep <- ifelse(grepl("Single Ticker$", x, ignore.case = TRUE), "st", ep)
  
  
  
  if (ep %in% names(.ep)) {
    out <- .ep[[ep]]
  } else if (ep %in% c("a", "ab")) {
    rlang::abort("See ?market_data for aggregates endpoint.")
  } else if (!ep %in% c("all", "ref", "sto")) {
    rlang::abort("No matching endpoint for `ep` argument. See ?polygon")
  }
  return(out)
}


# poly_transform ----
# Sun May 03 08:54:26 2020
#'
#' @title transform polygon.io reference endpoints
#' @keywords internal 
#' @description coerce data objects returned from the various polygon.io endpoints to R compliant objects
#' @param resp \code{\link[httr]{response}} from `{httr}`
#' @param e_p \code{(list)} The endpoint description
#' @return \code{list/data.frame/tibble} Either a list or tibble depending on the endpoint with a query attribute.

poly_transform <- function(resp, e_p) {
  # check for errors
  ep <- names(.ep)[which(purrr::map_chr(.ep, 1) %in% e_p$nm)]
  .resp <- response_text_clean(resp)
  if(any(grepl("^4", resp$status_code))) {
    rlang::abort(paste(.ep[[ep]]$nm, "endpoint error.\n Message:", .resp))
  }
  
  .s <- rlang::expr({
    .t <- grep("ticker", names(.resp))
    if (rlang::is_empty(.resp[[.t]])) {
      rlang::warn(e_p$nm, " returns no data when market is closed.")
      .resp
    } else {
      suppressWarnings(dplyr::bind_cols(.resp[[.t]][-c(1:5)], purrr::imap_dfc(.resp[[.t]][1:5], ~{
        setNames(.x, paste0(.y,".",names(.x)))
      })))
    }
  })
  .tf <- "minute"
  .o <- switch(ep,
               t = list(.tbl = "tickers" , .vars = c("updated"), timeframe = .tf),
               tt = ,
               m = ,
               l = list(.tbl = "results"),
               td = list(.tbl = .resp, .vars = c("updated", "listdate"), timeframe = .tf),
               tn = list(.tbl = .resp, .vars = c("timestamp"), timeframe = .tf),
               sd = ,
               ss = list(.tbl = "results", .vars = rlang::expr(tidyselect::ends_with("Date"))),
               sf = list(.tbl = "results", .vars = c('calendarDate', 'reportPeriod', 'updated', 'dateKey')),
               ms = list(.tbl = .resp, .vars = "serverTime", timeframe = .tf),
               mh = list(.tbl = .resp, .vars = c("date")),
               e = ,
               cm = list(.tbl = .resp),
               ht = ,
               hq = list(.tbl = "results", .vars = "t", timeframe = .tf),
               lt = ,
               lq = list(.tbl = "last", .vars = "timestamp", timeframe = .tf),
               do = list(.tbl = .resp[-1], .vars = "from", timeframe = .tf),
               sa = ,
               st = ,
               sg = list(.tbl = eval(.s),
                         .vars = c("lastQuote.t", "lastTrade.t", "updated")
                         , timeframe = .tf),
               pc = ,
               gd = list(.tbl = rlang::expr(dplyr::rename(.resp, time = 't', volume = "v", open = "o", high = "h", low = "l", close = "c", ticker = "T")), .vars = "time", timeframe = .tf)
  )
  
  if (rlang::is_empty(.o$.tbl)) {
    rlang::warn(paste0("Query returned no results. If metadata exists it will be returned"))
  } else {
    out <- rlang::exec(poly_parse, !!!.o, .resp = .resp, ep = ep)
  }
  return(out)
}


poly_parse <- function(.tbl, .vars, .f = try_date, ..., .resp, ep) {
  if (is.character(.tbl)) 
    .q <- append(attr(.resp, "query"), .resp[!names(.resp) %in% .tbl])
  else
    .q <- attr(.resp, "query")
  #browser(expr = ep %in% c("st", "sa", "sg"))
  
  if (!missing(.vars)) {
    .args <- rlang::list2(
      .data = tbl_parse(.tbl, .resp),
      rlang::expr(dplyr::across(!!.vars, .f = .f, !!!rlang::dots_list(...))) 
    )
    out <- rlang::eval_bare(rlang::call2(dplyr::mutate, !!!.args))
  } else if (is.character(.tbl)) {
    out <- .resp[[.tbl]]
  } else {
    out <- .tbl
  }
  
  
  
  attr(out, "query") <- .q
  out
}
tbl_parse <- function(.tbl, .resp) {
  UseMethod("tbl_parse")
}


tbl_parse.list <- function(.tbl, .resp) {
  out <- tibble::as_tibble(purrr::modify_if(.tbl, ~length(.x) != 1, list))
}

tbl_parse.default <- function(.tbl, .resp) {
  out <- eval(.tbl)
}

tbl_parse.character <- function(.tbl, .resp) {
  out <- .resp[[.tbl]]
  if (is.list(out)) out <- tbl_parse(out)
}


