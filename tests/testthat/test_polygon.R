#' @include internal.R
#' @include Polygon.R

# CHANGED 2021-03-19T17:43:02 Commented out endpoints no long accessible to Alpaca users

vcr::vcr_configure(dir = file.path(dirname(.log_path), "polygon"))
vcr::use_cassette("Tickers_is_accessible_and_returns_the_appropriate_data", match_requests_on = c("path"), {
test_that("Tickers is accessible and returns the appropriate data", {
  .resp <- polygon("Tickers", search = "Tesla", market = "stocks")
  expect_true(any(stringr::str_detect(.resp$ticker, "TSLA")))
  expect_s3_class(.resp$updated, "POSIXct")
  expect_length(.resp, 10)
  expect_length(attr(.resp, "query"), 7)
})
})

vcr::use_cassette("Ticker_Types_is_accessible_and_returns_appropriate_data", match_requests_on = c("path"), {
test_that("Ticker Types is accessible and returns appropriate data", {
  .resp <- polygon("Ticker Types")
  expect_type(.resp, "list")
  expect_type(attr(.resp, "query"), "list")
})
})

vcr::use_cassette("Ticker_Details_is_accessible_and_returns_appropriate_data", match_requests_on = c("path"), {
test_that("Ticker Details is accessible and returns appropriate data", {
  .resp <- polygon("Ticker Details", symbol = "AMZN")
  expect_s3_class(do.call(c, .resp[, c("listdate","updated")]), "POSIXct")
  expect_identical(.resp$sic, 5961L)
  expect_identical(.resp$exchangeSymbol, "NGS")
  expect_identical(.resp$symbol, "AMZN")
})
})

vcr::use_cassette("Ticker_News_is_accessible_and_returns_appropriate_data", match_requests_on = c("path"), serialize_with = "json", {
test_that("Ticker News is accessible and returns appropriate data", {
  .resp <- polygon("Ticker News", symbol = "AAPL")
  expect_s3_class(.resp$timestamp, "POSIXct")
  expect_length(.resp, 8)
})
})

vcr::use_cassette("Markets_is_accessible_and_returns_appropriate_data", match_requests_on = c("path"), {
test_that("Markets is accessible and returns appropriate data", {
  .resp <- polygon("Markets")
  expect_true(any(stringr::str_detect(.resp$market, "STOCKS")))
  expect_length(.resp, 2)
})
})

vcr::use_cassette("Locales_is_accessible_and_returns_appropriate_data", match_requests_on = c("path"), {
test_that("Locales is accessible and returns appropriate data", {
  .resp <- polygon("Locales")
  expect_identical(sum(.resp$locale %in% c("G","US")), 2L)
  expect_length(.resp, 2)
})
})

vcr::use_cassette("Stock_Splits_is_accessible_and_returns_appropriate_data", match_requests_on = c("path"), {
test_that("Stock Splits is accessible and returns appropriate data", {
  .resp <- polygon("Stock Splits", symbol = "AMD")
  expect_s3_class(.resp$exDate, "Date")
  expect_identical(.resp$ticker, "AMD")
  expect_length(.resp, 4)
  expect_type(attr(.resp, "query"), "list")
})
})


vcr::use_cassette("Stock_Dividends_is_accessible_and_returns_appropriate_data", match_requests_on = c("path"), {
test_that("Stock Dividends is accessible and returns appropriate data", {
  .resp <- polygon("Stock Dividends", symbol = "MSFT")
  expect_s3_class(.resp$exDate, "Date")
  expect_identical(.resp$ticker[[1]], "MSFT")
  expect_length(.resp, 6)
  expect_identical(attr(.resp, "query")$status, "OK")
})
})

vcr::use_cassette("Stock_Financials_is_accessible_and_returns_appropriate_data", match_requests_on = c("path"), {
test_that("Stock Financials is accessible and returns appropriate data", {
  .resp <- polygon("Stock Financials", symbol = "BYND")
  expect_s3_class(.resp$calendarDate, "Date")
  expect_s3_class(.resp$reportPeriod, "Date")
  expect_s3_class(.resp$updated, "Date")
  expect_identical(.resp$ticker[[1]], "BYND")
  expect_length(.resp, 111)
})
})

vcr::use_cassette("Market_Status_is_accessible_and_returns_appropriate_data", match_requests_on = c("path"), {
test_that("Market Status is accessible and returns appropriate data", {
  .resp <- polygon("Market Status")
  expect_s3_class(.resp$serverTime, "POSIXct")
  expect_length(.resp, 4)
  .ms_open <<- .resp$market
})
})

vcr::use_cassette("Market_Holidays_is_accessible_and_returns_appropriate_data", match_requests_on = c("path"), {
test_that("Market Holidays is accessible and returns appropriate data", {
  .resp <- polygon("Market Holidays")
  expect_s3_class(.resp$date, "Date")
  expect_identical(unique(.resp$status), c("closed", "early-close"))
  expect_length(.resp, 6)
})
})

vcr::use_cassette("Exchanges_is_accessible_and_returns_appropriate_data", match_requests_on = c("path"), {
test_that("Exchanges is accessible and returns appropriate data", {
  .resp <- polygon("Exchanges")
  expect_true(any(stringr::str_detect(.resp$name, "NYSE America")))
  expect_length(.resp, 7)
})
})

# vcr::use_cassette("Historic_Trades_is_accessible_and_returns_appropriate_data", match_requests_on = c("path"), {
# test_that("Historic Trades is accessible and returns appropriate data", {
#   .resp <- polygon("Historic Trades", limit = 5)
#   expect_s3_class(.resp$t, "POSIXct")
#   expect_length(.resp, 9)
#   .exp <- list(
#     c = list(name = "conditions", type = "[]int"),
#     I = list(name = "orig_id", type = "string"),
#     e = list(name = "correction",
#              type = "int"),
#     x = list(name = "exchange", type = "int"),
#     r = list(name = "trf_id", type = "int"),
#     s = list(name = "size",
#              type = "int"),
#     t = list(name = "sip_timestamp", type = "int64"),
#     f = list(name = "trf_timestamp", type = "int64"),
#     i = list(name = "id", type = "string"),
#     p = list(name = "price",
#              type = "float64"),
#     z = list(name = "tape", type = "int"),
#     y = list(name = "participant_timestamp", type = "int64"),
#     q = list(name = "sequence_number", type = "int")
#   ) %>% {.[sort(names(.))]}
#   expect_identical(attr(.resp, "query")$map %>% {.[sort(names(.))]}, .exp)
# })
# })

# vcr::use_cassette("Historic_Quotes_is_accessible_and_returns_appropriate_data", match_requests_on = c("path"), {
# test_that("Historic Quotes is accessible and returns appropriate data", {
#   .resp <- polygon("Historic Quotes", ticker = "MSFT", date = "2008-04-15", limit = 5)
#   expect_equal(.resp$t, structure(c(1208246852.8, 1208246875.777, 1208246877.527, 1208247302.04, 1208247302.04), class = c("POSIXct", "POSIXt")), tolerance = 1)
#   expect_identical(attr(.resp,"query")$map %>% {.[sort(names(.))]},list(
#     s = list(name = "bid_size", type = "int"),
#     x = list(name = "bid_exchange",
#              type = "int"),
#     P = list(name = "ask_price", type = "float64"),
#     S = list(name = "ask_size", type = "int"),
#     t = list(name = "sip_timestamp",
#              type = "int64"),
#     q = list(name = "sequence_number", type = "int"),
#     c = list(name = "conditions", type = "[]int"),
#     p = list(name = "bid_price",
#              type = "float64"),
#     X = list(name = "ask_exchange", type = "int"),
#     z = list(name = "tape", type = "int"),
#     y = list(name = "participant_timestamp",
#              type = "int64"),
#     f = list(name = "trf_timestamp", type = "int64"),
#     i = list(name = "indicators", type = "[]int")
#   )%>% {.[sort(names(.))]})
#   expect_length(.resp, 10)
# })
# })

# vcr::use_cassette("Last_Trade_is_accessible_and_returns_appropriate_data", match_requests_on = c("path"), {
# test_that("Last Trade is accessible and returns appropriate data", {
#   .resp <- polygon("Last trade for a symbol", symbol = "BYND")
#   expect_type(attr(.resp,"query"), "list")
# })
# })

# vcr::use_cassette("Last_Quote_is_accessible_and_returns_appropriate_data", match_requests_on = c("path"), {
# test_that("Last Quote is accessible and returns appropriate data", {
#   .resp <- polygon("Last quote for a symbol", symbol = "BYND")
#   expect_type(attr(.resp,"query"), "list")
#   expect_equal(dim(.resp), c(1,7))
# })
# })

vcr::use_cassette("Daily_Open_Close_is_accessible_and_returns_appropriate_data", match_requests_on = c("path"), {
test_that("Daily Open/Close is accessible and returns appropriate data", {
  .resp <- polygon("Daily Open/Close", symbol = "BYND", date = "2019-12-04")
  expect_equal(.resp, structure(
    list(
      from = structure(
        1575435600,
        class = c("POSIXct",
                  "POSIXt"),
        tzone = "America/New_York"
      ),
      symbol = "BYND",
      open = 76.75,
      high = 77.05,
      low = 73.51,
      close = 73.89,
      volume = 5168416L,
      afterHours = 73.85,
      preMarket = 76.92
    ),
    class = c("tbl_df",
              "tbl", "data.frame"),
    row.names = c(NA,-1L),
    query = list(status = "OK")
  ), ignore_attr = TRUE)
})
})

vcr::use_cassette("Condition_Mappings_is_accessible_and_returns_appropriate_data", match_requests_on = c("path"), {
test_that("Condition Mappings is accessible and returns appropriate data", {
  .resp <- polygon("Condition Mappings", ticktype = "trades")
  expect_length(.resp, 55)
  .resp <- polygon("Condition Mappings", ticktype = "quotes")
  expect_length(.resp, 45)
})
})

# vcr::use_cassette("Snapshot_All_Tickers_is_accessible_and_returns_appropriate_data", match_requests_on = c("path"), {
# test_that("Snapshot: All Tickers is accessible and returns appropriate data", {
#   if (.ms_open) {
#     .resp <- polygon("Snapshot: All Tickers")
#     expect_s3_class(.resp, "data.frame")
#     expect_s3_class(.resp$updated, "POSIXct")
#     expect_gt(nrow(.resp), 1)
#   } else {
#     # if it's a non market day
#     expect_warning(.resp <- polygon("Snapshot: All Tickers"), regexp = "(?:Query returned no results)|(?:returns no data when market is closed)")
#   }
#   expect_identical(attr(.resp, "query")$status_code, 200L)
# })
# })

# vcr::use_cassette("Snapshot_Single_Ticker_is_accessible_and_returns_appropriate_data", match_requests_on = c("path"), {
# test_that("Snapshot: Single Ticker is accessible and returns appropriate data", {
#   
#   
#   if (.ms_open) {
#     .resp <- polygon("Snapshot: Single Ticker", ticker = "BYND")
#     expect_identical(attr(.resp, "query")$status_code, 200L)
#     expect_s3_class(.resp, "tbl")
#     expect_identical(unique(.resp$ticker), "BYND")
#     expect_s3_class(.resp$updated, "POSIXct")
#     expect_equal(nrow(.resp), 1, tolerance = 1.1)
#   } else {
#     # if not a day the market was open
#     expect_warning({.resp <- polygon("Snapshot: Single Ticker", ticker = "BYND")}, regexp = "NotFound")
#   }
# })
# })

# vcr::use_cassette("Snapshot_Gainers_Losers_is_accessible_and_returns_appropriate_data", match_requests_on = c("path"), {
# test_that("Snapshot: Gainers/Losers is accessible and returns appropriate data", {
#   if (.ms_open) {
#     .resp <- polygon("Snapshot: Gainers/Losers", direction = "gainers")
#     expect_s3_class(.resp, "data.frame")
#     expect_s3_class(.resp$lastQuote.t,  "POSIXct")
#   } else {
#     expect_warning(.resp <- polygon("Snapshot: Gainers/Losers"), regexp = "(?:Query returned no results)|(?:returns no data when market is closed)")
#   }
#   expect_identical(attr(.resp, "query")$status_code, 200L)
# })
# })

vcr::use_cassette("Previous_Close_is_accessible_and_returns_appropriate_data", match_requests_on = c("path"), {
test_that("Previous Close is accessible and returns appropriate data", {
  .resp <- polygon("Previous Close", ticker = "BYND")
  expect_identical(attr(.resp, "query")$status, "OK")
  expect_identical(attr(.resp, "query")$ticker, "BYND")
  expect_s3_class(.resp, "data.frame")
  expect_s3_class(.resp$time, "POSIXct")
  expect_equal(dim(.resp), c(1,8))
})
})


vcr::use_cassette("Grouped_Daily_Bars_is_accessible_and_returns_appropriate_data", match_requests_on = c("path"), {
test_that("Grouped Daily (Bars) is accessible and returns appropriate data", {
  .resp <- polygon("Grouped Daily (Bars)", locale = "US", market = "STOCKS", date = "2020-04-16")
  expect_identical(attr(.resp, "query")$status, "OK")
  expect_s3_class(.resp, "data.frame")
  expect_s3_class(.resp$time, "POSIXct")
})
})

