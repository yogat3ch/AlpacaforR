#' @include internal.R
#' @include Polygon.R

`-` <- lubridate::`.__T__-:base`

context("Test all endpoints accessible from the polygon function")

test_that("Tickers is accessible and returns the appropriate data", {
  .resp <- polygon("Tickers", search = "Tesla", market = "stocks")
  expect_true(any(stringr::str_detect(.resp$ticker, "TSLA")))
  expect_s3_class(.resp$updated, "Date")
  expect_length(.resp, 10)
  expect_length(attr(.resp, "query"), 4)
})

test_that("Ticker Types is accessible and returns appropriate data", {
  .resp <- polygon("Ticker Types")
  expect_identical(class(.resp), "list")
  expect_identical(attr(.resp, "query"), list(status = "OK"))
})

test_that("Ticker Details is accessible and returns appropriate data", {
  .resp <- polygon("Ticker Details", symbol = "AMZN")
  expect_s3_class(.resp$listdate, "Date")
  expect_identical(.resp$sic, 5961L)
  expect_identical(.resp$exchangeSymbol, "NGS")
  expect_identical(.resp$symbol, "AMZN")
})

test_that("Ticker Details is accessible and returns appropriate data", {
  .resp <- polygon("Ticker News", symbol = "BYND")
  expect_s3_class(.resp$timestamp, "POSIXct")
  expect_length(.resp, 8)
})

test_that("Markets is accessible and returns appropriate data", {
  .resp <- polygon("Markets")
  expect_true(any(stringr::str_detect(.resp$market, "STOCKS")))
  expect_length(.resp, 2)
})

test_that("Locales is accessible and returns appropriate data", {
  .resp <- polygon("Locales")
  expect_identical(sum(.resp$locale %in% c("G","US")), 2L)
  expect_length(.resp, 2)
})

test_that("Stock Splits is accessible and returns appropriate data", {
  .resp <- polygon("Stock Splits", symbol = "AMD")
  expect_s3_class(.resp$exDate, "Date")
  expect_identical(.resp$ticker, "AMD")
  expect_length(.resp, 4)
  expect_identical(attr(.resp, "query"), list(status = "OK", count = 1L))
})

test_that("Stock Dividends is accessible and returns appropriate data", {
  .resp <- polygon("Stock Dividends", symbol = "MSFT")
  expect_s3_class(.resp$exDate, "Date")
  expect_identical(.resp$ticker[[1]], "MSFT")
  expect_length(.resp, 6)
  expect_identical(attr(.resp, "query")$status, "OK")
})

test_that("Stock Financials is accessible and returns appropriate data", {
  .resp <- polygon("Stock Financials", symbol = "BYND")
  expect_s3_class(.resp$calendarDate, "Date")
  expect_s3_class(.resp$reportPeriod, "Date")
  expect_s3_class(.resp$updated, "Date")
  expect_identical(.resp$ticker[[1]], "BYND")
  expect_length(.resp, 111)
})

test_that("Market Status is accessible and returns appropriate data", {
  .resp <- polygon("Market Status")
  expect_s3_class(.resp$serverTime, "POSIXct")
  expect_length(.resp, 4)
})

test_that("Market Holidays is accessible and returns appropriate data", {
  .resp <- polygon("Market Holidays")
  expect_s3_class(.resp$date, "Date")
  expect_s3_class(.resp$open, "POSIXct")
  expect_identical(unique(.resp$status), c("closed", "early-close"))
  expect_length(.resp, 6)
})

test_that("Exchanges is accessible and returns appropriate data", {
  .resp <- polygon("Exchanges")
  expect_true(any(stringr::str_detect(.resp$name, "NYSE America")))
  expect_length(.resp, 7)
})

test_that("Historic Trades is accessible and returns appropriate data", {
  .resp <- polygon("Historic Trades", limit = 5)
  expect_s3_class(.resp$time, "POSIXct")
  expect_true("time" %in% names(.resp))
  expect_length(.resp, 9)
  expect_identical(attr(.resp, "map"), list(
    c = list(name = "conditions", type = "[]int"),
    I = list(name = "orig_id", type = "string"),
    e = list(name = "correction",
             type = "int"),
    x = list(name = "exchange", type = "int"),
    r = list(name = "trf_id", type = "int"),
    s = list(name = "size",
             type = "int"),
    t = list(name = "sip_timestamp", type = "int64"),
    f = list(name = "trf_timestamp", type = "int64"),
    i = list(name = "id", type = "string"),
    p = list(name = "price",
             type = "float64"),
    z = list(name = "tape", type = "int"),
    y = list(name = "participant_timestamp", type = "int64"),
    q = list(name = "sequence_number", type = "int")
  ))
})

test_that("Historic Quotes is accessible and returns appropriate data", {
  .resp <- polygon("Historic Quotes", ticker = "MSFT", date = "2008-04-15", limit = 5)
  expect_identical(.resp$time, structure(c(1208246852.8, 1208246875.777, 1208246877.527, 1208247302.04, 1208247302.04), class = c("POSIXct", "POSIXt"), tzone = "America/New_York"))
  expect_identical(attr(.resp,"map"),list(
    s = list(name = "bid_size", type = "int"),
    x = list(name = "bid_exchange",
             type = "int"),
    P = list(name = "ask_price", type = "float64"),
    S = list(name = "ask_size", type = "int"),
    t = list(name = "sip_timestamp",
             type = "int64"),
    q = list(name = "sequence_number", type = "int"),
    c = list(name = "conditions", type = "[]int"),
    p = list(name = "bid_price",
             type = "float64"),
    X = list(name = "ask_exchange", type = "int"),
    z = list(name = "tape", type = "int"),
    y = list(name = "participant_timestamp",
             type = "int64"),
    f = list(name = "trf_timestamp", type = "int64"),
    i = list(name = "indicators", type = "[]int")
  ))
  expect_length(.resp, 10)
})

test_that("Last Trade is accessible and returns appropriate data", {
  .resp <- polygon("Last trade for a symbol", symbol = "BYND")
  expect_lt(lubridate::as.difftime(lubridate::now() - .resp$timestamp), 
                   lubridate::as.difftime(10, units = "days"))
  expect_identical(attr(.resp,"query"), list(status = "success", symbol = "BYND"))
  expect_equal(dim(.resp), c(1,4))
})

test_that("Last Quote is accessible and returns appropriate data", {
  .resp <- polygon("Last quote for a symbol", symbol = "BYND")
  expect_lt(lubridate::as.difftime(lubridate::now() - .resp$timestamp), 
                   lubridate::as.difftime(10, units = "days"))
  expect_identical(attr(.resp,"query"), list(status = "success", symbol = "BYND"))
  expect_equal(dim(.resp), c(1,7))
})

test_that("Daily Open/Close is accessible and returns appropriate data", {
  .resp <- polygon("Daily Open/Close", symbol = "BYND", date = "2019-12-04")
  expect_identical(.resp, structure(list(from = structure(1575430200, class = c("POSIXct", "POSIXt"), tzone = "America/New_York"), symbol = "BYND", open = 76.62, high = 77.2, low = 73.75, close = 74.41, afterHours = 73.757, volume = 4315965L), class = c("tbl_df", "tbl", "data.frame"), row.names = c(NA, -1L), query = list(status = "OK")))
})

test_that("Condition Mappings is accessible and returns appropriate data", {
  .resp <- polygon("Condition Mappings", ticktype = "trades")
  expect_equal(dim(.resp), c(55,1))
  .resp <- polygon("Condition Mappings", ticktype = "quotes")
  expect_equal(dim(.resp), c(45,1))
})

test_that("Snapshot: All Tickers is accessible and returns appropriate data", {
  .resp <- polygon("Snapshot: All Tickers")
  expect_identical(attr(.resp, "query")$status, "OK")
  if (polygon("Market Status")$market %in% c("open", "extended-hours")) {
    expect_s3_class(.resp, "tbl")
    expect_length(.resp, 33)
    expect_s3_class(.resp$updated, "POSIXct")
    expect_gt(nrow(.resp), 1)
  } else {
    # if the market is closed
  }
})

test_that("Snapshot: Single Ticker is accessible and returns appropriate data", {
  .resp <- polygon("Snapshot: Single Ticker", ticker = "BYND")
  expect_identical(attr(.resp, "query")$status, "OK")
  if (polygon("Market Status")$market %in% c("open", "extended-hours")) {
    expect_s3_class(.resp, "tbl")
    expect_length(.resp, 33)
    expect_identical(.resp$ticker, "BYND")
    expect_s3_class(.resp$updated, "POSIXct")
    expect_equal(nrow(.resp), 1)
  } else {
    # if the market is closed
  }
})

test_that("Snapshot: Gainers/Losers is accessible and returns appropriate data", {
  .resp <- polygon("Snapshot: Gainers/Losers", direction = "gainers")
  expect_identical(attr(.resp, "query")$status, "OK")
  if (polygon("Market Status")$market %in% c("open", "extended-hours")) {
    expect_s3_class(.resp, "tbl")
    expect_length(.resp, 33)
    expect_s3_class(.resp$lastQuote.t,  "POSIXct")
  } else {
  
  }
})

test_that("Previous Close is accessible and returns appropriate data", {
  .resp <- polygon("Previous Close", ticker = "BYND")
  expect_identical(attr(.resp, "query")$status, "OK")
  expect_identical(attr(.resp, "query")$ticker, "BYND")
  expect_s3_class(.resp, "tbl")
  expect_s3_class(.resp$time, "POSIXct")
  expect_equal(dim(.resp), c(1,8))
})


test_that("Grouped Daily (Bars) is accessible and returns appropriate data", {
  .resp <- polygon("Grouped Daily (Bars)", locale = "US", market = "STOCKS", date = "2020-04-16")
  expect_identical(attr(.resp, "query")$status, "OK")
  expect_s3_class(.resp, "tbl")
  expect_s3_class(.resp$time, "POSIXct")
  expect_length(.resp, 8)
})

