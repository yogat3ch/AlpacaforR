#' @include internal.R
#' @include Market-Data.R
library(dplyr)


# market_data ----
# Wed Apr 15 16:27:58 2020
.p <- "rds/market_data.rds"
.p <- ifelse(basename(getwd()) != "testthat", paste0("tests/testthat/",.p), .p)
bars_test <- readRDS(.p)

context("Tests the AlpacaforR exported functions")

vcr::use_cassette("market_data_works_when_v_2_and_full_T", match_requests_on = "method", {
test_that("market_data works when v = 2 and full = T", {
  expect_warning({bars <- market_data(ticker = c("BYND"), v = 2, from = "2020-01-05", until = "2020-04-05", multiplier = 1, timeframe = "h", full = T)}, "(?:`from` is a Sunday)|(?:`until` is a Sunday)", all = T)
  expect_equal(bars %>% purrr::map(~{attr(.x, "query") <- NULL}),
               bars_test %>% purrr::map(~{attr(.x, "query") <- NULL}))
})
})

vcr::use_cassette("market_data_works_with_last_quote", match_requests_on = "method", {
  test_that("market_data_works_with_last_quote", {
    .q <- market_data(c("TWTR", "BYND", "AAPL"), timeframe = "q")
    expect_equal(nrow(.q), 3)
    expect_equal(length(attr(.q, "query")), 3)
    expect_identical(.q$symbol, c("TWTR", "BYND", "AAPL"))
  })
})

vcr::use_cassette("market_data_works_with_last_trade", match_requests_on = "method", {
  test_that("market_data_works_with_last_trade", {
    .t <- market_data(c("TWTR", "BYND", "AAPL"), timeframe = "t")
    expect_equal(nrow(.t), 3)
    expect_equal(length(attr(.t, "query")), 3)
    expect_identical(.t$symbol, c("TWTR", "BYND", "AAPL"))
  })
})

vcr::use_cassette("market_data_errors_when_incompatible_arguments_are_requested", {
test_that("market_data_errors_when_incompatible_arguments_are_requested", {
  expect_error(expect_warning(market_data(ticker = "BYND", v = 1, multiplier = 30, timeframe = "m"), regexp = "(?:All formats)|(?:`to` was parsed to NA)", all = T), regexp = "1,5,15")
  expect_error(expect_warning(expect_message(market_data(ticker = "BYND", v = 2, multiplier = 1, timeframe = "d", to = "2015/13/1"), regexp = "(?:`from` argument omitted)|(?:Floor/Ceiling)|(?:'from' coerced to)|(?:'to' coerced to NA)"), regexp = "(?:All formats failed to parse.)|(?:`to` was parse to NA)", all = T), regexp = "Check the following argument\\(s\\) format: `to`")
})
})

vcr::use_cassette("market_data_warning_and_messages_are_informative", match_requests_on = c("method"), {
test_that("market_data_warning_and_messages_are_informative", {
  expect_warning(expect_message(market_data(ticker = "BYND", v = 1, multiplier = 7, timeframe = "d"), regexp = "(?:`to` argument omitted)|(?:`from` argument omitted)"), regexp = "The v1 API only accepts 1 as a `multiplier` for the day timeframe. One day bars will be returned.")
})
})


