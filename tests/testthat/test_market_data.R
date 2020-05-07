#' @include internal.R
#' @include Market-Data.R
library(dplyr)


# market_data ----
# Wed Apr 15 16:27:58 2020
.p <- "rds/market_data.rds"
.p <- ifelse(basename(getwd()) != "testthat", paste0("tests/testthat/",.p), .p)
bars_test <- readRDS(.p)

context("Tests the AlpacaforR exported functions")
test_that("market_data works when v = 2 and full = T", {
  bars <- market_data(ticker = c("BYND"), v = 2, from = "2020-01-05", until = "2020-04-05", multiplier = 1, timeframe = "h", full = T)
  expect_equal(bars %>% purrr::map(~{attr(.x, "query") <- NULL}),
                   bars_test %>% purrr::map(~{attr(.x, "query") <- NULL}))
})

test_that("market_data errors are informative when incompatible arguments are requested", {
  expect_error(market_data(ticker = "BYND", v = 1, multiplier = 30, timeframe = "m"), regexp = "1,5,15")
  expect_error(market_data(ticker = "BYND", v = 2, multiplier = 1, timeframe = "d", to = "2015/13/1"), regexp = "`to`")
})
test_that("market_data warning are informative when incompatible arguments are requested", {
  expect_warning(market_data(ticker = "BYND", v = 1, multiplier = 7, timeframe = "d"))
})
