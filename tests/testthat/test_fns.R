#' @include internal.R
#' @include alpaca4R.R
library(dplyr)
library(magrittr)
library(rlang)
library(testthat)

# get_bars ----
# Wed Apr 15 16:27:58 2020
bars_test <- readRDS("rds/get_bars.rds")
context("Tests the AlpacaforR exported functions")
test_that("get_bars works when v = 2 and full = T", {
  bars <- get_bars(ticker = c("BYND"), v = 2, from = lubridate::floor_date(lubridate::now(), "year"), multiplier = 1, timeframe = "h", full = T)
  expect_identical(bars %>% purrr::map(~{attr(.x, "query") <- NULL}),
                   bars_test %>% purrr::map(~{attr(.x, "query") <- NULL}))
})

test_that("get_bars errors are informative when incompatible arguments are requested", {
  expect_error(get_bars(ticker = "BYND", v = 1, multiplier = 30, timeframe = "m"), regexp = "1,5,15")
  expect_error(get_bars(ticker = "BYND", v = 2, multiplier = 1, timeframe = "d", to = "2015/13/1"), regexp = "`to`")
})
test_that("get_bars warning are informative when incompatible arguments are requested", {
  expect_warning(get_bars(ticker = "BYND", v = 1, multiplier = 7, timeframe = "d"))
})

test_that("get_calendar returns correct data", {
  .cal <- get_calendar("2020-04-13", "2020-04-18")
  expect_equal(nrow(.cal), 5)
  expect_equal(.cal$date, seq.POSIXt(as.POSIXct("2020-04-13"), as.POSIXct("2020-04-17"), by = "day"))
  expect_s4_class(.cal$day, "Interval")
  expect_s3_class(.cal$dow, "factor")
})