#' @include Account.R
#' @include internal.R

context("Account family of functions works properly.")
test_that("account retrieves account details properly", {
  .a <- account()
  expect_length(.a, 26)
  expect_s3_class(.a$created_at, "POSIXct")
})

test_that("account_config properly retrieves account parameters", {
  .ac <- account_config()
  expect_length(.ac, 5)
})

test_that("account_config properly sets account parameters", {
  .ac <- account_config(suspend_trade = T, no_shorting = T)
  expect_true(all(unlist(.ac[c("no_shorting", "suspend_trade")])))
})

test_that("account_config can reset to default", {
  .ac <- account_config("default")
  expect_identical(.ac, list(dtbp_check = "entry", trade_confirm_email = "all", pdt_check = "entry", suspend_trade = FALSE, no_shorting = FALSE)[names(.ac)])
})

test_that("account_activities properly retrieves activities", {
  .aa <- account_activities()
  expect_s3_class(.aa, "tbl")
  .nr <- tryCatch(nrow(.aa), error = function(e) 0)
  if (.nr > 0) {
    expect_identical(names(.aa),c("id", "activity_type", "transaction_time", "type", "price", "qty", "side", "symbol", "leaves_qty", "order_id", "cum_qty"))
  } else {
    expect_message(account_activities(), regexp = "No account activities matching criteria.")
  }
})
`-` <- lubridate::`.__T__-:base`
test_that("account_activities retrieves date ranges correctly", {
  .aa <- account_activities(after = lubridate::today() - lubridate::weeks(2))
  .int <- lubridate::interval(lubridate::today() - lubridate::weeks(2), lubridate::today() + lubridate::days(1), tzone = Sys.timezone())
  .nr <- tryCatch(nrow(.aa), error = function(e) 0)
  if (.nr > 0) {
    expect_true(all(lubridate::`%within%`(.aa$transaction_time, .int)))
  } else {
    expect_message(account_activities(), regexp = "No account activities matching criteria.")
  }
  
})

test_that("account_activities throws an error if invalid date is entered", {
  expect_warning(expect_error(.aa <- account_activities(after = '20202-2'), regexp = "Check after argument"))
})

