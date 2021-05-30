vcr::vcr_configure(dir = file.path(dirname(.log_path), "helpers"))

test_that("match_letters", {
  expect_identical(match_letters("m", "minutes", "months", "weeks", capitalize = TRUE, several.ok = TRUE), c("minutes", "months"))
  expect_identical(match_letters(c("account_alpaca", "data_alpaca"), "account_alpaca", "data_alpaca", "polygon", several.ok = TRUE), c("account_alpaca", "data_alpaca"))
  expect_identical(match_letters("mo", mi = "minute", ho = "hour", da = "day", we = "week", Mo = "month", qu = "quarter", ye = "year", x = 2), c(Mo = "month"))
})

test_that("date_try", {
  expect_identical(try_date("01-05-2020"), lubridate::mdy("01-05-2020"))
  expect_identical(try_date(2023), lubridate::mdy("01-01-2023"))
  expect_identical(try_date("14-05-2020"), lubridate::mdy("05-14-2020"))
  expect_identical(try_date(16224037, tz = "UTC"), lubridate::ymd("2021-05-30"), tolerance = .0001)
  expect_identical(try_date(162240379700, timeframe = "m", tz = "UTC"), lubridate::ymd_hms("2021-05-30 15:43:17"), tolerance = .0001)
})

.t <- tsibble::tsibble(time = Sys.Date() - do.call(c, purrr::map(1:12, lubridate::days)), index = time)

test_that("time_index", {
  expect_identical(time_index(.t), "time")
  expect_identical(time_index(.t, "l"), rlang::expr(time))
  expect_identical(time_index(.t, "v"), .t$time)
})

test_that("time_interval", {
  expect_identical(time_interval(.t), list(multiplier = 1, timeframe = "day"))
})

test_that("%|z|%", {
  expect_false(numeric(0) %|z|% FALSE)
})