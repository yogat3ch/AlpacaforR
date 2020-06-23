#' @include internal.R
#' @include Clock.R

context("calendar functions correctly")
vcr::use_cassette("calendar_returns_the_appropriate_response", {
test_that("calendar returns the appropriate response", {
  expect_message({.c <- calendar("2020-03-04", "2020-04-26")}, regexp = "Sunday")
  expect_equal(lubridate::tz(.c$date), "America/New_York")
  expect_s3_class(.c$date, "POSIXct")
  expect_s4_class(.c$day, "Interval")
  expect_s4_class(.c$session, "Interval")
  expect_s3_class(.c$dow, "factor")
  expect_s3_class(.c, "data.frame")
  expect_length(.c, 8)
})
})

