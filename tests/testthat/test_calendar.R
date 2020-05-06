#' @include internal.R
#' @include Clock.R

context("calendar functions correctly")
test_that("calendar returns the appropriate response", {
  expect_message({.c <- calendar()}, regexp = "from, to arg(s) is/are NULL")
  expect_message({.c <- calendar("2020-03-04", "2020-04-26")}, regexp = "Sunday")
  expect_s3_class(.c$date, "POSIXct")
  expect_s4_class(.c$day, "Interval")
  expect_s4_class(.c$session, "Interval")
  expect_s3_class(.c$dow, "factor")
  expect_s3_class(.c, "data.frame")
  expect_length(.c, 8)
})
test_that("get_calendar returns correct data", {
  .cal <- get_calendar("2020-04-13", "2020-04-18")
  .cal <- get_calendar("2020-04-13", "2020-04-18")
  expect_equal(nrow(.cal), 5)
  expect_equal(.cal$date, seq.POSIXt(as.POSIXct("2020-04-13"), as.POSIXct("2020-04-17"), by = "day"))
})
