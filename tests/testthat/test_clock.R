#' @include internal.R
#' @include Clock.R

context("Clock functions correctly")
vcr::use_cassette("clock_returns_the_appropriate_response", {
test_that("clock returns the appropriate response", {
  .c <- clock()
  if (lubridate::wday(lubridate::today(), week_start = 7) %in% c(1,7)) {
    expect_false(.c$is_open)
  } 
  expect_s3_class(do.call(c, .c[c(1,3:4)]), "POSIXct")
  expect_true(is.list(.c))
  expect_length(.c, 4)
})
})

vcr::use_cassette("clock_returns_the_appropriate_response_when_timezone_is_different", {
test_that("clock returns the appropriate response when timezone is different", {
  withr::with_envvar(c(TZ = "America/Los_Angeles"), {
    .c <- clock()
    expect_s3_class(do.call(c, unlist(purrr::compact(purrr::map(.c, ~{if (length(.x) > 1) .x[1:2]})), recursive = F)), "POSIXct")
    expect_s4_class(.c$next_close$offset, "Period")
    expect_named(.c$next_close, c("market", "local", "offset"))
  })
  
})
})
