#' @include internal.R
#' @include Clock.R
vcr::vcr_configure(dir = file.path(dirname(.log_path), "clock"))
vcr::use_cassette("clock_returns_the_appropriate_response", {
test_that("clock returns the appropriate response", {
  .c <- clock()
  expect_type(.c$is_open, "logical")
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
