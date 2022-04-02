#' @include internal.R
#' @include Clock.R
vcr::vcr_configure(dir = file.path(dirname(.log_path), "clock"))
vcr::use_cassette("clock", {
test_that("clock returns the appropriate response", {
  .c <- clock()
  expect_type(.c$is_open, "logical")
  expect_s3_class(do.call(c, .c[c(1,3:4)]), "POSIXct")
  expect_true(is.list(.c))
  expect_length(.c, 4)
})
})

vcr::use_cassette("clock_timezone", {
test_that("clock returns the appropriate response when timezone is different", {
  withr::with_envvar(c(TZ = "America/Los_Angeles"), {
    .c <- clock()
    expect_s3_class(rlang::exec(c, !!!.c$local[c("timestamp", "next_open", "next_close")]), "POSIXct")
    expect_s4_class(.c$local$offset, "Period")
  })

})
})
