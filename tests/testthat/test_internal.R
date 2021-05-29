#' @include internal.R
#' @include Calendar.R

# Internals Unit Testing ----
# Sat Apr 11 11:00:11 2020
vcr::vcr_configure(dir = file.path(dirname(.log_path), "internal"))
vcr::use_cassette("response_text_clean_returns_appropriate_object", {
test_that("response_text_clean returns appropriate object", {
  .resp <- httr::GET('https://postman-echo.com/get?foo1=bar1&foo2=bar2')
  .resp <- response_text_clean(.resp)
  expect_identical(.resp$args, list("foo1" = "bar1", "foo2" = "bar2"))
  expect_true(stringr::str_detect(.resp$headers$accept, "json"))
})
})


test_that("get_headers returns headers and errors as intended", {
  # Set environment variables
  withr::with_envvar(c("APCA-LIVE-KEY" = "LIVEKEY", 
                       "APCA-LIVE-SECRET" = "LIVESECRET",
                       "APCA-PAPER-KEY" = "PAPERKEY",
                       "APCA-PAPER-SECRET" = "PAPERSECRET"), 
                     {
                       headers <- list()
                       headers$live <- get_headers(live = T)
                       headers$paper <- get_headers(live = F)
                       expect_s3_class(headers$live, "request")
                       expect_s3_class(headers$paper, "request")
                       expect_identical(
                         headers$live$headers,
                         c(
                           "APCA-API-KEY-ID" = "LIVEKEY",
                           "APCA-API-SECRET-KEY" = "LIVESECRET"
                         )
                       )
                       expect_identical(
                         headers$paper$headers,
                         c(
                           "APCA-KEY-ID" = "PAPERKEY",
                           "APCA-SECRET-KEY" = "PAPERSECRET"
                         )
                       )
                       Sys.unsetenv("APCA-PAPER-KEY")
                       expect_error(get_headers(), regexp = "APCA-PAPER", class = c("rlang_error"))
                       Sys.unsetenv("APCA-LIVE-SECRET")
                       expect_error(get_headers(T), regexp = "APCA-LIVE", class = c("rlang_error"))
                     }
  )
})



test_that("get_url works", {
  expect_identical(get_url(live = T),"https://api.alpaca.markets/v2")
  expect_identical(get_url(live = F),"https://paper-api.alpaca.markets/v2")
  expect_identical(stringr::str_extract(get_url(data = TRUE, query = list(a = 1), "blah", api = "api", poly = TRUE), ".*(?=\\&api)"), "https://api.polygon.io/v2/blah?a=1")
  expect_identical(get_url(data = TRUE, path = "2020-04-02", v = 1), "https://data.alpaca.markets/v1/2020-04-02")
})


