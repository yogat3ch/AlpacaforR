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

vcr::use_cassette("get_headers_returns_headers_and_errors_as_intended", {
test_that("get_headers returns headers and errors as intended", {
  # Set environment variables
  # .env_var <- Sys.getenv(c("APCA-LIVE-API-KEY-ID", 
  #            "APCA-LIVE-API-SECRET-KEY",
  #            "APCA-PAPER-API-KEY-ID",
  #            "APCA-PAPER-API-SECRET-KEY"))
  withr::with_envvar(c("APCA-LIVE-API-KEY-ID" = "LIVEKEY", 
                       "APCA-LIVE-API-SECRET-KEY" = "LIVESECRET",
                       "APCA-PAPER-API-KEY-ID" = "PAPERKEY",
                       "APCA-PAPER-API-SECRET-KEY" = "PAPERSECRET"), 
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
                           "APCA-API-KEY-ID" = "PAPERKEY",
                           "APCA-API-SECRET-KEY" = "PAPERSECRET"
                         )
                       )
                       Sys.unsetenv("APCA-PAPER-API-KEY-ID")
                       expect_error(get_headers(), regexp = "APCA-PAPER", class = c("rlang_error"))
                       Sys.unsetenv("APCA-LIVE-API-SECRET-KEY")
                       expect_error(get_headers(T), regexp = "APCA-LIVE", class = c("rlang_error"))
                     }
  )
  
  # purrr::iwalk(.env_var, ~{
  #   .y <- sym(.y)
  #   eval(rlang::call2("Sys.setenv", !!rlang::enexpr(.y) := .x))
  # })
})
})


test_that("get_url works", {
  expect_identical(get_url(live = T),"https://api.alpaca.markets/")
  expect_identical(get_url(live = F),"https://paper-api.alpaca.markets/")
  expect_identical(get_url(data = TRUE, query = list(a = 1), "blah"), "https://api.polygon.io/v2/blah?a=1")
  expect_identical(get_url(data = TRUE, path = "2020-04-02", v = 1), "https://data.alpaca.markets/v1/2020-04-02")
})


