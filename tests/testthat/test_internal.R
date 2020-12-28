#' @include internal.R
#' @include Calendar.R

# Internals Unit Testing ----
# Sat Apr 11 11:00:11 2020

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
})



test_that("bars_bounds returns boundaries as anticipated", {
  .bound <- purrr::imap(c(ub = "upper_bound" , sb = "single_bound", lb = "lower_bound"), ~{
    .sym <- .x
    .bound <- purrr::pmap(test_internal, ~{
      # FINALLY figured out how to call arguments by name in pmap
      .from <- list(...)[[.sym]]
      .tf_num <- which(evar$tf_order %in% ..2)
      .bounds <- bars_bounds(from = .from, to = ..3, timeframe = ..2, multiplier = ..1)
      
    })
    
  })
  .bound <- tibble::as_tibble(.bound)
  names(.bound) <- paste0(names(.bound), "_bounds")
  expect_identical(dplyr::ungroup(dplyr::select(test_internal, dplyr::ends_with("_bounds"))), .bound) 
})



test_that("bars_bounds returns NA when unexpected parameters are provided", {
  expect_warning(bars_bounds(from = "1000-2-3", to = "4/15/2020", v = 2, timeframe = "minute", multiplier = 5), regexp = "parsed to")
  expect_warning(bars_bounds(from = "Feb. 25, 2020", to = lubridate::today() + lubridate::years(51), v = 2, timeframe = "minute", multiplier = 5), regexp = "parsed to")
})



test_that("bars_url returns URLs as anticipated", {
  .url <- purrr::imap(c(ub = "ub_bounds" , sb = "sb_bounds", lb = "lb_bounds"), ~{
    .sym <- .x
    .urls <- purrr::pmap(test_internal, ~{
      # FINALLY figured out how to call arguments by name in pmap
      .dots <- list(...)
      .bounds <- .dots[[.sym]]
      timeframe <- .dots[["timeframe"]]
      .url <- bars_url(ticker = "AMZN", bounds = .bounds, timeframe = ..2, multiplier = ..1, evar = evar)
      
    })
    
  })
  .url <- tibble::as_tibble(.url)
  names(.url) <- paste0(names(.url), "_url")
  
  # Strip the API key for comparison
  .out <- purrr::map_depth(list(expected = dplyr::ungroup(dplyr::select(test_internal, dplyr::ends_with("_url"))), object = .url), 3, ~{
    .u <- httr::parse_url(.x)
    .u$query$apiKey <- NULL
    httr::build_url(.u)
  })
  
  expect_identical(.out$object, .out$expected) 
})



.pre <- c(ub = "ub", sb = "sb", lb = "lb")
.dat <- purrr::map(.pre, ~{
  .pre <- .x
  .bound <- purrr::pmap(test_internal, ~{
    .sym <- paste0(.pre,"_url")
    # FINALLY figured out how to call arguments by name in pmap
    .vars <- list(...)
    .url <- .vars[[.sym]]
    .object <- bars_get(url = .url, bounds = .vars[[paste0(.pre, "bounds")]], timeframe = .vars$timeframe, multiplier = .vars$multiplier, v = 2)
    .expected <- test_internal %>%
      dplyr::filter(multiplier == .vars$multiplier &
                      timeframe == .vars$timeframe) %>%
      {.[[paste0(.pre, "_data")]][[1]]}
    if (Sys.Date() < lubridate::ymd("2020-12-31") && .vars$timeframe == "year") {
      # Alpaca aggregates for the entire year, 2020 must be complete for the value to stop changing
      # COMBAK Remove me in 2021
      .expected$AMZN <- .expected$AMZN[-nrow(.expected$AMZN),]
      .object$AMZN <- .object$AMZN[-nrow(.object$AMZN),]
    }
    .lab <- paste0("bars_get_",.sym,"_",..1,..2)
    vcr::use_cassette(.lab, match_requests_on = "uri", {
      test_that(.lab, {
        expect_equal(.object,
          .expected,
          ignore_attr = TRUE)
      })
    })
    .object
  })
})



# Grab calendar
# .cal <- purrr::map_depth(setNames(test_internal[paste0(.pre, "_bounds")], paste0(.pre,"_cal")), 2, ~{
#     do.call(calendar, args = .x)
# })
#   
# test_internal <- tibble::add_column(test_internal, !!!.cal, .after = "lb_bounds")

.complete <- purrr::map(c(ub_complete = "ub", sb_complete = "sb", lb_complete = "lb"), ~ {
  .pre <- .x
  .complete <- purrr::pmap(test_internal, ~ {
    .vars <- list(...)
    .bars <- .vars[[paste0(.pre, "_data")]]
    rlang::env_bind(evar, cal = .vars[[paste0(.pre, "_cal")]])
    .lab <- paste0("bars_complete_", .pre, "_", .vars$multiplier, .vars$timeframe)
    vcr::use_cassette(.lab, match_requests_on = "uri", {
    .complete <- bars_complete(bars = .bars, timeframe = .vars$timeframe, multiplier = .vars$multiplier, evar = evar)
    .expected <- dplyr::ungroup(test_internal) %>%
      dplyr::filter(multiplier == .vars$multiplier & timeframe == .vars$timeframe) %>%
      dplyr::pull(paste0(.pre, "_complete")) %>% magrittr::extract2(1)
      test_that(.lab, {
        expect_equal(.complete,
        .expected,
        tolerance = .5,
        ignore_attr = TRUE
        )
      })
    })
  })
})

