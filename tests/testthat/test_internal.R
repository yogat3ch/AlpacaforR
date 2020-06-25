#' @include internal.R
#' @include Calendar.R
library(dplyr)
library(magrittr)
library(rlang)
#quick detection of timespan abbreviations:  Thu Mar 26 08:34:00 2020 ----
.tf_opts <-
  list(
    m = c("m", "min", "minute"),
    h = c("h", "hour"),
    d = c("d", "day"),
    w = c("w", "week"),
    M = c("M", "mo", "month"),
    q = c("q", "quarter"),
    y = c("y", "year")
  )
# Create ordered factor or timeframe options
.tf_order <- purrr::map_chr(.tf_opts, tail, 1) %>% {factor(., levels = .)}
# Add appropriate variables to local environment
list2env(
  list(
    ticker = "AMZN",
    v = 2,
    after = NULL,
    until = NULL,
    full = T,
    unadjusted = F
  ),
  envir = .GlobalEnv
)

# Create a data.frame with multiple variations of typical calls for testing
suppressWarnings({
  .test <- data.frame(
    multiplier = c(
      minute = c(1, 5, 15),
      hour = 1,
      day = 1,
      week = c(1, 2, 4),
      month = c(1, 2, 3),
      quarter = 1,
      1
    ),
    timeframe = c(
      rep("minute", 3),
      "hour",
      "day",
      rep("week", 3),
      rep("month", 3),
      "quarter",
      "year"
    ),
    to = lubridate::ymd_hm("2020-04-03 13:05")
  ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(duration = lubridate::duration(
      multiplier,
      ifelse(
        as.character(timeframe) == "quarter",
        "month",
        as.character(timeframe)
      )
    )) %>%
    dplyr::mutate(
      upper_bound = to - (4 * duration),
      single_bound = to - duration,
      lower_bound = to - (.5 * duration)
    )
})

# Load the completed/checked data.frame for comparison
.p <- "rds/polygon_test.rds"
.p <- ifelse(basename(getwd()) != "testthat", paste0("tests/testthat/",.p), .p)
test_results <- readRDS(.p)

# Internals Unit Testing ----
context("Test that all CURL Retrieval functions work properly")
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
  expect_identical(get_url(T),"https://api.alpaca.markets")
  expect_identical(get_url(F),"https://paper-api.alpaca.markets")
})


context("Test that all bars_* helper functions work properly")

vcr::use_cassette("bars_bounds_returns_boundaries_as_anticipated", {
test_that("bars_bounds returns boundaries as anticipated", {
  .bound <- purrr::imap(c(ub = "upper_bound" , sb = "single_bound", lb = "lower_bound"), ~{
    .sym <- .x
    .bound <- purrr::pmap(.test, ~{
      # FINALLY figured out how to call arguments by name in pmap
      .from <- list(...)[[.sym]]
      .tf_num <- which(.tf_order %in% ..2)
      .bounds <- bars_bounds(from = .from, to = ..3, timeframe = ..2, multiplier = ..1)
      
    })
    
  })
  .bound <- tibble::as_tibble(.bound)
  names(.bound) <- paste0(names(.bound), "_bounds")
  expect_identical(dplyr::ungroup(dplyr::select(test_results, dplyr::ends_with("_bounds"))), .bound) 
})
})

vcr::use_cassette("bars_bounds_returns_NA_when_unexpected_parameters_are_provided", {
test_that("bars_bounds returns NA when unexpected parameters are provided", {
  expect_warning(bars_bounds(from = "1000-2-3", to = "4/15/2020", v = 2, timeframe = "minute", multiplier = 5), regexp = "`from`")
  expect_warning(bars_bounds(from = "Feb. 25, 2020", to = lubridate::today() + lubridate::years(51), v = 2, timeframe = "minute", multiplier = 5), regexp = "`to`")
})
})

vcr::use_cassette("bars_url_returns_URLs_as_anticipated", {
test_that("bars_url returns URLs as anticipated", {
  .url <- purrr::imap(c(ub = "ub_bounds" , sb = "sb_bounds", lb = "lb_bounds"), ~{
    .sym <- .x
    .bound <- purrr::pmap(test_results, ~{
      # FINALLY figured out how to call arguments by name in pmap
      .bounds <- list(...)[[.sym]]
      timeframe <- list(...)[["timeframe"]]
      .url <- bars_url(ticker = "AMZN", .bounds = .bounds, timeframe = ..2, multiplier = ..1, unadjusted = F)
      
    })
    
  })
  .url <- tibble::as_tibble(.url)
  names(.url) <- paste0(names(.url), "_url")
  expect_identical(dplyr::ungroup(dplyr::select(test_results, dplyr::ends_with("_url"))), .url) 
})
})
.test_raw <- dplyr::ungroup(dplyr::select(test_results, multiplier, timeframe, dplyr::ends_with("_data"))) %>% dplyr::mutate_at(dplyr::vars(dplyr::ends_with("_data")), ~purrr::map_depth(., 2, ~{attr(.x, "query") <- NULL}))

  .dat <- purrr::imap(c(ub = "ub_url" , sb = "sb_url", lb = "lb_url"), ~{
    .sym <- .x
    .bound <- purrr::pmap(test_results, ~{
      # FINALLY figured out how to call arguments by name in pmap
      list2env(list(...), envir = environment())
      .url <- list(...)[[.sym]]
      .dat <- bars_get(url = .url)
      .dat <- purrr::map(.dat, ~{attr(.x, "query") <- NULL})
      .lab <- paste0("bars_get_",.sym,"_",..1,..2)
      vcr::use_cassette(.lab, match_requests_on = "method", {
        test_that(.lab, {
          expect_identical(.test_raw %>% 
            dplyr::filter(multiplier == ..1 & timeframe == ..2) %>% 
            magrittr::extract2(stringr::str_replace(.sym, "url", "data")) %>% extract2(1),
            .dat)
        })
      })
    })
})


vcr::use_cassette("bars_expected_returns_the_appropriate_time_series", {
test_that("bars_expected returns the appropriate time-series", {
  .exp <- purrr::imap(c(ub = "ub" , sb = "sb", lb = "lb"), ~{
    .sym <- paste0(.x, "_data")
    .b <- paste0(.x, "_bounds")
    .exp <- purrr::pmap(test_results, ~{
      # FINALLY figured out how to call arguments by name in pmap
      .vars <- list(...)
      rlang::env_bind(rlang::current_env(), !!!list(...))
      .bars <- .vars[[.sym]]
      .bounds <- .vars[[.b]]
      .tf_num <- which(.tf_order %in% ..2)
      message(paste0("tf_num: ", .tf_num))
      .exp <- bars_expected(.bars, .bounds = .bounds)
    })
  })
  .exp <- tibble::as_tibble(.exp)
  names(.exp) <- paste0(names(.exp), "_complete")
  expect_true(all(purrr::map2_lgl(
    dplyr::ungroup(dplyr::select(test_results, dplyr::ends_with("_complete"))) %>% dplyr::mutate_all(~ purrr::map_depth(., 2, ~ {
      purrr::pluck(.x, "time")
    })),
    .exp, ~ {
      all(purrr::map2_lgl(.x, .y, ~ {
        
        .x <- ifelse(is.null(.x) || is.null(tryCatch(.x[[1]], error = function(e) NULL)), 0, .x)
        .y <- ifelse(is.null(.y) || is.null(tryCatch(.y[[1]], error = function(e) NULL)), 0, .y)
        .out <- all(.y[[1]] %in% .x[[1]])
        browser(expr = length(.out) == 0 || is.null(.out) || isFALSE(.out))
        return(.out)
      }))
    }
  )))
})
})

vcr::use_cassette("bars_missing_accurately_identifies_missing_data", {
test_that("bars_missing accurately identifies missing data", {
  .missing <- purrr::imap(c(ub = "upper_bound" , sb = "single_bound", lb = "lower_bound"), ~{
    .sym <- .x
    .bars <- paste0(.y,"_data")
    .bounds <- paste0(.y, "_bounds")
    (.missing <- purrr::pmap(test_results, ~{
      .vars <- list(...)
      .bars <- .vars[[.bars]]
      .from <- .vars[[.sym]]
      .bounds <- .vars[[.bounds]]
      .tf <- .vars[["timeframe"]]
      .tf_num <- which(.tf_order %in% .tf)
      message(paste0("from = ", as.character(.bounds[[1]]), " to = ", as.character(.bounds[[2]]), " multiplier = ",..1, " timeframe = ",..2, " tf_num: ",.tf_num))
      message(paste0("bars_missing"))
      if (inherits(.bars[[1]], "list")) return(.bars)
      bars_missing(bars = .bars, v = v, .bounds = .bounds, timeframe = ..2, multiplier = ..1)
    }))
  })
  
  names(.missing) <- paste0(names(.missing), "_missing")
  .missing <- tibble::as_tibble(.missing)
  expect_identical(dplyr::ungroup(dplyr::select(test_results, dplyr::ends_with("_missing"))), .missing) 
})
})

.test_raw <- dplyr::ungroup(dplyr::select(test_results, multiplier, timeframe, dplyr::ends_with("_complete"))) %>% dplyr::mutate_at(dplyr::vars(dplyr::ends_with("_complete")), ~purrr::map_depth(., 2, ~{attr(.x, "query") <- NULL;.x}))

  
.complete <- purrr::imap(c(ub = "upper_bound" , sb = "single_bound", lb = "lower_bound"), ~{
  .sym <- .y
  .complete <- purrr::pmap(test_results, ~{
    message(paste(.sym))
    .vars <- list(...)
    list2env(.vars[c("timeframe", "multiplier")], environment())
    .bars <- .vars[[paste0(.sym, "_data")]]
    .bounds <- .vars[[paste0(.sym, "_bounds")]]
    message(paste0("bars retrieved"))
    .missing <- .vars[[paste0(.sym, "_missing")]]
    message(paste0("missing retrieved"))
    tf_num(timeframe)
    message(paste0("bars_complete"))
    if (inherits(.bars[[1]], "list")) return(.bars)
    .complete <- bars_complete(bars = .bars, .missing = .missing, v = v, .bounds = .bounds, timeframe = timeframe, multiplier = multiplier, unadjusted = unadjusted, .tf_order = .tf_order)
    .complete <- purrr::map(.complete, ~{attr(.x, "query") <- NULL;.x})
    .lab <- paste0("bars_complete_",.sym,"_",..1,..2)

    vcr::use_cassette(.lab, match_requests_on = "method", {
      test_that(.lab, {
        expect_equal(.test_raw %>% 
                           dplyr::filter(multiplier == ..1 & timeframe == ..2) %>% 
                           magrittr::extract2(paste0(.sym, "_complete")) %>% extract2(1),
                         .complete, tolerance=1e-1)
      })
    })
  })
})


