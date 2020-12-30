#' @include Market_Data.R


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
  expect_error(bars_bounds(from = "1000-2-3", to = "4/15/2020", v = 2, timeframe = "minute", multiplier = 5), regexp = "parsed to")
  expect_error(bars_bounds(from = "Feb. 25, 2020", to = lubridate::today() + lubridate::years(51), v = 2, timeframe = "minute", multiplier = 5), regexp = "parsed to")
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



# market_data ----
# Wed Apr 15 16:27:58 2020
vcr::vcr_configure(dir = file.path(dirname(.log_path), "market_data"))
vcr::use_cassette("market_data_works_when_v_2_and_full_T", match_requests_on = "uri", {
test_that("market_data works when v = 2 and full = T", {
  expect_warning({bars <- market_data(ticker = c("BYND"), v = 2, from = "2020-01-05", until = "2020-12-25", multiplier = 5, timeframe = "m", full = T)})
  expect_equal(bars,
               test_market_data$v2,
               ignore_attr = TRUE)
})
})

vcr::use_cassette("market_data_works_when_v_1_and_full_T", match_requests_on = "uri", {
test_that("market_data works when v = 1 and full = T", {
  expect_message({bars <- market_data(ticker = c("BYND"), v = 1, from = "2020-01-05", until = "2020-12-25", multiplier = 5, timeframe = "m", full = T)})
  expect_equal(bars,
               test_market_data$v1,
               ignore_attr = TRUE)
})
})

vcr::use_cassette("market_data_works_with_last_quote", match_requests_on = "uri", {
  test_that("market_data_works_with_last_quote", {
    .q <- market_data(c("TWTR", "BYND", "AAPL"), timeframe = "q")
    expect_equal(nrow(.q), 3)
    expect_equal(length(attr(.q, "query")), 3)
    expect_identical(.q$symbol, c("TWTR", "BYND", "AAPL"))
  })
})

vcr::use_cassette("market_data_works_with_last_trade", match_requests_on = "uri", {
  test_that("market_data_works_with_last_trade", {
    .t <- market_data(c("TWTR", "BYND", "AAPL"), timeframe = "t")
    expect_equal(nrow(.t), 3)
    expect_equal(length(attr(.t, "query")), 3)
    expect_identical(.t$symbol, c("TWTR", "BYND", "AAPL"))
  })
})

vcr::use_cassette("market_data_errors_when_incompatible_arguments_are_requested", {
test_that("market_data_errors_when_incompatible_arguments_are_requested", {
  
  expect_error(
    market_data(
      ticker = "BYND",
      v = 1,
      to = Sys.Date(),
      multiplier = 30,
      timeframe = "m"
    ),
    regexp = "1,5,15"
  )
  expect_error(
    expect_message(
      market_data(
        ticker = "BYND",
        v = 2,
        multiplier = 1,
        timeframe = "d",
        to = "2015/13/1"
      ),
      regexp = "(?:`from` argument omitted)|(?:Floor/Ceiling)|(?:'from' coerced to)|(?:'to' coerced to NA)"
    ),
    regexp = "2015/13/1 was parsed to NA . Is this expected?"
  )
})
})

vcr::use_cassette("market_data_warning_and_messages_are_informative", match_requests_on = c("method"), {
test_that("market_data_warning_and_messages_are_informative", {
  expect_warning(expect_message(market_data(ticker = "BYND", v = 1, multiplier = 7, timeframe = "d"), regexp = "(?:`to` argument omitted)|(?:`from` argument omitted)"), regexp = "The v1 API only accepts 1 as a `multiplier` for the day timeframe. One day bars will be returned.")
})
})


