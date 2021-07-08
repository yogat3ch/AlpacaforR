#' @include Market_Data.R
vcr::vcr_configure(dir = file.path(dirname(.log_path), "market_data"))
# Errors & Warnings ----
# Fri Jan 01 09:23:21 2021
test_that("bars_bounds returns NA when unexpected parameters are provided", {
  expect_error(bars_bounds(from = "1000-2-3", to = "4/15/2020", v = 2, timeframe = "minute", multiplier = 5), regexp = "parsed to")
  expect_error(bars_bounds(from = "Feb. 25, 2020", to = lubridate::today() + lubridate::years(51), v = 2, timeframe = "minute", multiplier = 5), regexp = "parsed to")
})




test_that("market_data_errors_when_incompatible_arguments_are_requested", {
  
  expect_error(
    market_data(
      "BYND",
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
        "BYND",
        v = 2,
        multiplier = 1,
        timeframe = "d",
        to = "2015/13/1"
      ),
      regexp = "(?:`from/after` omitted)"
    ),
    regexp = "2015/13/1 was parsed to NA . Is this expected?"
  )
})


vcr::use_cassette("market_data_warnings_are_informative", match_requests_on = c("method"), {
  test_that("market_data_warnings_are_informative", {
    expect_warning(market_data("BYND", v = 1, multiplier = 7, timeframe = "d"), regexp = "The v1 API only accepts 1 as a `multiplier` for the day timeframe. One day bars will be returned.")
  })
})

# Supporting functions ----
# Fri Jan 01 09:23:46 2021
test_that("bars_bounds returns boundaries as anticipated", {
  .bounds <- purrr::map(stats::setNames(.pre, paste0(.pre,"_bounds")), ~{
    .pre <- .x
    .bound <- purrr::pmap(test_market_data$bars, ~{
      # FINALLY figured out how to call arguments by name in pmap
      .vars <- rlang::dots_list(...)
      rlang::exec(evar_bind, !!!.vars, from = .vars[[.pre]])
      list(bounds = evar$bounds,
           cal = evar$cal)
    })
    
  }) %>%
    tibble::as_tibble()
  .object <- tibble::as_tibble(purrr::map_depth(setNames(.bounds, paste0(.pre, "_bounds")), 2, ~ {
    .x$bounds
  }))
  
  expect_identical(.object, dplyr::ungroup(dplyr::select(test_market_data$bars, dplyr::ends_with("_bounds")))) 
})


.dat <- purrr::map(.pre, ~{
  .pre <- "lb"
  .bound <- purrr::pmap(test_market_data$bars, ~{
    .sym <- paste0(.pre,"_url")
    .vars <- rlang::dots_list(...)
    evar <- evar_reset()
    rlang::exec(evar_bind, !!!.vars, bounds = .vars[[paste0(.pre, "_bounds")]])
    .url <- bars_url("AMZN", evar = evar)
    .lab <- paste0("bars_get_",.sym,"_",..1,..2)
    
    .expected <- test_market_data$bars %>%
      dplyr::filter(multiplier == .vars$multiplier &
                      timeframe == .vars$timeframe) %>%
      {.[[paste0(.pre, "_data")]]} %>% 
      get_tibble()
    
    vcr::use_cassette(.lab, allow_playback_repeats = TRUE, {
      # added if since 1-day lower-bound returns no data
      if (is_legit(.expected)) {
        test_that(.lab, {
          testthat::skip_on_cran()
          .object <- bars_get(url = .url, evar = evar, just_tibble = TRUE)
          # check that all time points are present
          expect_equal(time_index(.object, "v"), time_index(.expected, "v"), ignore_attr = TRUE)
          # check that the mean of each column is with 5% of the expected mean
          purrr::walk2(
            colMeans(dplyr::select(as.data.frame(.object), where(is.numeric))),
            colMeans(dplyr::select(as.data.frame(.expected), where(is.numeric))),
            ~{
              expect_equal(.x,
                           .y,
                           tolerance = .y * .05,
                           ignore_attr = TRUE)
            })
        })
      }
    })
  })
})

.complete <- purrr::walk(stats::setNames(.pre, paste0(.pre,"_complete")), ~ {
  .pre <- .x
  .complete <- purrr::pwalk(test_market_data$bars, ~ {
    .vars <- rlang::dots_list(...)
    evar <- evar_reset()
    rlang::exec(evar_bind, !!!.vars, bounds = .vars[[paste0(.pre, "_bounds")]])
    .bars <- .vars[[paste0(.pre, "_data")]]
    .lab <- paste0("bars_complete_", .pre, "_", .vars$multiplier, .vars$timeframe)
      .expected <- dplyr::ungroup(test_market_data$bars) %>%
        dplyr::filter(multiplier == .vars$multiplier & timeframe == .vars$timeframe) %>%
        dplyr::pull(paste0(.pre, "_complete")) %>%
        get_tibble()
      
      if (is_legit(.expected)) {
        vcr::use_cassette(.lab, match_requests_on = "uri", {
          
          test_that(.lab, {
            testthat::skip_on_cran()
            .object <- bars_complete(bars = .bars, evar = evar)
            .object <- get_tibble(.object)
            # check that all time points are present
            expect_equal(time_index(.object, "v"), time_index(.expected, "v"), ignore_attr = TRUE)
            # check that the mean of each column is with 5% of the expected mean
            purrr::walk2(
              colMeans(dplyr::select(as.data.frame(.object), where(is.numeric))),
              colMeans(dplyr::select(as.data.frame(.expected), where(is.numeric))),
              ~{
                expect_equal(.x,
                             .y,
                             tolerance = .y * .05,
                             ignore_attr = TRUE)
              })
          })
        })
      }
  })
})



# market_data ----
# Wed Apr 15 16:27:58 2020

vcr::use_cassette("market_data_works_when_v_1_and_full_T", match_requests_on = "uri", {
test_that("market_data works when v = 1 and full = T", {
  testthat::skip_on_cran()
  .object <- market_data(c("BYND"), v = 1, from = "2020-03-06", until = "2020-12-25", multiplier = 5, timeframe = "m", full = T)
  .expected <- test_market_data$v1
  # check that the majority of time points are present
  # small discrepancies appear inevitable, so mean is used here
  expect_equal(mean(time_index(.object, "v")),  mean(time_index(.expected, "v")), ignore_attr = TRUE, tolerance = 10)
  # check that the mean of each column is with 5% of the expected mean
  purrr::walk2(
    colMeans(dplyr::select(as.data.frame(.object), where(is.numeric))),
    colMeans(dplyr::select(as.data.frame(.expected), where(is.numeric))),
    ~{
      expect_equal(.x,
                   .y,
                   tolerance = .y * .05,
                   ignore_attr = TRUE)
    })
})
})

vcr::use_cassette("market_data_works_when_v_2_and_full_T", match_requests_on = "uri", {
test_that("market_data works when v = 2 and full = T", {
  testthat::skip_on_cran()
  .object <- market_data(c("BYND"), v = 2, from = "2020-11-02", until = "2020-12-25", multiplier = 1, timeframe = "m", full = T)
  .expected <- test_market_data$v2
  # check that the majority of time points are present
  # small discrepancies appear inevitable, so mean is used here
  expect_equal(mean(time_index(.object, "v")),  mean(time_index(.expected, "v")), ignore_attr = TRUE, tolerance = 5)
  # check that the mean of each column is with 5% of the expected mean
  purrr::walk2(
    colMeans(dplyr::select(as.data.frame(.object), where(is.numeric))),
    colMeans(dplyr::select(as.data.frame(.expected), where(is.numeric))),
    ~{
      expect_equal(.x,
                   .y,
                   tolerance = .y * .05,
                   ignore_attr = TRUE)
    })
})
})

vcr::use_cassette("market_data_works_with_last_quote", match_requests_on = "uri", {
  test_that("market_data_works_with_last_quote", {
    testthat::skip_on_cran()
    .q <- market_data(c("TWTR", "BYND", "AAPL"), timeframe = "qu", v = 1)
    expect_equal(nrow(.q), 3)
    expect_equal(length(attr(.q, "query")), 3)
    expect_identical(.q$symbol, c("TWTR", "BYND", "AAPL"))
  })
})

vcr::use_cassette("market_data_works_with_last_trade", match_requests_on = "uri", {
  test_that("market_data_works_with_last_trade", {
    testthat::skip_on_cran()
    .t <- market_data(c("TWTR", "BYND", "AAPL"), timeframe = "t", v = 1)
    expect_equal(nrow(.t), 3)
    expect_equal(length(attr(.t, "query")), 3)
    expect_identical(.t$symbol, c("TWTR", "BYND", "AAPL"))
  })
})


test_that("v2_trades_latest_trades_quotes_latest_quotes", {
  testthat::skip_on_cran()
    test <- expand.grid(symbol = list("SPOT", c("SPOT", "BYND")), timeframe = c("t", "lt", "q", "lq"), v = 2, from = "2021-05-26", to = "2021-05-27", stringsAsFactors = FALSE) %>% tibble::rownames_to_column()
    results <- purrr::pmap(test, ~{
      .vars <- rlang::dots_list(...)
      vcr::use_cassette(paste0("v2_t_lt_q_lq.", .vars$rowname), match_requests_on = "uri", {
      .res <- rlang::exec(market_data, symbol = unlist(.vars$symbol), !!!.vars[!names(.vars) %in% c("symbol", "rowname")])
        })
      .res
      })
    
    purrr::walk2(results, test$timeframe, ~{
      res <- get_tibble(.x)
      .nms <- switch(.y, 
             t = ,
             lt = nms$t,
             q = ,
             lq = nms$q)
      expect_identical(names(res), .nms)
      expect_s3_class(time_index(res, "v"), "POSIXct")
    })
  
})


testthat::test_that("v2_ss", {
  testthat::skip_on_cran()
  test <- expand.grid(symbol = list("SPOT", c("SPOT", "BYND")), timeframe = c("ss"), v = 2, from = "2021-05-26", to = "2021-05-27", stringsAsFactors = FALSE) %>% tibble::rownames_to_column()
  results <- purrr::pmap(test, ~{
    .vars <- rlang::dots_list(...)
    vcr::use_cassette(paste0("v2_ss.", .vars$rowname), match_requests_on = "uri", {
      .res <- rlang::exec(market_data, symbol = unlist(.vars$symbol), !!!.vars[!names(.vars) %in% c("symbol", "rowname")])
    })
    .res
  })
  test <- rlang::as_function(~{
    .nms <- switch(stringr::str_extract(.y, stringr::regex("trade|quote|bar", ignore_case = TRUE)), 
                   Trade = nms$t,
                   Quote = nms$q,
                   Bar = nms$b)
    expect_identical(names(.x), .nms)
    expect_s3_class(time_index(.x, "v"), "POSIXct")
  })
  purrr::iwalk(results[[1]], test)
  
  purrr::walk(results[[2]], ~{
    purrr::iwalk(.x, test)
  })
})


