#' @include Account.R
#' @include internal.R

vcr::vcr_configure(dir = file.path(dirname(.log_path), "account"))
vcr::use_cassette("account_retrieves_account_details_properly", {
test_that("account retrieves account details properly", {
  .a <- account()
  expect_length(.a, 26)
  expect_s3_class(.a$created_at, "POSIXct")
})
})

vcr::use_cassette("account_config_properly_retrieves_account_parameters", {
test_that("account_config properly retrieves account parameters", {
  .ac <- account_config()
  expect_length(.ac, 5)
})
})

vcr::use_cassette("account_config_properly_sets_account_parameters", {
test_that("account_config properly sets account parameters", {
  .ac <- account_config(suspend_trade = T, no_shorting = T)
  expect_true(all(unlist(.ac[c("no_shorting", "suspend_trade")])))
})
})

vcr::use_cassette("account_config_can_reset_to_default", {
test_that("account_config can reset to default", {
  .ac <- account_config("default")
  expect_equal(.ac, list(dtbp_check = "entry", trade_confirm_email = "all", pdt_check = "entry", suspend_trade = FALSE, no_shorting = FALSE)[names(.ac)], ignore_attr = TRUE)
})
})

vcr::use_cassette("account_activities_properly_retrieves_activities", {
test_that("account_activities properly retrieves activities", {
  .aa <- account_activities()
  expect_s3_class(.aa, "tbl")
  .nr <- tryCatch(nrow(.aa), error = function(e) 0)
  if (.nr > 0) {
    expect_identical(names(.aa),c("id", "activity_type", "transaction_time", "type", "price", "qty", "side", "symbol", "leaves_qty", "order_id", "cum_qty"))
  } else {
    expect_message(account_activities(), regexp = "No account activities matching criteria.")
  }
})
})
`-` <- lubridate::`.__T__-:base`
vcr::use_cassette("account_activities_retrieves_date_ranges_correctly", {
test_that("account_activities retrieves date ranges correctly", {
  .aa <- account_activities(after = lubridate::ymd("2020-06-01") - lubridate::weeks(2), until = lubridate::ymd("2020-06-01"))
  .int <- lubridate::interval(lubridate::ymd("2020-06-01") - lubridate::weeks(2), lubridate::ymd("2020-06-01") + lubridate::days(1), tzone = Sys.timezone())
  .nr <- tryCatch(nrow(.aa), error = function(e) 0)
  if (.nr > 0) {
    expect_true(all(lubridate::`%within%`(.aa$transaction_time, .int)))
  } else {
    expect_message(account_activities(), regexp = "No account activities matching criteria.")
  }
  
})
})

vcr::use_cassette("account_activities_throws_an_error_if_invalid_date_is_entered", {
test_that("account_activities throws an error if invalid date is entered", {
  expect_error(.aa <- account_activities(after = '20202-2'), regexp = "was parsed to NA")
})
})

.sample <- sample(s = 10, c(1:77))
.p <- c(.test = "rds/account_portfolio_test.rds", .res = "rds/account_portfolio_results.rds")
e <- environment()
.p <- purrr::iwalk(.p, ~{
  assign(.y, readRDS(ifelse(basename(getwd()) != "testthat", paste0("tests/testthat/",.x), .x)), envir = e)
})
`%>%` <- magrittr::`%>%`
purrr::imap(c(char = "pchars", periods = "periods"), ~{
  .period <- .x
  .type <- .y
  purrr::pmap(e$.test[.sample,], ~{
    .vars <- list(...)
    .tt <- glue::glue("account_portfolio works properly with {.period} for test rowid: {.vars$rowid}")
    .ct <- paste0(stringr::str_extract_all(.tt, "[:alnum:]+")[[1]], collapse = "_")
vcr::use_cassette(.ct, {
    test_that(.tt, {
      .res <- e$.res[[.type]][[.vars$rowid]]
      output <- list(warnings = attr(.res, "warn"), messages = attr(.res, "msg"))
      output <- purrr::map(purrr::compact(output), ~{
        .i <- .x
        paste0(purrr::map_chr(unique(stringr::str_extract_all(.i, '[:alnum:]+')[[1]]), ~{glue::glue("(?:{.x})")}), collapse = "|")
      })
      out <- new.env()
      if (!is.null(output$warnings) && !is.null(output$messages)) {
        expect_message(expect_warning({out$out <- account_portfolio(.vars[[.period]], timeframe = .vars$timeframe, date_end = .vars$date_end, extended_hours = .vars$extended_hours)}, regexp = output$warnings), regexp = output$messages)
      } else if (!is.null(output$warnings)) {
        expect_warning({out$out <- account_portfolio(.vars[[.period]], timeframe = .vars$timeframe, date_end = .vars$date_end, extended_hours = .vars$extended_hours)}, regexp = output$warnings)
      } else if (!is.null(output$messages)) {
        expect_message({out$out <- account_portfolio(.vars[[.period]], timeframe = .vars$timeframe, date_end = .vars$date_end, extended_hours = .vars$extended_hours)}, regexp = output$messages)
      } else {
        out$out <- account_portfolio(.vars[[.period]], timeframe = .vars$timeframe, date_end = .vars$date_end, extended_hours = .vars$extended_hours)
      }
      browser(expr =  is.null(.res) || is.null(out$out))
      expect_equal(nrow(.res), nrow(out$out), ignore_attr = TRUE)
})
    })
  })
})



# .test <- data.frame(
#   pmultiplier = c(
#     day = c(1,7),
#     week = c(1, 2, 4),
#     month = c(1, 2, 3),
#     year = rep(1,3)
#   ),
#   period = c(
#     c("days", "D"),
#     c("weeks", "W", "Wk"),
#     c("months", "Mo", "M"),
#     c("years", "Y", "A")
#   ),
#   stringsAsFactors = F
# )
# `-` <- lubridate::`.__T__-:base`
# .test <- tibble::as_tibble(expand.grid(
#   .test %>% purrr::pmap_chr(paste0),
#   data.frame(
#     multiplier = c(
#       minute = c(1, 5, 15),
#       hour = c(1, 1),
#       day = c(1, 1)
#     ),
#     timeframe = c(c("m", "minute", "Min"),
#                   c("h", "Hour"),
#                   c("day", "D"))
#   ) %>% purrr::pmap_chr(paste0)
# )) %>%
#   setNames(c("pchars", "timeframe")) %>%
#   dplyr::mutate(periods = (function(pchars){
#     purrr::map(.[["pchars"]], ~{
#     .m <- stringr::str_extract(.x, "^\\d+")
#     .p <- tolower(substr(stringr::str_extract(.x, "[A-Za-z]+"), 0, 1))
#     .p <- ifelse(.p == "a", "y", .p)
#     .p <- c(d = "days", w = "weeks", m = "months", y = "years")[.p]
#     lubridate::period(as.numeric(.m), .p)
#     })
#     })(.)) %>%
#   dplyr::ungroup() %>%
#   # randomize date_end based on subtracting periods at random from today
#   mutate(date_end = do.call(c,purrr::map(sample(.[["periods"]], nrow(.)), ~{lubridate::ymd("2020-06-15") - .x})))
# .test$extended_hours <- sample(c(T,F), 77, replace = T)
# .test <- dplyr::mutate_if(.test, is.factor, as.character) %>% tibble::rowid_to_column()
# saveRDS(.test, "tests/testthat/rds/account_portfolio_test.rds")
# .test <- readRDS("tests/testthat/rds/account_portfolio_test.rds")
# #Create results table
# .res <- purrr::imap(list(char = "pchars", periods = "periods"), ~{
#   .period <- .x
#   out <- purrr::pmap(.test, ~{
#     .vars <- list(...)
#     message(paste0(.period, ": Rowid - ", .vars$rowid))
#     .e <- new.env()
#     withCallingHandlers(message = function(m) {
#       if (exists("msg", envir = .e, inherits = F)) {
#         assign("msg", append(.e$msg, m$message), envir = .e)
#       } else {
#         assign("msg", m$message, .e)
#       }
#     }, warning = function(m) {
#       if (exists("warn", envir = .e, inherits = F)) {
#         assign("warn", append(.e$msg, m$message), envir = .e)
#       } else {
#         assign("warn", m$message, .e)
#       }
#     }, expr = {
#       .out <- try(account_portfolio(.vars[[.period]], timeframe = .vars$timeframe, date_end = .vars$date_end, extended_hours = .vars$extended_hours))
#       browser(expr = class(.out) == "try-error")
#     })
#     if (exists("msg", envir = .e, inherits = F)) attr(.out, "msg") <- .e$msg
#     if (exists("warn", envir = .e, inherits = F)) attr(.out, "warn") <- .e$warn
#     attr(.out, "args") <- .vars
#     return(.out)
#   })
#   return(out)
# })
# saveRDS(.res, "tests/testthat/rds/account_portfolio_results.rds")

