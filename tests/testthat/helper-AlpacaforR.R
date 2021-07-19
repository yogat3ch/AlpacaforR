set.seed(1)
library(vcr)
.pkg_path <- tryCatch(c(pkgload::package_file(), "tests/testthat"), error = rlang::as_function(~{NULL}))
.log_path <- c(.pkg_path,
"vcr/vcr.log")
.log_path <- rlang::exec(file.path,
  !!!.log_path)
vcr::vcr_log_file(.log_path)
.redact <- Sys.getenv() %>% {.[stringr::str_subset(names(.),"(?:APCA)|(?:POLYGON)(?!\\-LIVE$)")]} %>% as.list()
vcr::vcr_configure(
  dir = dirname(.log_path),
  log_opts = list(file = .log_path, 
                  write_disk_path = dirname(.log_path)),
  filter_sensitive_data = .redact,
  warn_on_empty_cassette = FALSE
  
)
# name labels for t,q,lt,lq,ss endpoints
nms <- list(t = c("time", "x", "p", "s", "c", "i", "z"),
            q = c("time", "ax", "ap", "as", "bx", "bp", "bs", "c"),
            b = c("time", "open", "high", "low", "close", "volume"))
# For tests that require the market to be open
market_open <- TRUE
# internal.R prep ----
# Sat Dec 26 08:49:28 2020
#quick detection of timespan abbreviations:  Thu Mar 26 08:34:00 2020 ----
evar_reset <- function(...) {
    e <- rlang::env(
      .vn = list(
        symbol = "character",
        v = c("integer", "numeric", "character"),
        timeframe = c("factor", "character"),
        tf_num = c("integer", "numeric"),
        tf_order = "factor",
        multiplier = c("integer", "numeric"),
        from = c("character", "POSIXct", "Datetime", "Date", "NULL"),
        to = c("character", "POSIXct", "Datetime", "Date", "NULL"),
        after = c("character", "POSIXct", "Datetime", "Date", "NULL"),
        until = c("character", "POSIXct", "Datetime", "Date", "NULL"),
        limit = c("integer", "numeric", "NULL"),
        full = "logical",
        unadjusted = "logical",
        bounds = c("list"),
        cal = c("data.frame", "tibble"),
        tqs = "character",
        is_tqs = "logical"
      ),
  # Add appropriate variables to local environment
  symbol = "AMZN",
  after = NULL,
  until = NULL,
  full = FALSE,
  unadjusted = F,
  is_tqs = F,
  tqs =  c("lt", "tr", "lq", "qu", "ss")
  )
  rlang::env_bind(e, ...)
  return(e)
}
evar <- evar_reset()
.pre <- c(eb = "eb", sb = "sb", lb = "lb")
if (FALSE) {
  
  # Create a data.frame with multiple variations of typical calls for testing
  .test <- suppressWarnings({
    data.frame(
      multiplier = c(
        minute = 1,
        hour = 1,
        day = 1,
        week = 1,
        month = 1,
        quarter = 1,
        year = 1
      ),
      timeframe = c(
        "minute",
        "hour",
        "day",
        "week",
        "month",
        "quarter",
        "year"
      ),
      v = c(
        rep(2, 3),
        rep("p", 4)
      ),
      to = lubridate::ymd_hm("2020-04-03 16:30")
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
        eb = to - (4 * duration),
        sb = to - duration,
        lb = to - (.5 * duration)
      )
  })
  
  
  .bounds <- purrr::map(stats::setNames(.pre, paste0(.pre,"_bounds")), ~{
    .pre <- .x
    .bound <- purrr::pmap(.test, ~{
      # FINALLY figured out how to call arguments by name in pmap
      .vars <- rlang::dots_list(...)
      rlang::exec(evar_bind, !!!.vars, from = .vars[[.pre]])
      # Bounds are created inside of evar_bind
      list(bounds = evar$bounds,
           cal = evar$cal)
    })
    
  }) %>%
    tibble::as_tibble()
  .bounds <- dplyr::bind_cols(
  tibble::as_tibble(purrr::map_depth(setNames(.bounds, paste0(.pre, "_bounds")), 2, ~ {
    .x$bounds
  })),
  tibble::as_tibble(purrr::map_depth(setNames(.bounds, paste0(.pre, "_cal")), 2, ~ {
    .x$cal
  }))
  )
  
  
  
  .test2 <- dplyr::bind_cols(.test, .bounds)
  
  .url <- purrr::imap(stats::setNames(.pre, paste0(.pre,"_url")), ~{
    .pre <- .x
    .urls <- purrr::pmap(.test2, ~{
      .vars <- rlang::dots_list(...)
      rlang::exec(evar_bind, !!!.vars, bounds = .vars[[paste0(.pre, "_bounds")]], cal = .vars[[paste0(.pre, "_cal")]])
      .url <- bars_url(symbol = "AMZN", evar = evar)
      
    })
  }) %>% 
    tibble::as_tibble()
  
  
  .data <- purrr::map(stats::setNames(.pre, paste0(.pre,"_data")), ~{
    .pre <- .x
    .data <- purrr::pmap(dplyr::bind_cols(.test2, .url), ~{
      # FINALLY figured out how to call arguments by name in pmap
      .vars <- rlang::dots_list(...)
      rlang::exec(evar_bind, !!!.vars, bounds = .vars[[paste0(.pre, "_bounds")]], cal = .vars[[paste0(.pre, "_cal")]], .vars)
      .object <- bars_get(url = .vars[[paste0(.pre,"_url")]])
    })
  }) %>% 
    tibble::as_tibble()
  
  .test3 <- dplyr::bind_cols(.test2, .data)
  
  .complete <- purrr::imap(stats::setNames(.pre, paste0(.pre,"_complete")), ~ {
    .pre <- .x
    .complete <- purrr::pmap(.test3, ~ {
      .vars <- rlang::dots_list(...)
      evar <- evar_reset()
      rlang::exec(evar_bind, !!!.vars, bounds = .vars[[paste0(.pre, "_bounds")]], cal = .vars[[paste0(.pre, "_cal")]], full = TRUE, evar = evar)
      .object <- bars_complete(bars = .vars[[paste0(.pre, "_data")]], evar = evar)
    })
  }) %>%  
    tibble::as_tibble()
  test_market_data <- list(bars = dplyr::bind_cols(.test3, .complete))
  test_market_data$v1 <-  market_data(c("BYND"), v = 1, from = "2020-03-06", until = "2020-12-25", multiplier = 5, timeframe = "m", full = T)
  test_market_data$v2 <-  market_data(c("BYND"), v = 2, from = "2020-11-02", until = "2020-12-25", multiplier = 1, timeframe = "m", full = T)
  # Remove the Polygon API Key for security purposes
  test_market_data$bars[stringr::str_detect(names(test_market_data$bars), "data$|complete$")] <- dplyr::select(test_market_data$bars, contains("data"), contains("complete")) %>% 
    purrr::map(~{
      .x[4:7] <- purrr::map(.x[4:7], ~{
        .q <- get_query(.x)
        .q$url <- stringr::str_replace(.q$url, get_cred("POLYGON-KEY"), "POLYGON-KEY")
        attr(.x, "query") <- .q
        .x
      })
      .x
    })
  saveRDS(test_market_data, file.path(pkgload::package_file(), "tests/testthat/rds/test_market_data.rds"))
}

# market_data ----
# Sat Dec 26 15:05:40 2020
test_market_data <- readRDS(rlang::exec(file.path,
 !!!c(.pkg_path,
  "rds/test_market_data.rds")))


# get all files that start with "test" in the testthat directory
# .fn <- list.files("tests/testthat", pattern = "^test", full.names = T)
# tests <- purrr::map(.fn %>% setNames(nm = basename(.)), ~{
#   # read the file
#   .lines <- readLines(.x)
#   # get the line numbers of the test_that expressions.
#   # script assumes that all tests are written in the format:
#   # test_that("my test", {
#   #   [tests]...
#   # })
#   .b <- stringr::str_which(.lines, "test_that")
#   # map over the lines of the test_that expressions and find the ends of the expression by counting open/closed bracket pairs
#   .e <- purrr::map_int(.b, ~{
#     .l <- 0
#     # count open bracket/parentheticals
#     .pc <- 1
#     while (.pc != 0) {
#       .l <- .l + 1
#       .pc <- .pc + stringr::str_count(.lines[.x + .l], "\\{")
#       .pc <- .pc - stringr::str_count(.lines[.x + .l], "\\}")
#     }
#     .e <- as.integer(.x + .l)
#   })
#   # return that list of beginnings and ending line numbers
#   list(begin = .b, end = .e)
# })
# # if the number of beginnings and endings match
# if (all(purrr::map_lgl(tests, ~length(.x[[1]]) == length(.x[[2]])))) {
#   # map over the beginnigs and endings, and the file names
#   purrr::map2(tests, .fn, ~{
#     # read in the file
#     .lines <- readLines(.y)
#     # start a counter that will be used to determine how many lines have been added to the vector of the file lines (.count * 2), 1 line for the beginning of use_cassette expression and 1 line for the end.
#     .count <- 0
#     purrr::walk2(.x$begin, .x$end, ~{
#       # add use cassette
#       .test <- stringr::str_extract(.lines[.x + .count * 2], "\"[^\"]+\"")
#       # collapse the name into only it's alphanumeric characters and underscores (since vcr doesn't allow spaces/special chars)
#       .test <- paste0(stringr::str_extract_all(.test, "[:alnum:]+")[[1]], collapse = "_")
#       # if an appropriate name was not extracted bring up the browser
#       browser(expr = is.na(.test) || identical("NA", .test))
#       # if the use_that expression isnt already wrapped with use_cassette
#       if (stringr::str_detect(.lines[.x + .count * 2], "use_cassette", negate = T)) {
#         # append the use cassette expression one line above test_that
#         .lines <<- append(.lines, paste0('vcr::use_cassette(\"',.test,"\", {"), after = .x - 1 + .count * 2)
#         # and close it one line below the }) that closes test_That
#         .lines <<- append(.lines, "})", .y + .count * 2)
#       }
#       # increment the counter
#       .count <<- .count + 1
#     })
#     # show the entire document in the console
#     cat(.lines, sep = "\n")
#     # and pause with the browser to fix any issues before overwriting the file
#     browser()
#     write(.lines, .y)
#   })
# }
# 
# purrr::map2(tests, .fn %>% setNames(nm = basename(.)), ~{
#   .lines <- readLines(.y)
#   purrr::map_chr(.x$begin, ~{
#     .test <- stringr::str_extract(.lines[.x - 1], "(?<=vcr::use_cassette\\()[^,]+")
#     .fix <- paste0(stringr::str_extract_all(.test, "[:alnum:]+")[[1]], collapse = "_")
#     .replace <- paste0("vcr::use_cassette(\"",.fix,"\", {")
#     .lines[.x - 1] <<- .replace
#   })
#   cat(.lines, sep = "\n")
#   browser()
#   write(.lines, .y)
# })
  
  