`%>%` <- magrittr::`%>%`
set.seed(1)
library(vcr)
.log_path <- file.path(pkgload::package_file(), "tests/testthat/vcr/vcr.log")
vcr::vcr_log_file(.log_path)
.redact <- Sys.getenv() %>% {.[stringr::str_subset(names(.),"APCA")][-1]} %>% as.list()
vcr::vcr_configure(
  dir = dirname(.log_path),
  log_opts = list(file = .log_path, write_disk_path = dirname(.log_path)),
  filter_sensitive_data = .redact,
  filter_request_headers = .redact,
  filter_response_headers = .redact
)
# internal.R prep ----
# Sat Dec 26 08:49:28 2020
#quick detection of timespan abbreviations:  Thu Mar 26 08:34:00 2020 ----
evar <- rlang::env(
.vn = list(
  ticker = "character",
  v = c("integer", "numeric"),
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
  bounds = c("Date", "Datetime", "POSIXct"),
  cal = c("data.frame", "tibble")
),
# Create ordered factor or timeframe options
tf_order = purrr::map_chr(list(
  m = c("m", "min", "minute"),
  h = c("h", "hour"),
  d = c("d", "day"),
  w = c("w", "week"),
  M = c("M", "mo", "month"),
  q = c("q", "quarter"),
  y = c("y", "year")
), tail, 1) %>% {factor(., levels = .)},
# Add appropriate variables to local environment
ticker = "AMZN",
v = 2,
after = NULL,
until = NULL,
full = T,
unadjusted = F
)

# Create a data.frame with multiple variations of typical calls for testing
# suppressWarnings({
#   .test <- data.frame(
#     multiplier = c(
#       minute = c(1, 5, 15),
#       hour = 1,
#       day = 1,
#       week = c(1, 2, 4),
#       month = c(1, 2, 3),
#       quarter = 1,
#       1
#     ),
#     timeframe = c(
#       rep("minute", 3),
#       "hour",
#       "day",
#       rep("week", 3),
#       rep("month", 3),
#       "quarter",
#       "year"
#     ),
#     to = lubridate::ymd_hm("2020-04-03 13:05")
#   ) %>%
#     dplyr::rowwise() %>%
#     dplyr::mutate(duration = lubridate::duration(
#       multiplier,
#       ifelse(
#         as.character(timeframe) == "quarter",
#         "month",
#         as.character(timeframe)
#       )
#     )) %>%
#     dplyr::mutate(
#       early_bound = to - (4 * duration),
#       single_bound = to - duration,
#       late_bound = to - (.5 * duration)
#     )
# })

# Load the completed/checked data.frame for comparison
test_internal <- readRDS(file.path(pkgload::package_file(), "tests/testthat/rds/test_internal.rds"))


# market_data ----
# Sat Dec 26 15:05:40 2020
test_market_data <- readRDS(file.path(pkgload::package_file(), "tests/testthat/rds/test_market_data.rds"))


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
  
  