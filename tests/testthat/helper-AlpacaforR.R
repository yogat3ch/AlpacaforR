library("vcr")
if (basename(getwd()) == "AlpacaforR") {
  vcr::vcr_log_file("tests/testthat/vcr.log")
  invisible(vcr::vcr_configure(dir = "tests/testthat/vcr", log = TRUE, log_opts = list(file = "tests/testthat/vcr.log"), write_disk_path = "tests/testthat/vcr"))
} else {
  vcr::vcr_log_file("vcr/vcr.log")
  invisible(vcr::vcr_configure(dir = "vcr", log = TRUE))
}

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
  
  