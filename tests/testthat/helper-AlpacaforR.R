library("vcr")
invisible(vcr::vcr_configure(dir = "vcr", log = TRUE))
# .fn <- list.files("tests/testthat", pattern = "^test", full.names = T)
# tests <- purrr::map(.fn %>% setNames(nm = basename(.)), ~{
#   .lines <- readLines(.x)
#   .b <- stringr::str_which(.lines, "test_that")
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
#   list(begin = .b, end = .e)
# })
# if (all(purrr::map_lgl(tests, ~length(.x[[1]]) == length(.x[[2]])))) {
#   purrr::map2(tests[8:11], .fn[8:11], ~{
#     .lines <- readLines(.y)
#     #for each of the files
#     .count <- 0
#     purrr::walk2(.x$begin, .x$end, ~{
#       # add use cassette
#       .test <- stringr::str_extract(.lines[.x + .count * 2], "\"[^\"]+\"")
#       browser(expr = is.na(.test))
#       if (stringr::str_detect(.lines[.x + .count * 2], "use_cassette", negate = T)) {
#         .lines <<- append(.lines, paste0('vcr::use_cassette(',.test,", {"), after = .x - 1 + .count * 2)
#         .lines <<- append(.lines, "})", .y + .count * 2)
#       }
#       .count <<- .count + 1
#     })
#     cat(.lines, sep = "\n")
#     browser()
#     write(.lines, .y)
#   })
# }

