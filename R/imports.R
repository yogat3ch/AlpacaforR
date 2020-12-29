# .this <- pkgload::package_file("R/aaa_imports.R")
# .lines <- readLines(.this)
# write(stringr::str_subset(.lines, "^#' @import", negate = TRUE), .this)
# cat(do.call(c, purrr::map(list.files("R", full.names = TRUE), readLines)), file = pkgload::package_file("temp_scripts.R"), sep = "\n")
# capture.output(file = .this, sinew::make_import(pkgload::package_file("temp_scripts.R"), desc_loc = pkgload::package_file("DESCRIPTION"), cut = 12), append = TRUE)
#file.remove(pkgload::package_file("temp_scripts.R"))

 

#' @importFrom stringr str_subset str_extract str_detect regex str_sub str_replace str_replace_all str_match_all str_split str_remove
#' @import purrr
#' @importFrom httr GET PATCH add_headers parse_url build_url content POST DELETE PUT
#' @import lubridate
#' @import rlang
#' @importFrom tibble as_tibble tibble add_row
#' @import dplyr
#' @importFrom glue glue glue_data
#' @importFrom tsibble yearweek yearmonth yearquarter irregular new_interval index interval index_var count_gaps as_tsibble
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom utils head tail URLdecode object.size memory.size
#' @importFrom stats update setNames
#' @importFrom tidyselect all_of ends_with
#' @importFrom methods setOldClass
#' @importFrom websocket WebSocket

