# .this <- pkgload::package_file("R/aaa_imports.R")
# .lines <- readLines(.this)
# write(stringr::str_subset(.lines, "^#' @import", negate = TRUE), .this)
# cat(do.call(c, purrr::map(list.files("R", full.names = TRUE), readLines)), file = pkgload::package_file("temp_scripts.R"), sep = "\n")
# capture.output(file = .this, sinew::make_import(pkgload::package_file("temp_scripts.R"), desc_loc = pkgload::package_file("DESCRIPTION"), cut = 12), append = TRUE)
#file.remove(pkgload::package_file("temp_scripts.R"))


# R/aaa_imports.R


# R/Account.R
#' @importFrom httr GET PATCH
#' @importFrom purrr map_if map_lgl compact map iwalk
#' @importFrom lubridate as_datetime duration weeks days `.__T__-:base` `.__T__+:base` as_date origin
#' @importFrom rlang env_get_list is_missing is_empty abort warn
#' @importFrom stringr str_extract str_detect regex
#' @importFrom tibble as_tibble
#' @importFrom dplyr mutate across

# R/Assets.R
#' @importFrom rlang dots_list is_empty warn
#' @importFrom httr GET
#' @importFrom purrr map_dfr
#' @importFrom tibble tibble as_tibble

# R/Calendar.R
#' @importFrom purrr map_lgl map iwalk
#' @importFrom lubridate today wday as_date interval ymd_hm
#' @importFrom httr GET
#' @importFrom dplyr mutate across starts_with select
#' @importFrom stringr str_sub

# R/Clock.R
#' @importFrom lubridate `.__T__-:base`  as_datetime is.POSIXct with_tz hours force_tz
#' @importFrom httr GET
#' @importFrom purrr map_if

# R/dplyr-verbs.R
#' @importFrom dplyr arrange as_tibble expr group_by_drop_default enquos group_vars new_grouped_df ungroup group_by dplyr_col_modify
#' @importFrom tsibble interval index_var key_vars key_drop_default key_data build_tsibble index index2 is_ordered as_tsibble
#' @importFrom tidyselect eval_select
#' @importFrom generics union
#' @importFrom purrr is_empty
#' @importFrom rlang abort
#' @importFrom vctrs vec_is vec_ptype

# R/FirstRun.R
#' @importFrom usethis proj_get
#' @importFrom purrr compact map_dbl iwalk
#' @importFrom rlang set_names list2 is_empty
#' @importFrom stringr str_which str_detect
#' @importFrom glue glue
#' @importFrom cli cat_line col_blue col_white

# R/Helpers.R
#' @importFrom lubridate year parse_date_time with_tz years origin as_date as_datetime
#' @importFrom purrr map some map_if map_chr
#' @importFrom dplyr between
#' @importFrom rlang cnd_muffle exec dots_list
#' @importFrom tsibble yearweek yearmonth yearquarter

# R/internal.R
#' @importFrom rlang `%||%` `%|%` `%@%` caller_env dots_list is_empty env_bind list2 abort dots_n as_function
#' @importFrom purrr iwalk when compact
#' @importFrom lubridate duration with_tz parse_date_time
#' @importFrom httr add_headers build_url content
#' @importFrom stringr str_replace str_replace_all
#' @importFrom utils URLdecode
#' @importFrom jsonlite fromJSON

# R/Market_Data.R
#' @import rlang
#' @import purrr
#' @import lubridate
#' @importFrom glue glue
#' @importFrom stats setNames
#' @importFrom stringr str_replace regex
#' @importFrom httr parse_url GET build_url
#' @importFrom tsibble new_interval tsibble index_var count_gaps index interval
#' @importFrom tibble as_tibble tibble
#' @importFrom dplyr select everything mutate across filter between slice_max
#' @importFrom tidyselect any_of
#' @importFrom utils head tail

# R/Orders.R
#' @importFrom httr GET POST PATCH DELETE
#' @importFrom dplyr filter mutate across ends_with
#' @importFrom purrr modify_depth compact map imap_chr imap keep
#' @importFrom rlang list2 warn `%||%` abort is_empty env_bind caller_env
#' @importFrom jsonlite toJSON
#' @importFrom lubridate ymd_hms
#' @importFrom tibble as_tibble

# R/Polygon.R
#' @importFrom lubridate as_date floor_date
#' @importFrom rlang abort is_empty list2 expr warn exec dots_list eval_bare call2
#' @importFrom purrr map map_depth pmap map_chr imap_dfc modify_if
#' @importFrom glue glue glue_data
#' @importFrom stringr str_detect str_match_all
#' @importFrom httr GET
#' @importFrom dplyr bind_cols rename across mutate
#' @importFrom tidyselect ends_with
#' @importFrom tibble as_tibble

# R/Positions.R
#' @importFrom httr DELETE GET parse_url
#' @importFrom rlang expr warn
#' @importFrom purrr map_dfr map when pwalk
#' @importFrom dplyr mutate across bind_cols rename
#' @importFrom tidyselect ends_with
#' @importFrom lubridate as_datetime with_tz
#' @importFrom stringr str_extract

# R/tsibble-dependencies.R
#' @importFrom rlang is_null sym is_empty is_bare_logical is_bare_numeric vec_duplicate_any
#' @importFrom dplyr grouped_df new_grouped_df summarise is_grouped_ts group_vars group_by_drop_default left_join group_data
#' @importFrom tibble as_tibble
#' @importFrom vctrs vec_duplicate_any list_of vec_unique vec_size vec_duplicate_detect
#' @import tsibble
#' @importFrom purrr map_lgl

# R/tsymble.R
#' @importFrom tsibble new_tsibble build_tsibble new_interval is_grouped_ts is_tsibble index_var as_tsibble
#' @importFrom tibble tibble as_tibble
#' @importFrom dplyr distinct mutate across bind_rows
#' @importFrom rlang sym dots_list list2 is_empty
#' @importFrom lubridate year
#' @importFrom purrr map map_lgl flatten

# R/Watchlist.R
#' @importFrom purrr when compact map list_merge map_at
#' @importFrom rlang exec is_empty
#' @importFrom httr GET POST PUT DELETE build_url parse_url
#' @importFrom jsonlite toJSON
#' @importFrom dplyr select
#' @importFrom tibble as_tibble

# R/websocket_reprex.R
#' @importFrom rlang warn

# R/Websockets.R
#' @importFrom purrr map walk accumulate vec_depth compact map_if some map_lgl imap when list_modify
#' @importFrom jsonlite toJSON fromJSON
#' @import rlang
#' @importFrom glue glue_data
#' @importFrom httr parse_url
#' @importFrom stringr str_split str_extract str_detect str_subset str_to_title
#' @importFrom crayon yellow magenta silver underline
#' @importFrom tibble tibble_row
#' @importFrom cli cli_text col_red
#' @importFrom dplyr bind_rows
#' @importFrom utils memory.size memory.limit
#' @importFrom R6 R6Class
#' @importFrom websocket WebSocket