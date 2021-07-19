#' @title arrange.tsymble_ts
#' @description An S3 method for arranging `tsymble_ts` object
#' @inheritParams dplyr::arrange
#' @inherit dplyr::arrange return
#' @export
arrange.tsymble_ts <- function(.data, ...) {
  arr_data <- dplyr::arrange(dplyr::as_tibble(.data), ...)
  update_meta(arr_data, .data, ordered = FALSE, interval = tsibble::interval(.data)) %>%
    build_tsymble(query = get_query(.data), symbol = get_sym(.data))
}

#' @title arrange.grouped_tsymble
#' @description An S3 method for arranging `tsymble_ts` object
#' @inheritParams dplyr::arrange
#' @inherit dplyr::arrange return
#' @export
arrange.grouped_tsymble <- arrange.tsymble_ts

#' @title select.tsymble_ts
#' @description An S3 method for selecting from a `tsymble_ts` object
#' @inheritParams dplyr::select
#' @inherit dplyr::select return
#' @export
select.tsymble_ts <- function(.data, ...) {
  loc <- tidyselect::eval_select(dplyr::expr(c(...)), .data) 
  data_cp <- .data
  names(data_cp)[loc] <- names(loc)
  bind_tsibble(NextMethod(), data_cp, position = "after") %>%
    build_tsymble(query = get_query(.data), symbol = get_sym(.data))
}

#' @title select.grouped_tsymble
#' @description An S3 method for selecting from a `tsymble_ts` object
#' @inheritParams dplyr::select
#' @inherit dplyr::select return
#' @export
select.grouped_tsymble <- select.tsymble_ts

#' @title transmute.tsymble_ts
#' @description An S3 method for transmuting a `tsymble_ts` object
#' @inheritParams dplyr::transmute
#' @inherit dplyr::transmute return
#' @export
transmute.tsymble_ts <- function(.data, ...) {
  bind_tsibble(NextMethod(), .data, position = "before") %>%
    build_tsymble(query = get_query(.data), symbol = get_sym(.data))
}

#' @title transmute.grouped_tsymble
#' @description An S3 method for transmuting a `tsymble_ts` object
#' @inheritParams dplyr::transmute
#' @inherit dplyr::transmute return
#' @export
transmute.grouped_tsymble <- transmute.tsymble_ts

#' @title group_by.tsymble_ts
#' @description An S3 method for transmuting a `tsymble_ts` object
#' @inheritParams dplyr::group_by
#' @inherit dplyr::group_by return
#' @export
group_by.tsymble_ts <- function(.data, ..., .add = FALSE,
                             .drop = dplyr::group_by_drop_default(.data)) {
  lst_quos <- dplyr::enquos(..., .named = TRUE)
  grp_vars <- names(lst_quos)
  if (.add) grp_vars <- generics::union(dplyr::group_vars(.data), grp_vars)
  if (purrr::is_empty(grp_vars)) return(.data)

  index <- tsibble::index_var(.data)
  if (index %in% grp_vars) {
    rlang::abort(c(
      sprintf("Column `%s` (index) can't be a grouping variable for a tsibble.", index),
      i = "Did you mean `index_by()`?"))
  }

  grp_key <- identical(grp_vars, tsibble::key_vars(.data)) &&
    identical(.drop, tsibble::key_drop_default(.data))
  if (grp_key) {
    grped_tbl <- dplyr::new_grouped_df(.data, groups = tsibble::key_data(.data))
  } else {
    grped_tbl <- NextMethod()
  }
  tsibble::build_tsibble(
    grped_tbl,
    key = !!tsibble::key_vars(.data),
    key_data = if (grp_key) tsibble::key_data(.data) else NULL,
    index = !!tsibble::index(.data), index2 = !!tsibble::index2(.data),
    ordered = tsibble::is_ordered(.data), interval = tsibble::interval(.data), validate = FALSE
  ) %>% 
    build_tsymble(query = get_query(.data), symbol = get_sym(.data))
}

#' @title ungroup.tsymble_ts
#' @description An S3 method for ungrouping a `tsymble_ts` object
#' @inheritParams dplyr::ungroup
#' @inherit dplyr::ungroup return
#' @export
ungroup.tsymble_ts <- function(x, ...) {
  tbl <- dplyr::ungroup(dplyr::as_tibble(x))
  tsibble::build_tsibble(
    tbl,
    key_data = tsibble::key_data(x), index = !!tsibble::index(x),
    ordered = tsibble::is_ordered(x), interval = tsibble::interval(x), validate = FALSE
  ) %>% 
    build_tsymble(query = get_query(x), symbol = get_sym(x))
}

#' @title dplyr_row_slice.tsymble_ts
#' @description An S3 method for slicing a `tsymble_ts` object
#' @inheritParams dplyr::dplyr_row_slice
#' @inherit dplyr::dplyr_row_slice return
#' @inheritDotParams dplyr::dplyr_row_slice
#' @export
dplyr_row_slice.tsymble_ts <- function(data, i, ...) {
  build_tsymble(NextMethod(), query = get_query(data), symbol = get_sym(data))
}

#' @title dplyr_row_slice.grouped_tsymble
#' @description An S3 method for slicing a `tsymble_ts` object
#' @inheritParams dplyr::dplyr_row_slice
#' @inherit dplyr::dplyr_row_slice return
#' @inheritDotParams dplyr::dplyr_row_slice
#' @export
dplyr_row_slice.grouped_tsymble <- dplyr_row_slice.tsymble_ts

#' @title dplyr_col_modify.tsymble_ts
#' @description An S3 method for modifying a `tsymble_ts` object
#' @inheritParams dplyr::dplyr_col_modify
#' @inherit dplyr::dplyr_col_modify return
#' @export
dplyr_col_modify.tsymble_ts <- function(data, cols) {
  build_tsymble(NextMethod(), query = get_query(data), symbol = get_sym(data))
}

#' @title dplyr_col_modify.grouped_tsymble
#' @description An S3 method for modifying a `tsymble_ts` object
#' @inheritParams dplyr::dplyr_col_modify
#' @inherit dplyr::dplyr_col_modify return
#' @export
dplyr_col_modify.grouped_tsymble <- dplyr_col_modify.tsymble_ts

#' @title dplyr_reconstruct.tsymble_ts
#' @description An S3 method for reconstructing a `tsymble_ts` object
#' @inheritParams dplyr::dplyr_reconstruct
#' @inherit dplyr::dplyr_reconstruct return
#' @export
dplyr_reconstruct.tsymble_ts <- function(data, template) {
  build_tsymble(NextMethod(), query = get_query(data), symbol = get_sym(data))
}
#' @title dplyr_reconstruct.grouped_tsymble
#' @description An S3 method for reconstructing a `tsymble_ts` object
#' @inheritParams dplyr::dplyr_reconstruct
#' @inherit dplyr::dplyr_reconstruct return
#' @export
dplyr_reconstruct.grouped_tsymble <- dplyr_reconstruct.tsymble_ts

#' @noRd
#' @export
summarise.tsymble_ts <- function(.data, ..., .groups = NULL) { 
  
  idx_var <- tsibble::index_var(.data)
  out <- summarise(tsibble::as_tsibble(.data), ..., .groups = .groups)
  
  # If the distribution is lost, return a tsibble
  if(!(idx_var %in% names(.data))) {
    if(!vctrs::vec_is(out[[idx_var]], vctrs::vec_ptype(.data))){
      out <- .data
    }
  } else {
    out <- build_tsymble(out, query = get_query(data), symbol = get_sym(data))
  }
  return(out)
}

#' @noRd
#' @export
summarise.grouped_tsymble <- summarise.tsymble_ts