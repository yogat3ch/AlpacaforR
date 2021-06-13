
#' @inherit dplyr::arrange
#' @export
arrange.tsymble_ts <- function(.data, ...) {
  arr_data <- dplyr::arrange(dplyr::as_tibble(.data), ...)
  update_meta(arr_data, .data, ordered = FALSE, interval = tsibble::interval(.data)) %>%
    build_tsymble(query = get_query(.data), symbol = get_sym(.data))
}

#' @inherit dplyr::arrange
#' @export
arrange.grouped_tsymble <- arrange.tsymble_ts

#' @inherit dplyr::select
#' @export
select.tsymble_ts <- function(.data, ...) {
  loc <- tidyselect::eval_select(dplyr::expr(c(...)), .data) 
  data_cp <- .data
  names(data_cp)[loc] <- names(loc)
  bind_tsibble(NextMethod(), data_cp, position = "after") %>%
    build_tsymble(query = get_query(.data), symbol = get_sym(.data))
}

#' @inherit dplyr::select
#' @export
select.grouped_tsymble <- select.tsymble_ts

#' @inherit dplyr::transmute
#' @export
transmute.tsymble_ts <- function(.data, ...) {
  bind_tsibble(NextMethod(), .data, position = "before") %>%
    build_tsymble(query = get_query(.data), symbol = get_sym(.data))
}

#' @inherit dplyr::transmute
#' @export
transmute.grouped_tsymble <- transmute.tsymble_ts


#' @inherit dplyr::group_by
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

#' @inherit dplyr::ungroup
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

#' @inherit dplyr::dplyr_row_slice
#' @inheritDotParams dplyr::dplyr_row_slice
#' @export
dplyr_row_slice.tsymble_ts <- function(data, i, ...) {
  build_tsymble(NextMethod(), query = get_query(data), symbol = get_sym(data))
}

#' @inherit dplyr::dplyr_row_slice
#' @inheritDotParams dplyr::dplyr_row_slice
#' @export
dplyr_row_slice.grouped_tsymble <- dplyr_row_slice.tsymble_ts

#' @inherit dplyr::dplyr_row_slice
#' @inheritDotParams dplyr::dplyr_col_modify
#' @export
dplyr_col_modify.tsymble_ts <- function(data, cols) {
  build_tsymble(NextMethod(), query = get_query(data), symbol = get_sym(data))
}

#' @inherit dplyr::dplyr_col_modify
#' @inheritDotParams dplyr::dplyr_col_modify
#' @export
dplyr_col_modify.grouped_tsymble <- dplyr_col_modify.tsymble_ts

#' @inherit dplyr::dplyr_reconstruct
#' @export
dplyr_reconstruct.tsymble_ts <- function(data, template) {
  build_tsymble(NextMethod(), query = get_query(data), symbol = get_sym(data))
}

#' @inherit dplyr::dplyr_reconstruct
#' @export
dplyr_reconstruct.grouped_tsymble <- dplyr_reconstruct.tsymble_ts

#' @inherit dplyr::summarise
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
#' @inherit dplyr::summarise
#' @export
summarise.grouped_tsymble <- summarise.tsymble_ts