

#' @export
arrange.tsymble_ts <- function(.data, ...) {
  arr_data <- dplyr::arrange(dplyr::as_tibble(.data), ...)
  update_meta(arr_data, .data, ordered = FALSE, interval = tsibble::interval(.data)) %>%
    build_tsymble(query = get_query(.data), symbol = get_sym(.data))
}

#' @export
arrange.grouped_tsymble <- arrange.tsymble_ts


#' @export
select.tsymble_ts <- function(.data, ...) {
  loc <- tidyselect::eval_select(dplyr::expr(c(...)), .data) 
  data_cp <- .data
  names(data_cp)[loc] <- names(loc)
  bind_tsibble(NextMethod(), data_cp, position = "after") %>%
    build_tsymble(query = get_query(.data), symbol = get_sym(.data))
}

#' @export
select.grouped_tsymble <- select.tsymble_ts


#' @export
transmute.tsymble_ts <- function(.data, ...) {
  bind_tsibble(NextMethod(), .data, position = "before") %>%
    build_tsymble(query = get_query(.data), symbol = get_sym(.data))
}

#' @export
transmute.grouped_tsymble <- transmute.tsymble_ts



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



#' distinct.tsymble_ts <- function(.data, ...) {
#'   dplyr::distinct(dplyr::as_tibble(.data), ...)
#'   #TODO Didn't we already redefine this?
#' }
#' 
#' #' @export
#' dplyr_row_slice.tbl_ts <- function(data, i, ..., preserve = FALSE) {
#'   tbl <- dplyr::as_tibble(data)
#'   loc_df <- dplyr::summarise(tbl, !!".loc" := rlang::list2(i))
#'   ascending <- all(purrr::map_lgl(loc_df[[".loc"]], validate_order))
#'   res <- dplyr::dplyr_row_slice(tbl, i, ..., preserve = preserve)
#'   if (preserve) {
#'     update_meta2(res, data, ordered = ascending, interval = tsibble::interval(data))
#'   } else {
#'     update_meta(res, data, ordered = ascending, interval = tsibble::interval(data))
#'   }
#' }
#' 
#' #' @export
#' dplyr_row_slice.grouped_ts <- dplyr_row_slice.tbl_ts
#' 
#' #' @export
#' dplyr_col_modify.tbl_ts <- function(data, cols) {
#'   res <- dplyr::dplyr_col_modify(dplyr::as_tibble(data), cols)
#'   idx_chr <- tsibble::index_var(data)
#'   if (testthat::is_false(idx_chr %in% names(res))) { # index has been removed
#'     rlang::abort(c(
#'       "Column `%s` (index) can't be removed for a tsibble.",
#'       i = sprintf("Do you need `as_tibble()` to work with data frame?"), idx_chr))
#'   }
#'   
#'   vec_names <- names(cols)
#'   # either key or index is present in `cols`
#'   # suggests that the operations are done on these variables
#'   # validate = TRUE to check if tsibble still holds
#'   val_idx <- has_index(vec_names, data)
#'   if (val_idx) interval <- TRUE else interval <- tsibble::interval(data)
#'   
#'   val_key <- has_any_key(vec_names, data)
#'   if (val_key) {
#'     key_vars <- generics::setdiff(names(res), tsibble::measured_vars(data))
#'     data <- remove_key(data, key_vars)
#'   }
#'   
#'   validate <- val_idx || val_key
#'   if (validate) {
#'     res <- retain_tsibble(res, tsibble::key_vars(data), tsibble::index(data))
#'   }
#'   tsibble::build_tsibble(
#'     res,
#'     key = !!tsibble::key_vars(data),
#'     key_data = if (val_key) NULL else tsibble::key_data(data), index = !!tsibble::index(data),
#'     index2 = !!tsibble::index2(data), ordered = tsibble::is_ordered(data), interval = interval,
#'     validate = FALSE, .drop = is_key_dropped(data)
#'   )
#' }
#' 
#' #' @export
#' dplyr_col_modify.grouped_ts <- dplyr_col_modify.tbl_ts
#' 
#' #' @export
#' dplyr_reconstruct.tbl_ts <- function(data, template) {
#'   template <- rename_join_tsibble(data, template)
#'   update_meta(data, template,
#'               ordered = NULL, interval = tsibble::is_regular(template),
#'               validate = TRUE)
#' }
#' 
#' #' @export
#' dplyr_reconstruct.grouped_ts <- function(data, template) {
#'   data <- dplyr::grouped_df(data, dplyr::group_vars(template))
#'   dplyr_reconstruct.tbl_ts(data, template)
#' }
#' 
#' rename_join_tsibble <- function(data, template) {
#'   nm <- names(template)
#'   key_idx_pos <- vctrs::vec_match(c(tsibble::index_var(template), tsibble::key_vars(template)), nm)
#'   names(template)[key_idx_pos] <- names(data)[key_idx_pos]
#'   template
#' }


#' @export
dplyr_row_slice.tsymble_ts <- function(data, i, ..., preserve = FALSE) {
  build_tsymble(NextMethod(), query = get_query(data), symbol = get_sym(data))
}

#' @export
dplyr_row_slice.grouped_tsymble <- dplyr_row_slice.tsymble_ts


#' @export
dplyr_col_modify.tsymble_ts <- function(data, cols) {
  build_tsymble(NextMethod(), query = get_query(data), symbol = get_sym(data))
}

#' @export
dplyr_col_modify.grouped_tsymble <- dplyr_col_modify.tsymble_ts


#' @export
dplyr_reconstruct.tsymble_ts <- function(data, template) {
  build_tsymble(NextMethod(), query = get_query(data), symbol = get_sym(data))
}

#' @export
dplyr_reconstruct.grouped_tsymble <- dplyr_reconstruct.tsymble_ts


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

#' @export
summarise.grouped_tsymble <- summarise.tsymble_ts