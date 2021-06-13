duplicated_key_index <- function(data, key, index, key_data = NULL) {
  if (rlang::is_null(key_data)) {
    keyed_data <- dplyr::grouped_df(tibble::as_tibble(data), key, drop = TRUE)
  } else {
    keyed_data <- dplyr::new_grouped_df(data, groups = key_data)
  }
  res <- dplyr::summarise(keyed_data, !!"zzz" := vctrs::vec_duplicate_any(!!rlang::sym(index)))
  any(res$zzz > 0)
}

retain_tsibble <- function(data, key, index) {
  is_dup <- duplicated_key_index(data, key, index)
  if (is_dup) {
    abort(c(
      "The result is not a valid tsibble.",
      i = "Do you need `as_tibble()` to work with data frame?"))
  }
  data
}

has_any_key <- function(j, x) {
  key_vars <- tsibble::key_vars(x)
  any(key_vars %in% j)
}




is_key_dropped <- function(x) {
  if (!tsibble::is_grouped_ts(x)) {
    tsibble::key_drop_default(x)
  } else {
    key_vars <- tsibble::key_vars(x)
    grp_vars <- dplyr::group_vars(x)
    dplyr::group_by_drop_default(x) && any(is.element(key_vars, grp_vars))
  }
}

remove_key <- function(.data, .vars) {
  sel_key <- c(.vars[.vars %in% tsibble::key_vars(.data)], ".rows")
  attr(.data, "key") <- tsibble::key_data(.data)[sel_key]
  .data
}

update_meta <- function(new, old, ordered = TRUE, interval = TRUE,
                        validate = FALSE) {
  if (validate) {
    retain_tsibble(new, key = tsibble::key_vars(old), index = tsibble::index(old))
    validate <- FALSE
  }
  tsibble::build_tsibble(new,
                key = !!tsibble::key_vars(old), index = !!tsibble::index(old), index2 = !!tsibble::index2(old),
                ordered = ordered, interval = interval, validate = validate,
                .drop = is_key_dropped(old)
  )
}

update_meta2 <- function(new, old, ordered = TRUE, interval = TRUE,
                         validate = FALSE) {
  old_key <- select(tsibble::key_data(old), !!!tsibble::key(old))
  if (rlang::is_empty(old_key)) {
    return(update_meta(
      new, old,
      ordered = ordered, interval = interval, validate = validate
    ))
  }
  grped_df <- dplyr::grouped_df(new, tsibble::key_vars(old), drop = tsibble::key_drop_default(old))
  new_key <- dplyr::left_join(old_key, dplyr::group_data(grped_df), by = tsibble::key_vars(old))
  null_lgl <- purrr::map_lgl(new_key[[".rows"]], rlang::is_null)
  new_key[[".rows"]][null_lgl] <- vctrs::list_of(integer())
  ts <- tsibble::build_tsibble(new,
                key_data = new_key, index = !!tsibble::index(old), index2 = !!tsibble::index2(old),
                ordered = ordered, interval = interval, validate = validate
  ) %>% 
    build_tsymble(symbol = get_sym(new), query = merge_query(list(new, old)))
}

is_index_null <- function(x) {
  if (is.null(x %@% "index")) {
    abort("The `index` has been dropped somehow. Please reconstruct tsibble.")
  }
}

has_index <- function(j, x) {
  is_index_null(x)
  index <- c(tsibble::index_var(x), tsibble::index2_var(x))
  any(index %in% j)
}

remove_key <- function (.data, .vars) 
{
  sel_key <- c(.vars[.vars %in% tsibble::key_vars(.data)], ".rows")
  attr(.data, "key") <- tsibble::key_data(.data)[sel_key]
  .data
}


# bind_tsymble <- function(data, template, position = c("before", "after")) {
#   data <- tibble::as_tibble(data)
#   data_cols <- names(data)
#   key_vars <- setdiff(key_vars(template), data_cols)
#   key_data <- vctrs::vec_unique(select(tsibble::key_data(template), key_vars))
#   if (vctrs::vec_size(key_data) == 1) {
#     template <- remove_key(template, setdiff(tsibble::key_vars(template), key_vars))
#   }
#   tsbl_vars <- setdiff(c(tsibble::index_var(template), tsibble::key_vars(template)), data_cols)
#   if (position == "before") {
#     res <- bind_cols(tsibble::as_tibble(template)[tsbl_vars], data)
#   } else {
#     res <- bind_cols(data, template[tsbl_vars])
#   }
#   update_meta(res, template,
#               ordered = tsibble::is_ordered(template), interval = tsibble::interval(template)) %>%
#     build_tsymble(query = get_query(data), symbol = get_sym(data))
# }

is_ascending <- Negate(is.unsorted)

validate_order <- function(x) {
  if (rlang::is_bare_logical(x)) {
    any(x)
  } else if (rlang::is_bare_numeric(x) && all(x < 0)) {
    TRUE
  } else if (vctrs::vec_duplicate_any(x) > 0) {
    abort(sprintf("Duplicated indices: %s", comma(x[vctrs::vec_duplicate_detect(x)])))
  } else {
    is_ascending(x, na.rm = TRUE, strictly = TRUE)
  }
}