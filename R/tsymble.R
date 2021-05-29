

tsymble <- tsibble::new_tsibble(tsibble::build_tsibble(tibble::tibble(time = Sys.Date() + 0:3), index = "time", ordered = TRUE, interval = tsibble::new_interval(day = 1), index2 = "time"), symbol = character(), query = list(), class = "tsymble")


build_tsymble <- function(x, symbol, query, validate = TRUE) {
  if(tsibble::is_grouped_ts(x)){
    tsymble <- structure(x, class = c("grouped_tsymble", "grouped_ts", "grouped_df", 
                                  "tsymble_ts", "tbl_ts", "tbl_df", "tbl", "data.frame"),
                     symbol = symbol, query = query)
  } else {
    tsymble <- tsibble::new_tsibble(
      x, symbol = symbol, query = query,
      class = "tsymble_ts")
  }
  
  if (validate) validate_tsymble(tsymble)
  tsymble
}




#' @title Construct a tsybble
#' @description \lifecycle{experimental}
#' Constructs a \code{tsymble}. A \code{\link[tsibble]{tsibble}} with ticker symbol, and query metadata attributes. 
#' @param index \code{(character)} index column name
#' @param symbol \code{(character)} Ticker symbol
#' @param query \code{(list)} Query metadata
#' @inheritParams tsibble::as_tsibble
#' @inheritDotParams tsibble::build_tsibble  -index -x
#' @export

as_tsymble <-
  function(x,
           index,
           symbol = get_sym(x),
           query = get_query(x),
           validate = TRUE,
           ...) {
    
  force(query)
  # tsibble must have distinct index
  if (NROW(x) > 2) {
    dx <- dplyr::distinct(x, !!rlang::sym(index), .keep_all = TRUE)
  } else {
    dx <- x
  }
  
  if (isTRUE(rlang::dots_list(...)$interval$year > 0)) {
    # eliminates (can't obtain interval due to the mismatched index class)
    dx <- dplyr::mutate(dx, dplyr::across(index, lubridate::year))
  }
  
  if (missing(index) && tsibble::is_tsibble(x)) {
    index <- tsibble::index_var(x)
  } else {
    index <- grep("(?:^date$)|(?:^time$)", names(x), value = TRUE, perl = TRUE)
  }
  
  
    
  dx <- do.call(tsibble::build_tsibble, rlang::list2(
    dx,
    index = index,
    ...
  ))
  build_tsymble(
    dx,
    symbol = symbol,
    query = query,
    validate = validate
  )
}

#' @title Validate a `tsymble`
#' @param x \code{(tsymble)}

validate_tsymble <- function(x) {
  .classes <- c("tsymble_ts", "tbl_ts", "tbl_df", "tbl", "data.frame")
  # correct classes
  if (!all(class(x) %in% .classes)) abort("missing class: ", .classes[!.classes %in% class(x)])
  
  # query attribute is list
  if (!is.list(get_query(x)) && !is.null(get_query(x))) abort("query attribute must be a list.")
  
  # symbol is character
  if (!is.character(get_sym(x)) || rlang::is_empty(get_sym(x))) abort("symbol attribute must be a length one character vector")
  
}

#' @title Gather and flatten querys
#' @description Receives multiple tsymble's to be bound together and combines the query's as individual list items
#' @param . \code{(list)} of tsymbles
#' @keywords Internal

merge_query <- function(.) {
  .q <- purrr::map(., get_query) 
  .ql <- purrr::map_lgl(.q, ~!"ts" %in% names(.x))
  if (any(.ql)) {
    .qn <- .q[!.ql]
    for (i in which(.ql)) {
      .qn <- append(.qn, purrr::flatten(.q[i]))
    }
    .q <- .qn
  }
  .q
}




#' @inherit dplyr::bind_rows

bind_rows <- function (..., .id = NULL) {
  . <- purrr::compact(rlang::dots_list(...))
  .zero_row <- purrr::map_lgl(., ~nrow(.x) == 0)
  if (any(.zero_row)) . <- .[!.zero_row]
  .sym <- unique(do.call(c, purrr::map(., get_sym)))
  .query <- merge_query(.)
  .indexes <- purrr::map_chr(., time_index)
  .index <- unique(.indexes)
  if (length(.index) > 1) {
    # if some indexes arent named the same, make them uniform
    .off_named <- !.indexes %in% .index
    purrr::map(.[which(.off_named)], ~{
      names(.x) <- stringr::str_replace(names(.x), unique(.indexes[.off_named]), paste0("^",.index,"$"))
    })
  }
  dplyr::bind_rows(!!!purrr::map(., tibble::as_tibble), .id = .id) %>% 
    dplyr::distinct(!!rlang::sym(.index), .keep_all = TRUE) %>% 
    as_tsymble(index = .index, symbol = .sym, query = .query)
}





#' @title Retrieve the ticker symbol
#' @description \lifecycle{experimental}
#' Retrieve the ticker symbol from a `tsymble` returned from market_data.
#' @param x \code{(tsybble)}
#' @return \code{(character)}
#' @export 

get_sym <- function(x) x %@% "symbol"

#' @title get_query
#' @description \lifecycle{experimental}
#' Retrieve the query metadata from a `tsymble` returned from \link[AlpacaforR]{market_data}. 
#' @param x \code{(tsybble)}
#' @return \code{(character)}
#' @export 

get_query <- function(x) x %@% "query"





#' @title Add a row to a tsybble
#' @description \lifecycle{experimental}
#' Adds a row to a tsybble
#' @inheritParams tibble::add_row
#' @keywords internal

#' @importFrom tibble add_row
#' @export
#' @inherit tibble::add_row
add_row.tsymble_ts <- function(.data, ..., .before = NULL, .after = NULL)  {
  NextMethod()
}


# add_row.tsymble <- function(.data, ..., .before = NULL, .after = NULL) {
#   .c <- class(.data)
#   class(.data) <- .c[!.c %in% c("tsymble", "tbl_ts")]
#   structure(tibble::add_row(.data, ..., .before = .before, .after = .after), class = .c)
# }



as.list.interval <- function(x, ...) {
  .nm <- attributes(x)$names
  purrr::map(setNames(.nm,.nm), ~x[[.x]] %||% 0)
}

as.list <- function(x, ...) {
  UseMethod("as.list")
}