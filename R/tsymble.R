tsymble <- tsibble::new_tsibble(tsibble::build_tsibble(tibble::tibble(time = Sys.Date() + 0:3), index = "time", ordered = TRUE, interval = tsibble::new_interval(day = 1), index2 = "time"), symbol = character(), query = list(), class = "tsymble")
tsibble <- getFromNamespace("tsibble", "tsibble")
methods::setClass("tsymble", 
                  contains = c("tbl_ts", "tbl_df", "tbl", "data.frame"))

#' @title Construct a tsybble
#' @description \lifecycle{experimental}
#' Constructs a \code{tsymble}. A \code{\link[tsibble]{tsibble}} with ticker symbol, and query metadata attributes. 
#' @param symbol \code{(character)} Ticker symbol
#' @param query \code{(list)} Query metadata
#' @inheritParams tsibble::as_tsibble
#' @inheritDotParams tsibble::as_tsibble  -index -x
#' @export

as_tsymble <- function(x, index = tsibble::index_var(x), index2 = NULL, symbol = get_sym(x), query = get_query(x), interval = NULL, ordered = NULL, key_data = NULL) {
  force(symbol)
  force(query)
  # tsibble must have distinct index
  if (NROW(x) > 2) {
    dx <- dplyr::distinct(x, !!rlang::sym(index), .keep_all = TRUE)
  } else {
    dx <- x
  }
  if (isTRUE(interval$year > 0)) {
    # eliminates (can't obtain interval due to the mismatched index class)
    dx <- dplyr::mutate(dx, dplyr::across(index, lubridate::year))
  }
  
  dx <- do.call(tsibble::build_tsibble, list(
    dx,
    index = index,
    if (!missing(index2)) index2 = index2,
    interval = interval,
    ordered = ordered,
    key_data = key_data
  ))
  structure(
    dx,
    symbol = symbol,
    query = query,
    class = c("tbl_ts", "tbl_df", "tbl", "data.frame", "tsymble")
  )
}

#' @title Validate a `tsymble`
#' @param x \code{(tsymble)}

validate_tsymble <- function(x) {
  
  # correct classes
  if (!all(class(x) %in% c("tbl_ts", "tbl_df", "tbl", "data.frame", "tsymble"))) stop("classes must be: ", paste0(c("tbl_ts", "tbl_df", "tbl", "data.frame", "tsymble"), collapse = ", "), call. = FALSE)
  # valid index
  if (!tsibble::index_valid(x[[tsibble::index(x)]])) stop("tsymble must have valid index", call. = FALSE)
  # query attribute is list
  if (!is.list(attr(x, "query"))) {
    stop("query attribute must be a list.", call. = FALSE)
  }
  # symbol is character
  if (!is.character(attr(x, "symbol"))) stop("symbol attribute must be a character", call. = FALSE)
  # symbol is length 1
  if (!length(attr(x, "symbol"))) stop("symbol must be length 1", call. = FALSE)
  
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
  . <- rlang::dots_list(...)
  .a <- attributes(.[[1]])
  .a$query <- merge_query(.)
  .a$.Data <- dplyr::bind_rows(!!!purrr::map(., tibble::as_tibble), .id = .id) %>% 
    dplyr::distinct(!!rlang::sym(.a$index), .keep_all = TRUE) %>% 
    tsibble::as_tsibble(index = .a$index)
  .a$row.names <- row.names(.a$Data)
  do.call(structure, .a)
}

arrange.tsymble <- function(.data, ..., .by_group = FALSE) {
  .a <- attributes(.data)
  .a$.Data <- dplyr::arrange(.data, ..., .by_group)
  do.call(structure, .a)
}


#' @inherit dplyr::arrange

arrange <- function (.data, ..., .by_group = FALSE) 
{
  UseMethod("arrange")
}
#' @title Retrieve the ticker symbol
#' @description \lifecycle{experimental}
#' Retrieve the ticker symbol from a `tsymble` returned from market_data.
#' @param x \code{(tsybble)}
#' @return \code{(character)}
#' @export 

get_sym <- function(x) attr(x, "symbol")

#' @title get_query
#' @description \lifecycle{experimental}
#' Retrieve the query metadata from a `tsymble` returned from \link[AlpacaforR]{market_data}. 
#' @param x \code{(tsybble)}
#' @return \code{(character)}
#' @export 

get_query <- function(x) attr(x, "query")






#' @inherit tibble::add_row
add_row <- function(.data, ..., .before = NULL, .after = NULL)  {
  UseMethod("add_row", .data)
}

#' @title Add a row to a tsybble
#' @description \lifecycle{experimental}
#' Adds a row to a tsybble
#' @inheritParams tibble::add_row
#' @keywords internal

#' @importFrom tibble add_row
add_row.tsymble <- function(.data, ..., .before = NULL, .after = NULL) {
  .c <- class(.data)
  class(.data) <- .c[!.c %in% c("tsymble", "tbl_ts")]
  structure(tibble::add_row(.data, ..., .before = .before, .after = .after), class = .c)
}


setMethod("add_row", "tsymble", add_row.tsymble)


as.list.interval <- function(x, ...) {
  .nm <- attributes(x)$names
  purrr::map(setNames(.nm,.nm), ~x[[.x]] %||% 0)
}

as.list <- function(x, ...) {
  UseMethod("as.list")
}