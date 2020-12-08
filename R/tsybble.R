#' @title Construct a tsybble
#' @description \lifecycle{experimental}
#' Constructs a \code{tsybble}. A \code{\link[tsibble]{tsibble}} with ticker symbol, and query metadata attributes. 
#' @param symbol \code{(character)} Ticker symbol
#' @param query \code{(list)} Query metadata
#' @inheritParams tsibble::as_tsibble
#' @inheritDotParams tsibble::as_tsibble  -index -x
#' @export

as_tsybble <- function(x, index, symbol, query, ...) {
  structure(tsibble::as_tsibble(x, index = index, ...),
            symbol = symbol,
            query = query,
            class = c("tbl_ts", "tbl_df", "tbl", "data.frame", "tsybble"))
}



#' @title Retrieve the ticker symbol
#' @description \lifecycle{experimental}
#' Retrieve the ticker symbol from a `tsybble` returned from market_data.
#' @param x \code{(tsybble)}
#' @return \code{(character)}
#' @export 

get_sym <- function(x) attr(x, "symbol")

#' @title get_query
#' @description \lifecycle{experimental}
#' Retrieve the query metadata from a `tsybble` returned from \link[AlpacaforR]{market_data}. 
#' @param x \code{(tsybble)}
#' @return \code{(character)}
#' @export 

get_query <- function(x) attr(x, "query")