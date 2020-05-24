# positions ----
# Wed Apr 22 20:25:01 2020
#' @family Positions
#' @title Get Positions
#'
#' @description The positions API provides information about an account's current open positions. The response will include information such as cost basis, shares traded, and market value, which will be updated live as price information is updated. Character values are returned as a string while integer values are returned as numeric. See [Positions](https://alpaca.markets/docs/api-documentation/api-v2/positions/) for details.
#' @param tickers `(character)` Specify which symbol(s) you want to call by inserting ticker symbol(s) as a character vector. 
#' @param action `(character)` `"get"/"g"` to get all positions or those specified by `tickers`. `"close","c"` to close positions specified by `tickers`. Use `"close_all"` to close all positions. 
#' @inheritParams account
#' @details `position` is vectorized and thus multiple arguments may be provided to `tickers` and the function will perform the `action` specified for each. `action` is not vectorized, and only one action may be performed for a set of tickers at a given time.
#' @return Position `(tibble)` [Position object](https://alpaca.markets/docs/api-documentation/api-v2/positions/#position-entity) or array of Position objects of length 0 if no positions, otherwise with length of the number of positions, each with the following attributes: 
#'\itemize{
#' \item{`asset_id`}{`(character)` Asset ID.}
#' \item{`symbol`}{`(character)` Symbol of the asset.}
#' \item{`exchange`}{`(character)` Exchange name of the asset.}
#' \item{`asset_class`}{`(character)` Asset class name.}
#' \item{`qty`}{`(integer)` The number of shares.}
#' \item{`avg_entry_price`}{`(numeric)` Average entry price of the position.}
#' \item{`side`}{`(character)` long/short exposure.}
#' \item{`market_value`}{`(numeric)` Total dollar amount of the position.}
#' \item{`cost_basis`}{`(numeric)` Total cost basis in dollar.}
#' \item{`unrealized_pl`}{`(numeric)` Unrealized profit/loss in dollar.}
#' \item{`unrealized_plpc`}{`(numeric)` Unrealized profit/loss percent (by a factor of 1).}
#' \item{`unrealized_intraday_pl`}{`(numeric)` Unrealized profit/loss in dollar for the day.}
#' \item{`unrealized_intraday_plpc`}{`(numeric)` Unrealized profit/loss percent (by a factor of 1).}
#' \item{`current_price`}{`(numeric)` Current asset price per share.}
#' \item{`lastday_price`}{`(numeric)` Last day's asset price per share.}
#' \item{`change_today`}{`(numeric)` Percent change from last day price (by a factor of 1).}
#' }
#' @examples 
#' positions(ticker = "AAPL", live = FALSE)
#' positions("AAPL", live = TRUE)
#' positions() # all paper positions
#' positions(live = TRUE) # all live positions
#' #close a single position with `action  = "cancel"`
#' positions("aapl", a = "c")
#' #cancel all paper positions
#' positions(a = "cancel_all")
#' @importFrom httr GET parse_url build_url
#' @importFrom purrr map_dfr compact
#' @importFrom tibble as_tibble
#' @export
positions <- function(tickers = NULL, action = "get", live = FALSE, v = 2){
  if (is.character(tickers)) tickers <- toupper(tickers)
  #Set URL, live = FALSE & Headers
  .url = httr::parse_url(get_url(live))
  headers = get_headers(live)
  .all <- grepl("close_all", action, ignore.case = TRUE)
  if (!.all) {
    action <- c(g = "get", c = "close")[substr(action,1,1)]
  }
  #Set URL, live = FALSE & Headers
  .url = httr::parse_url(get_url(live))
  headers = get_headers(live)
  .url$path <- list(
    paste0("v",v),
    "positions")
  if (.all) {
    # close all positions
    .url <- httr::build_url(.url)
    #Send Request
    pos = httr::DELETE(url = .url, headers)
    out <- pos_transform(pos)
  } else if (length(tickers) > 0 && action %in% c("close", "get")) {
    if (action == "get") {
      .expr <- rlang::expr({pos = httr::GET(url = .url, headers)})
    } else if (action == "close") {
      .expr <- rlang::expr({pos = httr::DELETE(url = .url, headers)})
    }
    # if a list of tickers is specified, close or fetch each.
    out <- purrr::map_dfr(tickers, ~{
      .url$path <- list(
        v = paste0("v",v),
        "positions",
        ticker = .x)
      .url <- httr::build_url(.url)
      #Send Request
      eval(.expr)
      pos_transform(pos)
    })
  } else {
    # if action = "get" and no ticker is specified
    .url <- httr::build_url(.url)
    #Send Request
    pos = httr::GET(url = .url, headers)
    out <- pos_transform(pos)
  }
  return(out)
}
#----------------------------------------------------------------------------------------------
#UPDATED for V2
#' @family Positions
#' @title get_positions
#' @rdname positions
#' @description `get_positions` is deprecated, use \code{\link[AlpacaforR]{positions}} instead.
#' @examples get_positions()
#' @export
get_positions <- positions









#' @family Positions
#' @title Close Positions by ticker, or close all (Deprecated)
#'
#' @description `close_position` is deprecated. Use \code{\link[AlpacaforR]{positions}} with `action = "close"` instead. The close_positions function liquidates all positions or those specified by `ticker`. Character values are returned as a string while integer values are returned as numeric. See [Positions](https://alpaca.markets/docs/api-documentation/api-v2/positions/) for details.
#' @param ticker `(character)` *required* the symbol of the ticker for which positions will be closed. Use "close_all" to close all positions.
#' @inheritParams account
#' @inherit positions return
#' @examples 
#' close_position(ticker = "AAPL", live = FALSE)
#' close_position(ticker = "AAPL")
#' close_position(ticker = "AAPL", live = TRUE)
#' @importFrom httr DELETE parse_url build_url
#' @importFrom rlang abort
#' @importFrom purrr compact
#' @export
close_position <- function(ticker = NULL, live = FALSE, v = 2){
  message(paste0("`close_positions` is deprecated. Use `positions` instead."))
  #Set URL, live = FALSE & Headers
  .url = httr::parse_url(get_url(live))
  headers = get_headers(live)
  if (is.null(ticker)) {
    rlang::abort("ticker must be set.")
  } else if (ticker == "close_all") {
    ticker <- NULL
  }
  .url$path <- purrr::compact(list(
    v = paste0("v",v),
    "positions",
    ticker = ticker))
  .url <- httr::build_url(.url)
  #Send Request
  positions = httr::DELETE(url = .url, headers) 
  positions = response_text_clean(positions)
  
  
  return(positions)
  
}
# Wed Apr 22 20:24:07 2020
#' @family Positions
#' @rdname positions
#' @title close_all_positions (Deprecated)
#'
#' @description Deprecated. Please use \code{\link[AlpacaforR]{positions}} with `tickers = 'close_all'". The close all positions API liquidates all currently open long and short positions. Character values are returned as a string while integer values are returned as numeric.
#' @param live TRUE / FALSE if you are connecting to a live account. Default to FALSE, so it will use the paper url if nothing was provided.
#' @inherit positions return
#' @examples 
#' close_all_positions(live = FALSE)
#' close_all_positions()
#' close_all_positions(live = TRUE)
#' @importFrom httr DELETE
#' @export
close_all_positions <- function(live = FALSE){
  message("This function has been deprecated. Please use `positions` with `tickers = 'close_all'")
  #Set URL, live = FALSE & Headers
  url = get_url(live)
  headers = get_headers(live)
  
  #Send Request
  positions = httr::DELETE(url = paste0(url,"/v2/positions"), headers) 
  positions = response_text_clean(positions)
  
  
  return(positions)
}
