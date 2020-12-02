# positions ----
# Wed Apr 22 20:25:01 2020
#' @family Positions
#' @title Get Positions
#'
#' @description The positions API provides information about an account's current open positions. The response will include information such as cost basis, shares traded, and market value, which will be updated live as price information is updated. Character values are returned as a string while integer values are returned as numeric. See [Positions](https://alpaca.markets/docs/api-documentation/api-v2/positions/) for details.
#' @param tickers `(character)` Symbol(s) for which Position information will be retrieved. 
#' @param action `(character)` 
#' \itemize{
#'   \item{\code{"get"/"g"}}{ \code{GET} all positions or those specified by `tickers`}
#'   \item{\code{"close","c"}}{ Close positions specified by `tickers`}
#'   \item{\code{"close_all"}}{ Close all positions}
#' }
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
#' #close a single position with `action  = "close"`
#' positions("aapl", a = "c")
#' #cancel all paper positions
#' positions(a = "close_all")
#' @importFrom httr GET DELETE
#' @importFrom purrr map_dfr
#' @importFrom tibble as_tibble
#' @export
positions <-
  function(tickers = NULL,
           action = "get",
           live = as.logical(Sys.getenv("APCA-LIVE", FALSE))) {
    
  if (is.character(tickers)) tickers <- toupper(tickers)
  
  .all <- grepl("close_all", action, ignore.case = TRUE)
  if (!.all) {
    action <- c(g = "get", c = "close")[substr(action,1,1)]
  }
  #Set URL, live = FALSE & Headers
  .url <- get_url("positions", live = live)
  headers = get_headers(live)
  if (.all) {
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
      .url <- get_url(c("positions", .x), live = live)
      #Send Request
      eval(.expr)
      pos_transform(pos)
    })
  } else {
    #Send Request
    pos = httr::GET(url = .url, headers)
    out <- pos_transform(pos)
  }
  return(out)
}
