
# assets ----
# Wed Apr 22 20:32:51 2020
#' @family Assets
#' @title Get Assets 
#' 
#' @description The assets API serves as the master list of assets available for trade and data consumption from Alpaca. Assets are sorted by asset class, exchange and symbol. Some assets are only available for data consumption via Polygon, and are not tradable with Alpaca. These assets will be marked with the flag `tradable = FALSE`. See [Assets: GET](https://alpaca.markets/docs/api-documentation/api-v2/assets#get-an-asset) for details.
#' @param ticker_id `(character)` Find asset details by symbol or ID. If NULL (the default), a list of assets is returned
#' @inheritParams account 
#' @return Asset `(tibble)` [Asset](https://alpaca.markets/docs/api-documentation/api-v2/assets/#asset-entity) Object or array of Asset objects with the following parameters:
#' \itemize{
#'  \item{\code{id}}{ \code{(character)} Asset ID as a string.}
#'  \item{\code{class}}{ \code{(character)} us_equity as a string.}
#'  \item{\code{exchange}}{ \code{(character)} AMEX, ARCA, BATS, NYSE, NASDAQ or NYSEARCA as a string.}
#'  \item{\code{symbol}}{ \code{(character)} Stock symbol as a string.}
#'  \item{\code{name}}{ \code{(character)} Full company name}
#'  \item{\code{status}}{ \code{(character)} active or inactive as a string.}
#'  \item{\code{tradable}}{ \code{(logical)} Asset is tradable on Alpaca or not.}
#'  \item{\code{marginable}}{ \code{(logical)} Asset is marginable on Alpaca or not.}
#'  \item{\code{shortable}}{ \code{(logical)} Asset is shortable on Alpaca or not.}
#'  \item{\code{easy_to_borrow}}{ \code{(logical)} Asset is easy-to-borrow or not (filtering for `easy_to_borrow = TRUE` is the best way to check whether the name is currently available to short at Alpaca).}
#'  }
#' @examples
#' # Get a tibble of all active assets: 
#' assets()
#' # Get a specific asset by symbol:
#' (AAPL <- assets(ticker = "AAPL"))
#' # or by id:
#' (AAPL <- assets(AAPL$id))
#' @importFrom httr GET parse_url build_url
#' @importFrom purrr compact
#' @importFrom tibble as_tibble
#' @export
assets <- function(ticker_id = NULL, v = 2){
  #Set URL & Headers
  .url = httr::parse_url(get_url())
  headers = get_headers()
  
  # Create url
  .url$path <- purrr::compact(list(paste0("v",v),
                                   "assets",
                                   ticker_id))
  .url <- httr::build_url(.url)
  # get response
  asts = httr::GET(url = .url, headers)
  asts = response_text_clean(asts)
  asts <- tibble::as_tibble(asts)
  return(asts)
}
#----------------------------------------------------------------------------------------------
#UPDATED for V2
#assets(ticker = "AAPL",version = "v2")
#' @family Assets
#' @title get_assets
#' @rdname assets
#' @description `get_assets` is deprecated. Use \code{\link[AlpacaforR]{assets}} instead.
#' @examples get_assets()
#' @export
get_assets <- assets
