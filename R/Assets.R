
# assets ----
# Wed Apr 22 20:32:51 2020
#' @family Assets
#' @title Get Assets 
#' 
#' @description The assets API serves as the master list of assets available for trade and data consumption from Alpaca. Assets are sorted by asset class, exchange and symbol. Some assets are only available for data consumption via Polygon, and are not tradable with Alpaca. These assets will be marked with the flag `tradable = FALSE`. See [Assets: GET](https://alpaca.markets/docs/api-documentation/api-v2/assets#get-an-asset) for details.
#' @param ... `(character)` asset symbols or IDs. If NULL (the default), a tibble of all assets is returned. 
#' @details This function is vectorized over all arguments.
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
#' (AAPL <- assets("AAPL"))
#' # or by id:
#' (AAPL <- assets(AAPL$id))
#' @importFrom purrr map_dfr
#' @importFrom httr GET
#' @export
assets <- function(...){
  ticker_id <- rlang::dots_list(...)
  headers = get_headers()
  if (rlang::is_empty(ticker_id)) {
    ticker_id <- NULL
    out = asset_transform(httr::GET(url = get_url(c("assets")), headers))
  } else {
    .is_id <- is_id(ticker_id)
    if (!.is_id) ticker_id <- toupper(ticker_id) # caps if ticker
    out <- purrr::map_dfr(ticker_id, ~{
      # get response
      asts = asset_transform(httr::GET(url = get_url(c("assets", .x)), headers))
    })
  }
  
  return(out)
}


#' @title asset_transform
#' @description Transform API response to asset tibble
#' @param asts \code{(response)} 
#' @return \code{(tibble)}
#' @keywords Internal
#' @importFrom tibble as_tibble
#' @importFrom rlang warn
asset_transform <- function(asts) {
  out = response_text_clean(asts)
  if (!is.null(out$code)) {
    rlang::warn(out$message)
    out <- tibble::tibble(
      id = NA,
      class = NA,
      exchange = NA,
      symbol = basename(asts$url),
      name = "",
      status = out$message,
      tradeable = FALSE,
      marginable = FALSE
    )
  } else {
    out <- tibble::as_tibble(out)
  }
  out
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
