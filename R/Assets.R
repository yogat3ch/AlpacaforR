
# assets ----
# Wed Apr 22 20:32:51 2020
#' @family Assets
#' @title Get Assets 
#' 
#' @description The assets API serves as the master list of assets available for trade and data consumption from Alpaca. Assets are sorted by asset class, exchange and symbol. Some assets are only available for data consumption via Polygon, and are not tradable with Alpaca. These assets will be marked with the flag `tradable = FALSE`. See [Assets: GET](https://alpaca.markets/docs/api-documentation/api-v2/assets#get-an-asset) for details.
#' @param ticker_id `(character)` of asset symbols or IDs. If NULL (the default), a tibble of all assets is returned. 
#' @inheritParams account 
#' @details This function is vectorized and will accept a `(character)` vector for `ticker_id`.
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
#' @importFrom httr GET parse_url build_url
#' @importFrom purrr compact map_dfr
#' @importFrom tibble as_tibble
#' @export
assets <- function(ticker_id = NULL, v = 2){
  #Set URL & Headers
  .is_id <- is_id(ticker_id)
  if (!.is_id) ticker_id <- toupper(ticker_id) # caps if ticker
  .url = httr::parse_url(get_url())
  headers = get_headers()
  if (!inherits(v, c("integer", "numeric"))) {
    message("`v` must be numeric. `v` set to 2.")
    v <- 2
  }
  # Create url
  out <- purrr::map_dfr(ticker_id, ~{
    .url$path <- purrr::compact(list(paste0("v",v),
                                     "assets",
                                     .x))
    .url <- httr::build_url(.url)
    # get response
    asts = httr::GET(url = .url, headers)
    asts = response_text_clean(asts)
    if (!is.null(asts$code)) {
      rlang::warn(asts$message)
      return(tibble::tibble(id = NA, class = NA, exchange = NA, symbol = .x, name = "", status = asts$message, tradeable = FALSE, marginable = FALSE))
    }
    out <- tibble::as_tibble(asts)
  })
  
  return(out)
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
