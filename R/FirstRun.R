#' @title First Run Credential Setup
#' @description This function helps edit the .Renviron file to work with `AlpacaforR`. It's for first time users and only needs to be run once.
#' @param paper_api \code{(named character)} of length two: `key` and `secret` for the Alpaca paper API.
#' @param live_api \code{(named character)} of length two: `key` and `secret` for the Alpaca live API.
#' @param polygon_api \code{(named character)} of length one: `key`
#' @param pro \code{(logical)} of length one: Set as `TRUE` to use the \href{Alpaca Market Data Pro Subscription}{https://alpaca.markets/docs/api-documentation/api-v2/market-data/alpaca-data-api-v2/#subscription-plans} otherwise the Basic plan will be used. **Default** `FALSE`
#' @param live \code{(logical)}: the default option for whether to use the live account `TRUE` or paper account `FALSE`. This option can be overridden for functions that accept the `live` argument, or can be changed at a later time. **Default** `FALSE`
#' @param scope \code{(character)} specify at which level of scope the changes to .Renviron should apply.
#' @return Sets environment variables for this session and edits the .Renviron file in R's home directory `path.expand("~")` and adds the specified key/secret combinations for future sessions. If an .Renviron file does not exist, it will be created.
#' @export
#' @examples
#' \dontrun{
#' firstrun(
#' paper_api = c(key = "mypaperkey", secret = "mypapersecret"),
#' live_api = c(key = "mylivekey", secret = "mylivesecret"),
#' polygon_api = c(key = "polygonkey"),
#' live = FALSE,
#' pro = FALSE
#' )
#' }

firstrun <- function(paper_api = NULL, live_api = NULL, polygon_api = NULL, pro = FALSE, live = FALSE, scope = c("user", "project")) {

  keys <- purrr::compact(
    list(
      `APCA-PAPER-KEY` = if (!missing(paper_api))
        paper_api[1],
      `APCA-PAPER-SECRET` = if (!missing(paper_api))
        paper_api[2],
      `APCA-LIVE-KEY` = if (!missing(live_api))
        live_api[1],
      `APCA-LIVE-SECRET` = if (!missing(live_api))
        live_api[2],
      `POLYGON-KEY` = if (!missing(polygon_api))
        polygon_api,
      `APCA-LIVE` = live,
      `APCA-PRO` = pro
    )
  )

  rlang::exec(UU::creds_to_renviron, !!!keys, scope = scope)
}

