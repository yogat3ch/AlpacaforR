# positions ----
# Wed Apr 22 20:25:01 2020
#' @family Positions
#' @title Get Positions
#'
#' @description The positions API provides information about an account's current open positions. The response will include information such as cost basis, shares traded, and market value, which will be updated live as price information is updated. Character values are returned as a string while integer values are returned as numeric. See [Positions](https://alpaca.markets/docs/api-documentation/api-v2/positions/) for details.
#' @param symbols `(character)` Symbol(s) for which Position information will be retrieved. 
#' @param action `(character)` 
#' \itemize{
#'   \item{\code{"get"/"g"}}{ \code{GET} all positions or those specified by `symbols`}
#'   \item{\code{"close","c"}}{ Close positions specified by `symbols`}
#'   \item{\code{"close_all"}}{ Close all positions}
#' }
#' @inheritParams account
#' @details `position` is vectorized and thus multiple arguments may be provided to `symbols` and the function will perform the `action` specified for each. `action` is not vectorized, and only one action may be performed for a set of symbols at a given time.
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
#' positions("AAPL", live = FALSE)
#' positions("AAPL", live = TRUE)
#' positions() # all paper positions
#' positions(live = TRUE) # all live positions
#' #close a single position with `action  = "close"`
#' positions("aapl", a = "c")
#' #cancel all paper positions
#' positions(a = "close_all")
#' @export
positions <-
  function(symbols = NULL,
           action = "get",
           live = get_live()) {
    
  if (is.character(symbols)) symbols <- toupper(symbols)
  
  .all <- grepl("close_all", action, ignore.case = TRUE)
  if (!.all) {
    action <- c(g = "get", c = "close")[substr(action,1,1)]
  }
  #Set URL, live = FALSE & Headers
  .url <- get_url("positions", live = live)
  headers = get_headers(live)
  if (.all) {
    .url <- get_url("positions", query = list(cancel_orders = TRUE), live = live)
    #Send Request
    pos = httr::DELETE(url = .url, headers)
    out <- pos_transform(pos)
  } else if (length(symbols) > 0 && action %in% c("close", "get")) {
    if (action == "get") {
      .expr <- rlang::expr({pos = httr::GET(url = .url, headers)})
    } else if (action == "close") {
      .expr <- rlang::expr({pos = httr::DELETE(url = .url, headers)})
    }
    # if a list of symbols is specified, close or fetch each.
    out <- purrr::map_dfr(symbols, ~{
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

pos_recursive <- . %>%
  {suppressMessages(dplyr::mutate(., dplyr::across(tidyselect::ends_with("at"), lubridate::as_datetime, tz = Sys.timezone())))} %>% 
  dplyr::mutate(dplyr::across(tidyselect::ends_with(c("qty", "price", "percent")), ~as.numeric(.x %||% NA)))



#' @title transform positions objects
#' 
#' @description Cleans arrays of position objects, and position objects
#' @keywords internal
pos_transform <- function(pos) {
  
  if (inherits(pos,"response")) {
    .method <- pos$request$method
    .url <- httr::parse_url(pos$url)
    .sym <- stringr::str_extract(.url$path, "\\w+$")
    .q <- list(ts = lubridate::with_tz(pos$date, Sys.timezone()),
         status_code = pos$status_code,
         url = pos$url)
    .pos <- response_text_clean(pos)
    .message <- .pos$message
  } else {
    stop("`*_transform` requires a response object.")
  }
  
  # check for error
  if(any(grepl(pattern = "^4", x = .q$status_code))) {
    rlang::warn(paste("Position was not",ifelse(grepl("GET", .method, ignore.case = TRUE), "found.", "modified."),"\n Message:", .message))
    return(.pos)
  }
  
  # if close_all
  .all <- isTRUE(as.logical(.url$query$cancel_orders))
  if (.all && !rlang::is_empty(.pos$body)) {
    out <- dplyr::bind_cols(.pos[!names(.pos) %in% "body"], dplyr::rename(.pos$body, order_status = "status", order_symbol = "symbol")) 
    out <- pos_recursive(out)
    if ("legs" %in% names(out))
      out$legs <- purrr::map(
        out$legs,
        ~ purrr::when(
          is.null(.x),
          isTRUE(.) ~ .x, isFALSE(.) ~ pos_recursive(.x)
        )
      )
    purrr::pwalk(out, ~{
      .vars <- list(...)
      if (.vars$status == 200) 
        message(paste0(
          .vars$updated_at,
          " - ",
          .vars$id, " for ",
          .vars$symbol,
          " is ",
          .vars$order_status,
          if (is.character(.vars$message) && !is.na(.vars$message)) paste0("\nmessage:", .vars$message)
              else
                NULL
        ))
      else
        rlang::warn(paste0(
          .vars$symbol,
          " has status ",
          .vars$status,
          if (!is.na(.vars$message) && is.character(.vars$message)) paste0("\nmessage:", .vars$message)
              else
                NULL
        ))
    })
    
  } else if(rlang::is_empty(.pos$body)) {
    message("No positions are open at this time.")
    out <- .pos
  } else if (length(.pos) > 1 && !(grepl("DELETE", .method, ignore.case = TRUE) && .sym == "positions")) {
    out <- order_transform(pos)
  } 
  
  attr(out, "query") <- .q
  return(out)
}

