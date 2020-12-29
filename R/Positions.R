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


#' @title transform positions objects
#' 
#' @description Cleans arrays of position objects, and position objects
#' @keywords internal
pos_transform <- function(pos) {
  
  if (class(pos) != "response") {
    .code <- pos$code
    .message <- pos$message
  } else if (class(pos) == "response") {
    .method <- pos$request$method
    .sym <- stringr::str_extract(pos$request$url, "\\w+$")
    .code <- pos$status_code
    #browser()
    .pos <- response_text_clean(pos)
    .message <- .pos$message
  }
  
  # if related orders
  .held <- !is.null(.pos$body$held_for_orders)
  if (.held) {
    # close all orders. As of 2020-05-15, all open related orders must be canceled to close positions
    # This should properly return orders
    message("Related orders prevent positions from being closed. Canceling related orders...")
    .pos <- purrr::pmap(.pos$body, ~{
      .vars <- list(...)
      if (is.null(.vars$related_orders)) return(NULL)
      .e <- new.env()
      withCallingHandlers(message = function(e) {
        if (e$message == "Order canceled successfully\n") 
          rlang::warn(message = paste0("Canceled order ", .vars$related_orders," for ", .vars$symbol)) 
        else
          rlang::warn(message = paste0("Could not cancel order ", .vars$related_orders," for ", .vars$symbol, ". Position for ", .vars$symbol, " remains open."))
      }, {
        .out <- order_submit(.vars$related_orders, action = "cancel")
        .out <- positions(.vars$symbol, a = "c")
      })
      return(.out)
    })
    # order_transform should properly transform orders
    
  }
  
  if(any(grepl(pattern = "^4", x = .code))) {
    rlang::warn(paste("Position was not",ifelse(grepl("GET", .method, ignore.case = TRUE), "found.", "modified."),"\n Message:", .message))
    return(.pos)
  } else if (.sym != "positions" && .sym %in% .pos$symbol && grepl("DELETE", .method, ignore.case = TRUE)) {
    message(paste0(.sym, " closed successfully."))
  } else if (grepl("DELETE", .method, ignore.case = TRUE) && any(grepl("^2", .pos$body$code %||% .pos$status))) {
    message(paste0("All positions closed successfully.\nClosed Position(s): ", paste0(.pos$body$sym[grepl("^2", .pos$body$code %||% .pos$status)], collapse = ", ")))
  }
  
  #Check if any pos exist before attempting to return
  if(length(.pos) == 0) {
    message("No positions are open at this time.")
    out <- .pos
  } else if(length(.pos) > 1 && !(grepl("DELETE", .method, ignore.case = TRUE) && .sym == "positions")) {
    out <- order_transform(pos)
  } else if (.sym == "positions") {
    # if close_all
    if (.held) {
      out <- bind_rows(.pos)
    } else {
      out <- order_transform(.pos$body)
      attr(out, "info") <- .pos[1:2]
    }
    
  }
  return(out)
}

