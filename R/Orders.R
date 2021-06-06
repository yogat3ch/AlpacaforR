#' @importFrom httr GET POST PATCH DELETE
#' @importFrom dplyr filter bind_rows
#' @importFrom purrr modify_depth compact map map_chr map2 pmap map_if
#' @importFrom rlang list2 abort warn
#' @importFrom jsonlite toJSON
# orders ----
# Wed Apr 22 20:23:46 2020
#' @family Orders
#' @title Get Orders
#' 
#' @description The orders API allows a user to monitor, place and cancel their orders with Alpaca. Times are returned as yyyy-mm-dd hh-mm-ss POSIXct, quantity and price as numeric, and all others as a string. See [Orders GET](https://alpaca.markets/docs/api-documentation/api-v2/orders#get-a-list-of-orders)for more details.
#' @param symbol_id \code{(character)} Specify symbol, order ID, or client_order_id (must set `client_order_id = TRUE`).
#' @param status \code{(character)} Order status to be queried. \code{open}, \code{closed} or \code{all}. Defaults to open as a string.
#' @param limit \code{(integer)} The maximum number of orders in response. Defaults to 50 and max is 500.
#' @param after \code{(Date/character)} Date in YYYY-MM-DD \href{https://www.iso.org/iso-8601-date-and-time-format.html}{(ISO8601 Format)} The response will include only orders submitted \emph{after} this date exclusive as a timestamp object.
#' @param until \code{(Date/character)} Date in YYYY-MM-DD \href{https://www.iso.org/iso-8601-date-and-time-format.html}{(ISO8601 Format)} The response will include only orders submitted \emph{before} this date exclusive as a timestamp object.
#' @param direction \code{(character)} The chronological order of response based on the submission time. \code{'asc'} or \code{'desc'}. Defaults to \code{desc}.
#' @param nested \code{(logical)} If true, the result will roll up multi-leg orders under the legs field of primary order. Default `TRUE`.
#' @param client_order_id \code{(logical)} Whether `symbol_id` is a client_order_id, defaults to `NULL (FALSE)`
#' @inheritParams account
#' @return Order `(tibble)` [Order object](https://alpaca.markets/docs/api-documentation/api-v2/orders/#order-entity) or Array of Order Objects with the following information:
#' \itemize{
#'  \item{\code{id}}{`(character)` order id}
#'  \item{\code{client_order_id}}{`(character)` client unique order id}
#'  \item{\code{created_at}}{`(POSIXct)` nullable When the order was created}
#'  \item{\code{updated_at}}{`(POSIXct)` nullable When the order was updated}
#'  \item{\code{submitted_at}}{`(POSIXct)` nullable When the order was submitted}
#'  \item{\code{filled_at}}{`(POSIXct)` nullable When the order was filled}
#'  \item{\code{expired_at}}{`(POSIXct)` nullable When the order was expired}
#'  \item{\code{canceled_at}}{`(POSIXct)` nullable When the order was canceled}
#'  \item{\code{failed_at}}{`(POSIXct)` nullable When the order failed}
#'  \item{\code{replaced_at}}{`(POSIXct)` nullable When the order was replaced}
#'  \item{\code{replaced_by}}{`(character)` id of replacement order}
#'  \item{\code{replaces}}{`(character)` id of the replaced order}
#'  \item{\code{asset_id}}{`(character)` asset ID}
#'  \item{\code{symbol}}{`(character)` Asset symbol}
#'  \item{\code{exchange}}{`(character)` Asset exchange}
#'  \item{\code{asset_class}}{`(character)` Asset class}
#'  \item{\code{qty}}{`(integer)` Ordered quantity}
#'  \item{\code{filled_qty}}{`(integer)` Filled quantity}
#'  \item{\code{filled_avg_price}}{`(numeric)` Filled average price}
#'  \item{\code{order_class}}{`(character)`}
#'  \item{\code{order_type}}{`(character)`}
#'  \item{\code{type}}{`(character)` Valid values: market, limit, stop, stop_limit}
#'  \item{\code{side}}{`(character)` Valid values: buy, sell}
#'  \item{\code{time_in_force}}{`(character)` time in force}
#'  \item{\code{limit_price}}{`(numeric)` Limit price}
#'  \item{\code{stop_price}}{`(numeric)` Stop price}
#'  \item{\code{status}}{`(character)` Status of the order}
#'  \item{\code{extended_hours}}{`(logical)` If true, eligible for execution outside regular trading hours}.
#'  \item{\code{legs}}{`(character)` When querying non-simple order_class orders in a nested style, an array of Order entities associated with this order. Otherwise, null.}
#' }
#' @examples 
#' orders(live = FALSE)
#' orders(status = "all")
#' # For a specific symbol:
#' orders("AAPL", status = "all")

#' @export
orders <-
  function(symbol_id = NULL,
           status = "open",
           limit = NULL,
           after = NULL,
           until = NULL,
           direction = "desc",
           client_order_id = NULL,
           nested = T,
           live = get_live()) {
    
  #Set URL & Headers
  
  headers = get_headers(live)
  # set status if abbreviated
  status <- match_letters(status, o = "open", c = "closed", a = "all")
  # check if id
  .is_id <- is_id(symbol_id)
  if (.is_id) {
    # if it's an order_id
    .o_id <- symbol_id
  } else {
    # if its a ticker symbol
    .o_id <- NULL
    symbol_id <- toupper(symbol_id)
  }
  if (isTRUE(client_order_id)) {
    .url <- get_url("orders:by_client_order_id", client_order_id = .o_id, live = live)
  } else if (isFALSE(client_order_id) || is.null(client_order_id)) {
    .url <- get_url(c("orders", .o_id),
                         list(status = status,
                              limit = limit,
                              after = after,
                              until = until,
                              direction = direction,
                              nested = nested),
                    live = live)
    
  }
  # yogat3ch: Create Query 2020-01-11 2157
  
  
  if (isTRUE(get0(".dbg", envir = .GlobalEnv, mode = "logical", inherits = F))) message(paste0(.url))
  # Query
  out <- httr::GET(.url, headers)
  # Clean
  out <- order_transform(out)
  
  
  if (isTRUE(inherits(symbol_id, "character") && nchar(symbol_id) > 0) && !.is_id && length(out) != 0){       #If the ticker is a character string but not an order id, and results came through then return the orders for the tickers specified.
    out = dplyr::filter(out, symbol %in% symbol_id)
  }
  return(out)
  }


# order_submit ----
# Wed Apr 22 20:23:21 2020
#' @family Orders
#' @title Submit, Cancel & Replace Orders, 
#' @description Places/Replaces/Cancels an order, or cancels all orders depending on argument to `action`. See parameter documentation and [Orders](https://alpaca.markets/docs/api-documentation/api-v2/orders) for details.Depending on the `action` specified, some arguments are required:
#' \itemize{
#'  \item{\code{action = 'submit'}}{ All arguments can be submitted. See Arguments for which are *required*.}
#'  \item{\code{action = 'replace'}}{ `qty`, `time_in_force`, `limit`, `stop`, `client_order_id` are all eligible. Only one is *required*.}
#'  \item{\code{action = 'cancel'}}{ Only `symbol_id` is *required*.}
#'  \item{\code{action = 'cancel_all'}}{ No arguments necessary.}
#'  }
#' @param symbol_id \code{(character)}  The stock symbol (*Required* when `action = "submit"`) or Order object (single row tibble) (*Required* when `action = "cancel"/"replace"`). 
#' To expedite the setting of stops and limits for open positions, an Order ID from a `'buy'` order can be provided when `action = "submit"` to place a `'sell'` order with the following parameters such that they do not need to be set manually:
#' \itemize{
#'   \item{\code{side = 'sell'}}
#'   \item{If \code{qty} is not provided, it will be populated from the buy order}
#'   \item{`symbol_id` will be set to the symbol from the buy order.}
#'   \item{If `client_order_id = TRUE`, the `client_order_id` will be set to the buy Order ID provided, effectively linking the orders for your records.}
#'   \item{All other parameters can be specified as usual.}
#' }
#' @param action \code{(character)} The action to take:
#' \itemize{
#'   \item{\code{"submit"/"s"}}{ [Request a new order](https://alpaca.markets/docs/api-documentation/api-v2/orders/#request-a-new-order) **Default**}
#'  \item{\code{"replace"/"r"}}{ [Replace an order](https://alpaca.markets/docs/api-documentation/api-v2/orders/#replace-an-order)}
#'  \item{\code{"cancel"/"c"}}{ [Cancel an order](https://alpaca.markets/docs/api-documentation/api-v2/orders/#cancel-an-order)}
#'  \item{\code{"cancel_all"}}{ [Cancel all orders](https://alpaca.markets/docs/api-documentation/api-v2/orders/#cancel-all-orders)}
#' }
#' @param qty \code{(integer)} The amount of shares to trade (*required* when `action = "submit"`, *optional* when `action = 'replace'`).
#' @param side \code{(character)} The side of the trade. I.E `"buy"/"b"` or `"sell"/"s"`. (*required* when `action = "submit"`). Assumed to be `"buy"` if `order_class = "bracket"`. 
#' @param type \code{(character)} The type of trade order. I.E `"market"/"m"`,`"limit"/"l"`,`"stop"/"s"`,`"stop_limit"/"sl"`, `"trailing_stop"/"ts"` etc. Default `NULL`. Typically *required* except in certain situations where the value can be assumed:
#' \itemize{
#'   \item{\code{stop} is set (and `type` is unset)}{ `type = "stop"/"s"`}
#'   \item{\code{limit} is set}{ `type = "limit"/"l"`}
#'   \item{\code{stop} & \code{limit} are set}{ `type = "stop_limit"/"sl"`}
#'   \item{\code{order_class = "bracket"}}{ `type = "market"/"m"`}
#'   \item{\code{order_class = "oco"}}{ `type = "limit"/"l"`}
#'   \item{\code{order_class = "oto"}}{ `type = "market"/"m"`}
#'   \item{\code{trail_price} is set}{ `type = "trailing_stop"`}
#'   \item{\code{trail_percent} is set}{ `type = "trailing_stop"`}
#' }
#' See [Understand Orders](https://alpaca.markets/docs/trading-on-alpaca/orders/#bracket-orders) for details.
#' @param time_in_force \code{(character)} The time in force for the order. *Optional* when `action = "replace"`. Args can be `"day"`, `"gtc"`, `"opg"` etc. Default `"day"`. Please see [Understand Orders: Time in Force](https://alpaca.markets/docs/trading-on-alpaca/orders/#time-in-force) for all types and more info. Must be `"day"` or `"gtc"` for [Advanced Orders](https://alpaca.markets/docs/trading-on-alpaca/orders/#bracket-orders). 
#' @param limit \code{(numeric)} limit price. *Required* if type is `"limit"` or `"stop_limit"` for `action = 'replace'/'submit'`. 
#' @param stop \code{(numeric)} stop price. *Required* if type is `"stop"` or `"stop_limit"` for `action = 'replace'/'submit'`.
#' @param extended_hours \code{(logical)} Default \code{FALSE}. If \code{TRUE}, order will be eligible to execute in premarket/afterhours. Currently supported hours are: Pre-market: 9:00 - 9:30am, After-hours: 4:00 - 6:00pm ET. Only works with `type = 'limit'` and `time_in_force = 'day'` on the V2 API.
#' @param client_order_id \code{(character/logical)}  <= 48 Characters.  A unique identifier for the order. Automatically generated if not sent. 
#' \itemize{
#'   \item{\code{`action = 'replace'/'submit'`}}{ *Optional* character vector}
#'  \item{\code{`action = 'submit'`}}{ If an Order object is provided to `symbol_id`, `TRUE` will set the `client_order_id` for the sell order to Order ID in `symbol_id`. Used to link buy & sell orders for your records.}
#' } 
#' @param order_class \code{(character)} `'simple'`, `'bracket'`, `'oco'` or `'oto'`. *Required for advanced orders.* For details of non-simple order classes, please see [Advanced Orders](https://alpaca.markets/docs/trading-on-alpaca/orders#bracket-orders). If `order_class = 'bracket'/'oto'`, `type` can be omitted as it will always be `'market'`, this is also true with `order_class = "oco"` as `type` will always be `'limit'`. *Note* that order replacement is not supported for all advanced order types. 
#' @param take_profit \code{(named list)} Additional parameters for take-profit leg of advanced orders:
#' \itemize{
#'  \item{\code{'limit_price'/'l'}}{ \code{numeric} **required** for `'bracket'` & `'oco'` order classes.}
#' }
#' @param stop_loss \code{(named list)} Additional parameters for stop-loss leg of advanced orders
#' \itemize{
#'   \item{\code{stop_price/s}}{ \code{numeric} **required** for bracket orders}
#'   \item{\code{limit_price/l}}{ \code{numeric} The stop-loss order becomes a stop-limit order if specified. **Required** for `'bracket'` & `'oco'` order classes}
#' }
#' @param trail_price \code{(numeric)} a dollar value away from the highest water mark (hwm). If you set this to 2.00 for a sell trailing stop, the stop price is always hwm - 2.00
#' @param trail_percent \code{(numeric)} a percent value away from the highest water mark. If you set this to 1.0 for a sell trailing stop, the stop price is always hwm * 0.99. Values less than 1 are assumed to be percentages, ie .07 = 7%, values must be less than 100.
#' @inheritParams account
#' @inherit orders return
#' @examples 
#' # most orders (except limit) must be placed during market hours or they will not be filled until the next trading day. 
#' .c <- clock()
#' (bo <- order_submit("AAPL", qty = 1, side = "buy", type = "market"))
#' # Or you can submit a limit order (`type` is assumed to be `"limit"` when only `limit` is set):
#' (so <- order_submit("AAPL", q = 1, side = "s", lim = 120))
#' if (.c$is_open) {
#' # cancel an order with `action = "cancel"`. symbol_id can be either the id of the order to cancel or the order tbl object.
#' order_submit(so, a = "c")
#' # expedite a simple "sell" order by providing the id of a buy order. This can be linked to it's original buy order on the Alpaca side via the `client_order_id` by simply setting `client_order_id = T`
#' (so <- order_submit(bo, stop = 120, cli = TRUE)) # here the id is used
#' so$client_order_id == bo$id
#' # replace `"r"` parameters for simple orders (Alpaca devs are working on replacement for complex orders as of 2020-05-06)
#' order_submit(so, a = "r", stop = 123)
#' # Complex orders can be submitted as well
#' # Here is an example of setting a "bracket" order
#' # first retrieve the going price
#' (.lq <- market_data("lq", symbol = "AMZN"))
#' # sell if the price moves up 3% by setting `take_profit`
#' tp <- list(l = .lq$askprice * 1.03)
#' # hedge risk by setting a stop if it loses 5% and limit if it loses 6% with `stop_loss`
#' sl <- list(l = .lq$askprice * .94, s = .lq$askprice * .95)
#' # note that the names of these list items can be partial
#' (br_o <- order_submit("AMZN", order_class = "bracket", qty = 2, take_profit = tp, stop_loss = sl))
#' # side is assumed to be buy, and type is assumed to be market
#' # Set a trailing stop by price
#' m_o <- order_submit("AMZN", side = "buy", type = "market", qty = 1)
#' ts_o <- order_submit(m_o, trail_price = 30)
#' # Set a trailing stop by percent
#' m_o <- order_submit("AMZN", side = "buy", type = "market", qty = 1)
#' ts_o <- order_submit(m_o, trail_percent = 5)
#' }
#' # all open orders can be canceled with `action = "cancel_all"`
#' order_submit(a = "cancel_all")
#' @export

order_submit <-
  function(symbol_id,
           action = "submit",
           qty = NULL,
           side = NULL,
           type = NULL,
           time_in_force = "day",
           limit = NULL,
           stop = NULL,
           extended_hours = NULL,
           client_order_id = NULL,
           order_class = NULL,
           take_profit = NULL,
           stop_loss = NULL,
           trail_price,
           trail_percent,
           live = get_live()) {
    
    if (isTRUE(tolower(type) %in% c("trailing_stop", "ts")) || (!missing(trail_percent) || !missing(trail_price))) {
      if (is.null(type))
        type <- "trailing_stop"
      .mc <- match.call()
      stop_price <- grep("^trail", names(.mc), value = TRUE)
      stopifnot(length(stop_price) == 1)
      stop <- round(.mc[[stop_price]], 2)
    } else {
      stop_price <- "stop_price"
    }
  
    ovar <- environment()
    ovar$.vn <-
      list(
        symbol_id = "character",
        action = "character",
        side = c("character", "NULL"),
        type = c("character", "NULL"), 
        qty = c("numeric", "integer", "NULL"),
        time_in_force = "character",
        limit = c("numeric", "integer", "NULL"),
        stop = c("numeric", "integer", "NULL"),
        stop_price = c("character", "numeric"),
        extended_hours = c("logical", "NULL"),
        client_order_id = c("logical","character", "NULL"),
        order_class = c("character", "NULL"),
        take_profit = c("list", "NULL"),
        stop_loss = c("list", "NULL"),
        trail_price = c("numeric", "integer"),
        trail_percent = c("numeric", "integer"),
        live = "logical"
      )
    
  .cancel_all <- any(grepl("cancel_all", as.character(match.call()), ignore.case = T))
  if (!.cancel_all) {
    action <- substr(tolower(action), 0, 1)
    # if the order tbl is supplied directly, extract the id
    order_symbol_id(symbol_id)
  } else {
    action = "c"
  } 
  
  
  
  rlang::env_bind(ovar, type = type, action = action)
  
  # smart detect: type, order_class, extended_hours
  # fix names for take_profit, stop_loss if partialed
  # or throw errors/warnings for specific criteria
  if (any(action %in% c("s", "r", "c"))) {
    order_check() 
  }
  
  .is_id <- is_id(symbol_id)
  # detect the argument provided to symbol_id
  if (action == "s") {
  
    #Create body with order details if action is submit or replace
    bodyl <-
        append(purrr::modify_depth(purrr::compact(
          rlang::list2(
            symbol = symbol_id,
            qty = qty,
            side = side,
            type = type,
            time_in_force = time_in_force,
            limit_price = limit,
            !!stop_price := stop,
            client_order_id = client_order_id,
            order_class = order_class
            )
        ),-1, .f = as.character, .ragged = TRUE),
        purrr::modify_depth(purrr::compact(list(take_profit = take_profit,
        stop_loss = stop_loss)), -1, .f = as.character, .ragged = TRUE))
    
    bodyl$extended_hours <- extended_hours
    bodyl <- jsonlite::toJSON(bodyl, auto_unbox = TRUE)
  } else if (action == "r") {
    bodyl <-
      jsonlite::toJSON(
        purrr::modify_depth(purrr::compact(
          list(
            qty = qty,
            side = side,
            time_in_force = time_in_force,
            limit_price = limit,
            stop_price = stop,
            client_order_id = client_order_id)
        ),-2, .f = as.character),
        auto_unbox = TRUE)
  }
  
  #Set URL & Headers
  headers = get_headers(live)
  .path <- c("orders")
  if (action %in% c("r","c") && !.cancel_all) {
    # if replacing or canceling, append the order ID
    .path <- append(.path, symbol_id)
  }
  .url <- get_url(.path, live = live)
  .f <- switch(action,
         s = httr::POST,
         r = httr::PATCH,
         c = httr::DELETE)
  .args <- switch(action,
         s = ,
         r = list(url = .url, body = bodyl, encode = "raw", headers),
         c = list(url = .url, headers))
  out <- rlang::exec(.f, !!!.args)
  out <- order_transform(out)
  return(out)
}

#' @title Transform order objects
#' @description Replaces character quantities with numeric and character dates with POSIXct
#' @param .o An order object
#' @keywords internal
o_transform <- function(.o) {
  if ("cost_basis" %in% names(.o))
    .o <- dplyr::mutate(.o, dplyr::across(.fns = ~ifelse(!is.na(as.numeric(.x)), as.numeric(.x), .x)))
  .o <- dplyr::mutate(.o, dplyr::across(dplyr::ends_with("at"), ~lubridate::ymd_hms(.x, tz = Sys.timezone())))
  out <- dplyr::mutate(.o, dplyr::across(where(~is.character(.x) && !is.na(as.numeric(toNum(.x)))), toNum))
  return(out)
}

#' @title Transform order responses
#' 
#' @description Parses order type responses and replaces plain text quantities and dates with respective R objects
#' @param orders A dataframe returned from any orders_* endpoint
#' @return \code{(tibble)}  with respective R compliant objects (numeric, POSIXct/Datetime, character)
#' @keywords internal

order_transform <- function(o) {
  if (class(o) == "response") {
    if (length(o$content) == 0 && grepl("^2", o$status_code)) {
      message(paste0("Order canceled successfully"))
    } else if (grepl("^5", o$status_code)) {
      rlang::warn("Failed to cancel order.")
    }
    .method <- o$request$method
    .code <- o$status_code
    .o <- response_text_clean(o)
    if (!inherits(.o, "character")) {
      .message <- .o$message
    } else {
      .message <- .o
    }
  } else if (class(o) != "response") {
    .method <- "DELETE"
    .code <- 200
    .o <- o
  }
  
  if (grepl("^4", .code)) {
    rlang::warn(paste0("Code: ",.code,",\nMessage:", .message))
    return(.o)
  }
  
  if ((is.list(.o) && length(.o) > 0) || ("body" %in% names(.o) && grepl("DELETE", .method, ignore.case = TRUE))) {
    if (grepl("DELETE", .method, ignore.case = TRUE) && "body" %in% names(.o)) {.o <- .o$body;.q <- .o[1:2]}
    .o <- tibble::as_tibble(purrr::map(.o, rlang::`%||%`, NA))
    suppressMessages({
      suppressWarnings({
        if (!is.null(.o$legs) && !is.na(.o$legs)) {
          if (inherits(.o$legs, "list")) {
            .o$legs <- purrr::map(.o$legs, ~{
              if (!is.null(.x)) {
                .out <- o_transform(.x)
              } else {
                .out <- .x
              }
              return(.out)
            })
          } else {
            .o$legs <- o_transform(.o$legs)
          }
        }
        out <- o_transform(.o) 
      })})
    
  } else if (length(.o) == 0 && grepl("GET", .method, ignore.case = TRUE)) {
    message(paste("No orders for the selected query/filter criteria.","\nCheck `symbol_id` or set status = 'all' to see all orders."))
    out <- .o
  } else if (grepl("DELETE", .method, ignore.case = TRUE)) {
    # case when deleting single order
    out <- .o
  }
  if (exists(".q", inherits = FALSE)) attr(out, "query") <- .q
  return(out)
}


#' @title order_check
#' @description smart detect: type, order_class, extended_hours. Fix names for take_profit, stop_loss if partialled. Throw errors/warnings for specific criteria
#' @param penv \code{environment} the parent environment, otherwise a named list of arguments from the parent environment
#' @param ... named arguments. Will automatically get arguments from enclosing environment. 
#' @return \code{(list)} returns list with appropriate arguments, to be merged with parent environment via `list2env`
#' @keywords internal
order_check <- function(..., ovar = get0("ovar", mode = "environment", envir = rlang::caller_env())) {
  force(ovar)
  fetch_vars(ovar$.vn[!names(ovar$.vn) %in% c("trail_price", "trail_percent")], ..., evar = ovar)

  #  smart detect order_class ----
  # Fri May 15 13:48:32 2020
  if (!is.null(order_class)) {
    .oc <- tolower(substr(order_class, 0, 1))
    if (.oc == "b") {
      order_class <- "bracket"
    } else if (!order_class %in% c("oto", "oco")) {
      rlang::abort(paste0(order_class, "is invalid `order_class`. See ?order_submit for help."))
    }
  }
  
  # if side is partialled or missing ----
  # Thu Apr 30 20:32:52 2020
  if (action == "s") {
    
    # set type if partialled and order_class is NULL  ----
    # Thu Apr 30 20:20:16 2020
    if (!is.null(type) && is.null(order_class)){
      type <- tolower(type)
      if (grepl("^s", type) && grepl("(?<!i)l", type, perl = TRUE)) {
        type <- "stop_limit"
      } else {
        type <- match_letters(type, n = 3, "trailing_stop", "stop", "limit", "market")
      }
    }
    
    if (!is.null(side)) {
      side <- match_letters(side, "buy", "sell")
      if (class(side) == "try-error") rlang::abort("Invalid value for `side`")
    } else if (is.null(side)) {
      if ((order_class %||% "none") %in% c("bracket", "oto")) {
        side <- "buy"
        message("order_class: ", order_class," requires side = 'buy', `side` set to 'buy'.")
      } else if ((order_class %||% "none") == "oco") {
        side <- "sell"
        message("order_class: 'oco' requires side = 'sell', `side` set to 'sell'.")
      } else {
        rlang::abort("`side` is required for order submissions.")
      }
    }
    
    # Short sell/stop buy warning
    if (side == "sell") {
      .pos <- try(positions(symbol_id, live = live), silent = TRUE)
      
      if (is_error(.pos) && grepl("position does not exist", attributes(.pos)$condition$message)) {
        cli::cli_alert_warning(paste0("No positions exist for ",paste0(symbol_id, collapse = ", "),". This order will be a short sell."))
      }
    } else if (side == "buy" && (!is.null(stop))) {
      .warn_msg <- switch(stop_price, 
             stop_price = paste0("reaches ", stop),
             trail_price = paste0("decreases by ", stop),
             trail_percent = paste0("decreases by ", stop, " percent"))
      cli::cli_alert_warning(paste0("This stop buy order will execute when the price ", .warn_msg))
    }
    
    # if quantity is missing ----
    # Thu Apr 30 20:17:38 2020
    if (is.null(qty)) {
      rlang::abort("qty must be set.")
    }
    # fix names for take_profit and stop_loss
    if (!is.null(take_profit)) names(take_profit) <- "limit_price"
    if (!is.null(stop_loss)) {
      .n <- purrr::imap_chr(stop_loss, ~{
        if (grepl("^l", .y, ignore.case = T)) "limit_price" else "stop_price"
      })
      names(stop_loss) <- .n
    }
    if ((order_class %||% "none") == "bracket" && (type %||% "none") != "market") {
      message("order_class: 'bracket' requires type = 'market'. `type` set to 'market'.")
      type <- "market"
    } else if ((order_class %||% "none") == "oco" && (type %||% "none") != "limit") {
      
      message("order_class: 'oco' requires type = 'limit'. `type` set to 'limit'.")
      type <- "limit"
    } else if ((order_class %||% "none") == "oto" && (type %||% "none") != 'market') {
      message("order_class: 'oto' requires type = 'market'. `type` set to 'market'.")
      type <- "market"
    }
    
    
    if (is.null(order_class)) {
      # smart detect type in the absence of order_class
      if (is.null(type) && !is.null(limit) && is.null(stop)) {
        # if just limit is provided
        if (is.null(stop) && is.null(type)) {
          type <- "limit";message("`type` set to 'limit'")
        }
      } else if (is.null(type) && !is.null(stop) && is.null(limit)) { 
        # if just stop is provided
        if (is.null(limit) && is.null(type)) {
          type <- "stop";message("`type` set to 'stop'")
        }
      } else if (!is.null(stop) && !is.null(limit)) {
        if (is.null(type)) type <- "stop_limit";message("`type` set to 'stop_limit'")
      } else if (is.null(type)) {
        rlang::abort("`type` must be set.")
      }
      # throw errors if not detected or arguments don't match
      if (type == "limit" && is.null(limit)){ 
        rlang::abort(paste0("Please set limit price."))
      } else if (type == "stop" && is.null(stop)) {
        rlang::abort(paste0("Please set value for `stop` argument when `type = ", type,"`."))
      } else if ((is.null(stop) || is.null(limit)) && type == "stop_limit") {
        rlang::abort(paste0(paste0(unlist(purrr::imap(list(stop = stop, limit = limit), ~{
          if (is.null(.x)) .y else NULL
        })), collapse = ", "), " must be set."))
      } else if (is.null(stop) && type == "trailing_stop") {
        rlang::abort(paste0("Please set `trail_price` or `trail_percent` when `type = 'trailing_stop'`"))
      } else if (stop_price == "trail_percent" && stop < 1) {
        stop <- stop * 100
      } else if (stop_price == "trail_percent" && stop > 100) {
        rlang::abort("`trail_percent` must be < 100")
      } 
    } else if (!is.null(order_class)) {
      # if order class is specified, set required arguments accordingly or throw errors
      # order_class Advanced orders ----
      # Thu Apr 30 15:05:26 2020  
      if ((is.null(take_profit) && is.null(stop_loss)) && order_class == "oto") {
        rlang::abort("`take_profit` or `stop_loss` must have at least one parameter set when order_class = 'oto'")
      } else if ((is.null(take_profit) || is.null(stop_loss)) && order_class %in% c('oco','bracket')) {
        rlang::abort("`take_profit` must be set, and `stop_loss` must have at least one parameter set when order_class = 'oco'/'bracket'")
      }
      # parameter parsing, error checking & warnings for advanced orders
      if (order_class == "bracket") {
        if (!time_in_force %in% c("day","gtc")) {
          rlang::abort("time_in_force must be 'day' or 'gtc' when `order_class = 'bracket'. See documentation for details.")
        }
      } 
    }
    if (isTRUE(extended_hours) && (type != "limit" || time_in_force != "day" || order_class %in% c("oco","oto", "bracket"))) rlang::abort(paste0("Extended hours only supports simple 'limit' orders and `time_in_force = 'day'`"))
  } else if (action == "c") {
    if (is.null(symbol_id)) rlang::abort("`symbol_id` is NULL, the order may not have been placed successfully?")
  } 
  
  out <- list(
    symbol_id = symbol_id,
    action = action,
    type = type,
    qty = qty,
    side = side,
    time_in_force = time_in_force,
    limit = limit,
    stop = stop,
    stop_price = stop_price,
    extended_hours = extended_hours,
    client_order_id = client_order_id,
    order_class = order_class,
    take_profit = take_profit,
    stop_loss = stop_loss
  )
  rlang::env_bind(ovar, !!!out)
}

#' @title retrieve the order id if an order object is supplied
#' @description Retrieves the order_id from an order object if provided as `symbol_id`, or provides informative error if input order failed.
#' @inheritParams order_submit
#' @keywords internal

order_symbol_id <- function(symbol_id, ..., ovar = get0("ovar", mode = "environment", envir = rlang::caller_env())) {
  fetch_vars(ovar$.vn[c("side", "action", "qty", "client_order_id")], ..., evar = ovar)
  # symbol_id ----
  # Fri May 01 11:15:39 2020
  # Check if ticker is an id or order tbl
  if (is_id(symbol_id))
    symbol_id <- orders(symbol_id)
  
  if (inherits(symbol_id, "data.frame")) {
    if (is_id(symbol_id$id)) {
      if (action == "s") {
        if (symbol_id$side == "buy") {
          # Create message update
          .m <- purrr::keep(list(
            side %||% "`side` set to 'sell'",
            qty %||% paste0("`qty` set to ", symbol_id$qty),
            paste0("`symbol_id` set to ", symbol_id$symbol),
            ifelse(isTRUE(client_order_id), paste0("`client_order_id` set to ", symbol_id$id), 1)
          ), is.character)
          
          if (isTRUE(client_order_id)) client_order_id <- symbol_id$id
          side <- "sell"
          #if symbol_id is ID, action is submit and qty is NULL, populate qty from previous order
          qty <- symbol_id$qty
          symbol_id <- symbol_id$symbol
          message(paste0(.m, collapse = "\n"))
        }
      } else if (action %in% c("r","c")) {
        symbol_id <- unique(symbol_id$id)
        if (length(symbol_id) > 1) rlang::abort("`symbol_id` must contain a single order")
      }
    } else 
      rlang::abort(paste0("The order object provided as `symbol_id` is invalid.\norder code: ",symbol_id$code,"\nmessage: ", symbol_id$message))
    
    
  } else {
    symbol_id <- unique(toupper(symbol_id))
    # If client_order_id is TRUE but the symbol_id is not an order, change it back to NULL
    client_order_id <- purrr::when(isTRUE(client_order_id),
                                   . ~ NULL,
                                   ~ client_order_id)
  }
  rlang::env_bind(ovar, symbol_id = symbol_id, side = side, action = action, qty = qty, client_order_id = client_order_id)
}
