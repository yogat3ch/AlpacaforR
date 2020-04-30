
# orders ----
# Wed Apr 22 20:23:46 2020
#' @family Orders
#' @title Get Orders
#' 
#' @description The orders API allows a user to monitor, place and cancel their orders with Alpaca. Times are returned as yyyy-mm-dd hh-mm-ss POSIXct, quantity and price as numeric, and all others as a string. See [Orders: GET](https://alpaca.markets/docs/api-documentation/api-v2/orders#get-a-list-of-orders)for more details.
#' @param ticker_id \code{(character)} Specify which symbol or order #
#' @param status \code{(character)} Order status to be queried. \code{open}, \code{closed} or \code{all}. Defaults to open as a string.
#' @param limit \code{(integer)} The maximum number of orders in response. Defaults to 50 and max is 500.
#' @param after \code{(Date/character)} Date in YYYY-MM-DD \href{https://www.iso.org/iso-8601-date-and-time-format.html}{(ISO8601 Format)} The response will include only orders submitted \emph{after} this date exclusive as a timestamp object.
#' @param until \code{(Date/character)} Date in YYYY-MM-DD \href{https://www.iso.org/iso-8601-date-and-time-format.html}{(ISO8601 Format)} The response will include only orders submitted \emph{before} this date exclusive as a timestamp object.
#' @param direction \code{(character)} The chronological order of response based on the submission time. \code{asc} or \code{desc}. Defaults to \code{desc}.
#' @param silent \code{(logical)} TRUE / FALSE on if you want the "no orders to cancel" message to print to the console. Default to FALSE.
#' @inheritParams account
#' @return Order `(tibble)` [Order object](https://alpaca.markets/docs/api-documentation/api-v2/orders/#order-entity) or Array of Order Objects with the following information:
#' \itemize{
#'  \item{`id`}{`(character)` order id}
#'  \item{`client_order_id`}{`(character)` client unique order id}
#'  \item{`created_at`}{`(POSIXct)` nullable When the order was created}
#'  \item{`updated_at`}{`(POSIXct)` nullable When the order was updated}
#'  \item{`submitted_at`}{`(POSIXct)` nullable When the order was submitted}
#'  \item{`filled_at`}{`(POSIXct)` nullable When the order was filled}
#'  \item{`expired_at`}{`(POSIXct)` nullable When the order was expired}
#'  \item{`canceled_at`}{`(POSIXct)` nullable When the order was canceled}
#'  \item{`failed_at`}{`(POSIXct)` nullable When the order failed}
#'  \item{`replaced_at`}{`(POSIXct)` nullable When the order was replaced}
#'  \item{`replaced_by`}{`(character)` id of replacement order}
#'  \item{`replaces`}{`(character)` id of the replaced order}
#'  \item{`asset_id`}{`(character)` asset ID}
#'  \item{`symbol`}{`(character)` Asset symbol}
#'  \item{`exchange`}{`(character)` Asset exchange}
#'  \item{`asset_class`}{`(character)` Asset class}
#'  \item{`qty`}{`(integer)` Ordered quantity}
#'  \item{`filled_qty`}{`(integer)` Filled quantity}
#'  \item{`filled_avg_price`}{`(numeric)` Filled average price}
#'  \item{`order_class`}{`(character)`}
#'  \item{`order_type`}{`(character)`}
#'  \item{`type`}{`(character)` Valid values: market, limit, stop, stop_limit}
#'  \item{`side`}{`(character)` Valid values: buy, sell}
#'  \item{`time_in_force`}{`(character)` time in force}
#'  \item{`limit_price`}{`(numeric)` Limit price}
#'  \item{`stop_price`}{`(numeric)` Stop price}
#'  \item{`status`}{`(character)` Status of the order}
#'  \item{`extended_hours`}{`(logical)` If true, eligible for execution outside regular trading hours}.
#'  \item{`legs`}{`(character)` When querying non-simple order_class orders in a nested style, an array of Order entities associated with this order. Otherwise, null.
#' }
#' @examples 
#' orders(live = FALSE)
#' orders(status = "all")
#' # For a specific ticker:
#' orders(ticker = "AAPL", status = "all")
#' @importFrom dplyr filter
#' @importFrom httr parse_url build_url GET
#' @importFrom purrr compact
#' @export
orders <- function(ticker_id = NULL, status = "open", limit = NULL, after = NULL, until = NULL, direction = "desc", silent = FALSE, live = FALSE, v = 2){
  #Set URL & Headers
  .url = httr::parse_url(get_url(live))
  headers = get_headers(live)
  .char <- tryCatch(nchar(ticker_id), error = function(e) 0)
  if (is.null(ticker_id) || .char < 15) {
    .o_id <- NULL
  } else {
    # if it's an order_id
    .o_id <- ticker_id
  }
  # yogat3ch: Create Query 2020-01-11 2157
  .url$path <- purrr::compact(list(paste0("v", v), "orders", .o_id))
  .url$query <- list(status = status,
                     limit = limit,
                     after = after,
                     until = until,
                     direction = direction)
  # Build the url
  .url <- httr::build_url(.url)
  # Query
  out <- httr::GET(.url, headers)
  # Clean
  out <-  response_text_clean(out)
  
  if (!is.null(ticker_id) && .char < 15 && length(out) != 0){       #If the ticker is not null, and not an order id then return the orders for the tickers specified.
    out = dplyr::filter(out, symbol %in% ticker_id)
  } else if (length(out) == 0) {
    #Make sure there are orders to return before calling return. 
    if(!silent) message(paste("No",status,"orders for the selected query/filter criteria at this time.","Try removing the `ticker_id` filter or setting status = 'all' to see all orders."))
    return(NULL)
  }  
  out <- orders_transform(out)
  return(out)
}
#----------------------------------------------------------------------------------------------
#UPDATED for V2
#orders(status = "all",live = TRUE, version = "v2")

#' @rdname orders
#' @title get_orders
#' @description `get_orders` is deprecated. Use \code{\link[AlpacaforR]{orders}} instead.
#' @examples get_orders()
#' @export
get_orders <- orders






# order_submit ----
# Wed Apr 22 20:23:21 2020
#' @family Orders
#' @title Submit Orders
#' 
#' @description Places a new order of the specified stock, quantity, buy / sell, type of order, time in force, and limit / stop prices if selected. See [Orders: POST](https://alpaca.markets/docs/api-documentation/api-v2/orders#request-a-new-order).
#' @param ticker \code{(character)} *required* The stock's symbol.
#' @param qty \code{(integer)} *required* The amount of shares to trade.
#' @param side \code{(character)} *required* The side of the trade. I.E "buy" or "sell"
#' @param type \code{(character)} The type of trade order. I.E "market","limit","stop","stop_limit", etc. This argument is assumed to be "market" unless a `limit`, `stop`, or both are set. If only `limit` is set, `"limit"` is assumed to be the type. If only `stop` is set, `"stop"` is assumed to be the type. If both are set `"stop_limit"` is assumed to be the type. If this argument is specified, and the requisite arguments are not set, the function will stop with error.
#' @param time_in_force \code{(character)} The time in force for the order. I.E "day", "gtc", "opg". In the V2 API, Immediate Or Cancel (IOC) & Fill or Kill (FOK) is added. Default is "day". Please see [Understand Orders](https://alpaca.markets/docs/trading-on-alpaca/orders/#time-in-force) for more info.
#' @param limit \code{(numeric)} If order type was a limit, then enter the limit price here.
#' @param stop \code{(numeric)} If order type was a stop, then enter the stop price here.
#' @param extended_hours \code{(logical)} Default: \code{FALSE}. If \code{TRUE}, order will be eligible to execute in premarket/afterhours. Only works with type limit and time_in_force day on the V2 API.
#' @param client_order_id \code{(character)} <= 48 Characters.  A unique identifier for the order. Automatically generated if not sent.
#' @param order_class \code{(character)} `'simple'`, `'bracket'`, `'oco'` or `'oto'`. For details of non-simple order classes, please see [Bracket Order Overview](https://alpaca.markets/docs/trading-on-alpaca/orders#bracket-orders). Order replacement not supported for `order_class = 'bracket'`. 
#' @param take_profit \code{(named list)} Additional parameters for take-profit leg of advanced orders:
#' \itemize{
#'  \item{\code{limit_price/l}}{ \code{numeric} **required** for bracket orders}
#' }
#' @param stop_loss \code{(named list)} Additional parameters for stop-loss leg of advanced orders:
#' \itemize{
#'   \item{\code{stop_price/s}}{ \code{numeric} **required** for bracket orders}
#'  \item{\code{limit_price/l}}{ \code{numeric} The stop-loss order becomes a stop-limit order if specified}
#' }
#' @inheritParams account
#' @inherit orders return
#' @examples 
#' # For market order (`type` is assumed to be `"market"` when `stop` and `limit` are not specified):
#' order_submit(ticker = "AAPL", qty = 100, side = "buy")
#' # Or you can submit a limit order (`type` is assumed to be `"limit"` when only `limit` is set):
#' order_submit(ticker = "AAPL", qty = 100, side = "sell", lim = 120)
#' @importFrom httr POST
#' @importFrom rlang abort `%||%`
#' @export
order_submit <- function(ticker, qty, side, type = NULL, time_in_force = "day", limit = NULL, stop = NULL, extended_hours = FALSE, client_order_id = NULL, order_class = NULL, take_profit = NULL, stop_loss = NULL, live = FALSE, v = 2){
  `%||%` <- rlang::`%||%`
  #Set URL & Headers
  url = get_url(live)
  headers = get_headers(live)
  
  # smart set the type
  if (is.null(stop_price) && !is.null(limit_price)) {
    if (type %in% c("stop", "stop_limit")) rlang::abort(paste0("Required arguments are not set for type = ",type))
    type <- "limit"
  } else if (!is.null(stop_price) && is.null(limit_price)) {
    if (type %in% c("limit", "stop_limit")) rlang::abort(paste0("Required arguments are not set for type = ",type))
    type <- "stop"
  } else if (!is.null(stop_price) && !is.null(limit_price)) {
    type <- "stop_limit"
  } else if (!is.null(order_class)) {
    if (order_class %in% c("bracket", "oco", "oto")) {
      .o <- list(take_profit.limit_price = take_profit$l, stop_loss.stop_price = stop_loss$s, stop_loss.limit_price = stop_loss$l)
      .o <- purrr::imap(.o[1:2], ~{
        if (is.null(.x)) paste0(.y, " is missing.") else NULL
      })
      # parameter parsing, error checking & warnings for advanced orders
      if (order_class == "bracket") {
        if (!is.null(extended_hours) && isTRUE(extended_hours)) {
          rlang::warn(paste0("Extended hours not supported, changing to extended_hours to FALSE."))
          extended_hours <- F
        }
        if (!time_in_force %in% c("day","gtc")) {
          rlang::abort("time_in_force must be 'day' or 'gtc'. See documentation for details.")
        }
        
        if ((is.null(take_profit$l) || is.null(stop_loss$l))) {
          rlang::abort(paste0(.o[[1]],.o[[2]]))
        } 
      } else if (order_class == "oco") {
        if (type != "limit") {
          rlang::warn("'oco' orders must be type = 'limit'. changing type to 'limit'.")
          type <- "limit"
        }
        if ((is.null(take_profit$l) || is.null(stop_loss$l))) {
          rlang::abort(paste0(.o[[1]],.o[[2]]))
        }
      } else if (order_class == "oto") {
        if (is.null(take_profit) && is.null(stop_loss)) {
          rlang::abort("Either take_profit or stop_loss must have at least one parameter set when order_class = 'oto'")
        }
      }
    }
  }
  #Convert ticker argument to upper if lower
  if(grepl("^[[:lower:]]+$",ticker)){ticker <- toupper(ticker)}
  
  #Create body with order details, most common is a named list 
  bodyl <- list(symbol=ticker, qty=qty, side = side, type = type, time_in_force = time_in_force, limit_price = limit_price, stop_price = stop_price)
  bodyl <- lapply(bodyl, as.character)
  bodyl$extended_hours <- extended_hours
  
  #Send Request
  out = httr::POST(url = paste0(url,"/",paste0("v",v),"/orders"), body = bodyl, encode = "json",headers)
  out = response_text_clean(out)
  out <- orders_transform(out)
  return(out)
}
#----------------------------------------------------------------------------------------------
#UPDATED for V2
#' @family Orders
#' @title submit_orders
#' @rdname orders_submit
#' @description `submit_orders` is deprecated. Use \code{\link[AlpacaforR]{order_submit}} instead.
#' @export
submit_orders <- order_submit







# order_cancel ----
# Wed Apr 22 20:22:55 2020
#' @family Orders
#' @title Cancel Order function
#' 
#' @description Cancels any open order by order_id or all open orders. Use "cancel_all" as the order_id to cancel all open orders. See [Orders: DELETE](https://alpaca.markets/docs/api-documentation/api-v2/orders#cancel-all-orders) in the Alpaca API V2 documentation for details.
#' @param ticker_id `(character)` The ticker symbol(s) or order ID(s). Use `"cancel_all"` to cancel all open orders.
#' @inheritParams account
#' @return Order `(tibble)` with details about canceled orders. This function places the request to cancel an order. In most cases the order is canceled instantly, but sometimes it's not. Connect to the [Alpaca Streaming API](https://alpaca.markets/docs/api-documentation/api-v2/streaming/) via the websocket functions to receive order updates (See \code{\link[AlpacaforR]{ws_create}}). 
#' @return Query `(list)` attribute accessed via `attr(df, "query")` on the returned `df` object which contains:
#' \itemize{
#'  \item{`id`}{`(character)` vector of cancel order ids}
#'  \item{`status`}{`(integer)` vector of cancel order statuses}
#' } 
#' @inherit orders return
#' @examples 
#' # Cancel by the order id
#' order <- order_submit("AAPL", 1, "buy", "market", live = F)
#' order_cancel(order_id = order$id, live = F)
#' # Or cancel all orders for a ticker symbol See also: positions:
#' order_submit("AAPL", 1, "buy", "market", live = F)
#' order_submit("AMZN", 1, "buy", "market", live = F)
#' canceled_orders <- order_cancel(c("AAPL", "AMZN"))
#' str(canceled_orders)
#' # Or cancel all orders:
#' order_submit("AAPL", 1, "buy", "market", live = F)
#' order_submit("AMZN", 1, "buy", "market", live = F)
#' order_submit("FB", 1, "buy", "market", live = F)
#' canceled_orders <- order_cancel("cancel_all")
#' str(canceled_orders)
#' @importFrom httr DELETE parse_url build_url
#' @importFrom purrr compact pmap map 
#' @importFrom dplyr bind_rows
#' @seealso positions
#' @export
order_cancel <- function(ticker_id = NULL, live = FALSE, v = 2){
  #Set URL & Headers
  .url = httr::parse_url(get_url(live))
  headers = get_headers(live)
  if (is.null(ticker_id)) stop("order_id is required.")
  
  #Gather the open order ID for the symbol specified
  open_orders = orders(status = "open", live = live, silent = TRUE)
  
  
  #Check if any open orders before proceeding. 
  if(is.null(open_orders)){
    message("There are no orders to cancel at this time.")
    return(NULL)
  } else if (any(ticker_id %in% "cancel_all")) { #If order id is cancel_all
    message(paste0("Canceling ALL ", nrow(open_orders)," open orders"))
  } 
  
  
  # Create a list of order_ids or calls
  .o_ids <- purrr::map(ticker_id, ~{
    if (.x == "cancel_all") {
      .o <- NULL
    } else if (nchar(.x) > 15) { #If order id supplied
      .o <- .x
    } else { # if ticker symbol is supplied
      .o <- open_orders$id[open_orders$symbol %in% .x]
    }
    return(.o)
  })
  if (is.list(.o_ids) && !is.null(.o_ids[[1]])) {
    .calls <- do.call(c, .o_ids)
  } else {
    .calls <- .o_ids
  }
  .calls <- purrr::map_chr(.calls, ~{
    .url$path <- purrr::compact(list(
      paste0("v",v),
      "orders",
      .x
    ))
    .url <- httr::build_url(.url)
  })
  
  .q <- new.env()
  .q$.query <- list()
  
  .out <- purrr::map2(.calls, .o_ids, ~{  
    cancel = httr::DELETE(url = .x, headers)
    browser()
    cancel <- tryCatch({response_text_clean(cancel)}, error = function(e) NULL)
    if (!is.null(cancel)) {
      # if the order was pending, this endpoint returns data. If the order was new, it doesn't. This generates informative results regardless
      .out <- orders_transform(cancel$body)
      .q$.query <- append(cancel[1:2], get0(".query", envir = .q, ifnotfound = NULL))
    } else {
      .out <- orders(ticker_id = .y)
    }
    # Coerce dates to R objects
    
    purrr::pmap(.out, ~{
      .vars <- list(...)
      message(paste0("Canceling ",.vars$side," order ", .vars$client_order_id, " for ", .vars$symbol, " created at ", .vars$created_at, " Qty: ", .vars$qty, " as of ", .vars$updated_at))
    })
    return(.out)
  })
  .out <- dplyr::bind_rows(.out)
  attr(.out, "query") <- .q$.query
  return(.out)
}
#' @family Orders
#' @rdname order_cancel
#' @title cancel_orders
#' @description `cancel_orders` is deprecated. Use \code{\link[AlpacaforR]{order_cancel}} instead.
#' @examples cancel_orders()
#' @export
cancel_orders <- order_cancel


# order_replace ----
# Wed Apr 22 20:22:23 2020
#' @family Orders
#' @title Replace Order Details
#' 
#' @description Cancels any open order by either ticker or order id. If multiple open orders exist for one ticker, then the default is to cancel the most recent order. Useful for updating unprocessed market orders placed outside market hours, or for updating limit, and stop orders. See [Orders: PATCH](https://alpaca.markets/docs/api-documentation/api-v2/orders#replace-an-order) for details.
#' @param ticker_id `(character)` The ticker symbol or the order id.
#' @param qty `(integer)` The amount of shares to replace. Defaults to orders existing quantity.
#' @param time_in_force `(character)` The type of time order. I.E day, gtc, opg, cls, ioc, fok. Defaults to the replaced order's existing time in force. Please see [Understand Orders](https://alpaca.markets/docs/trading-on-alpaca/orders/#time-in-force) for more info.
#' @param limit_price `(numeric)` If order type was a limit, then enter the limit price here. Defaults to replaced order's existing limit price (if applicable.)
#' @param stop_price `(numeric)` If order type was a stop, then enter the stop price here. Defaults to replaced order's existing stop price (if applicable.)
#' @inheritParams account
#' @inherit orders return
#' @examples
#' \dontrun{
#' # Set a stop_loss and update it. Only works during market hours
#' # Create an order
#' o <- order_submit("BYND", 1, "buy", "market")
#' # Give the order a moment to process
#' Sys.sleep(10) 
#' # Get the filled price
#' os <- orders(status = "filled")
#' filled_price <- os[os$id %in% o$id, "filled_avg_price", drop = T]
#' # Set the stop loss at 5% less than the purchase price
#' stop_loss <- order_submit("BYND", 1, "sell", "stop", stop_price = filled_price * .95)
#' # Update that to 10% less than the purchase price
#' stop_loss2 <- order_replace(stop_loss$id, stop_price = filled_price * .9)
#' }
#' @importFrom httr PATCH
#' @importFrom rlang warn `%||%`
#' @importFrom purrr map_if
#' @export
order_replace <- function(ticker_id, qty = NULL, time_in_force = "day", limit_price = NULL, stop_price=NULL, live = FALSE, v = 2){
  #Set URL & Headers
  url = get_url(live)
  headers = get_headers(live)
  
  open_orders = orders(status = "open", live = live, silent = TRUE)
  .oo <- tryCatch(nrow(open_orders), error = function(e) 0)
  if (nchar(ticker_id) > 15 && .oo > 0) { #If order id supplied then do this
    order_id <- ticker_id
    ticker <- open_orders$symbol[open_orders$id %in% order_id]
  } else if (.oo > 0) { #If ticker supplied then do this
    #Gather the open order ID for the symbol specified
    ticker <- ticker_id 
    open_orders_sym <- grep(ticker, open_orders$symbol, ignore.case = T)
    #If more than one order is open print message to notify which order is being cancelled
    #if(length(open_orders_sym) > 1) message(paste0("More than one order open for ",ticker,", the order placed at ", lubridate::with_tz(as.POSIXlt(open_orders$submitted_at[open_orders_sym[1]], tz = "UTC", tryFormats = c("%Y-%m-%dT%H:%M:%OS")), Sys.timezone())," will be canceled"))
    order_id <- open_orders[[open_orders_sym[1], "id"]]
  } else {
    stop("No open orders to replace.")
  }
  
  # Preserve the current parameters if unset
  if (.oo > 0) {
    .qty <- qty %||% open_orders$qty[open_orders$id %in% order_id] %||% NULL
    .time_in_force <- time_in_force %||% open_orders$time_in_force[open_orders$id %in% order_id] %||% NULL
    .limit_price <- limit_price %||% open_orders$limit_price[open_orders$id %in% order_id] %||% NULL
    .stop_price <- stop_price %||% open_orders$stop_price[open_orders$id %in% order_id] %||% NULL
  }
  
  #Send Request & Cancel the order through the order_id
  
  #Create body with order details, most common is a named list 
  bodyl <- purrr::map_if(list(qty = .qty, time_in_force = .time_in_force, limit_price = .limit_price, stop_price = .stop_price), is.na, ~{NULL})
  replace = httr::PATCH(url = paste0(url,"/",paste0("v",v),"/orders/",order_id), body = bodyl, encode = "json", headers)
  replace = response_text_clean(replace)
  if(TRUE %in% grepl(pattern = "^4", x = replace$status_code %||% 200)){
    rlang::warn(paste("Order ID", order_id,"for",ticker, "was not replaced.\n Message:", replace$message))
  } else {
    message(paste("Order", replace$id,"for", replace$symbol, "successfully replaced", replace$replaces,"\n"))
    replace <- orders_transform(replace)
  }
  return(replace)
}
#----------------------------------------------------------------------------------------------
#' @family Orders
#' @title replace_orders
#' @rdname order_replace
#' @description `replace_orders` is deprecated. Use \code{\link[AlpacaforR]{order_replace}}
#' @examples replace_orders()
#' @export
replace_orders <- order_replace

