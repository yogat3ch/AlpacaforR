
# orders ----
# Wed Apr 22 20:23:46 2020
#' @family Orders
#' @title Get Orders
#' 
#' @description The orders API allows a user to monitor, place and cancel their orders with Alpaca. Times are returned as yyyy-mm-dd hh-mm-ss POSIXct, quantity and price as numeric, and all others as a string. See [Orders GET](https://alpaca.markets/docs/api-documentation/api-v2/orders#get-a-list-of-orders)for more details.
#' @param ticker_id \code{(character)} Specify symbol, order ID, or client_order_id (must set `client_order_id = TRUE`).
#' @param status \code{(character)} Order status to be queried. \code{open}, \code{closed} or \code{all}. Defaults to open as a string.
#' @param limit \code{(integer)} The maximum number of orders in response. Defaults to 50 and max is 500.
#' @param after \code{(Date/character)} Date in YYYY-MM-DD \href{https://www.iso.org/iso-8601-date-and-time-format.html}{(ISO8601 Format)} The response will include only orders submitted \emph{after} this date exclusive as a timestamp object.
#' @param until \code{(Date/character)} Date in YYYY-MM-DD \href{https://www.iso.org/iso-8601-date-and-time-format.html}{(ISO8601 Format)} The response will include only orders submitted \emph{before} this date exclusive as a timestamp object.
#' @param direction \code{(character)} The chronological order of response based on the submission time. \code{'asc'} or \code{'desc'}. Defaults to \code{desc}.
#' @param nested \code{(logical)} If true, the result will roll up multi-leg orders under the legs field of primary order. Default `TRUE`.
#' @param client_order_id \code{(logical)} Whether `ticker_id` is a client_order_id, defaults to `NULL (FALSE)`
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
#' # For a specific ticker:
#' orders(ticker = "AAPL", status = "all")
#' @importFrom dplyr filter
#' @importFrom httr parse_url build_url GET
#' @importFrom purrr compact
#' @export
orders <-
  function(ticker_id = NULL,
           status = "open",
           limit = NULL,
           after = NULL,
           until = NULL,
           direction = "desc",
           client_order_id = NULL,
           nested = T,
           live = as.logical(Sys.getenv("APCA-LIVE", FALSE)),
           v = 2) {
    
  #Set URL & Headers
  .url = httr::parse_url(get_url(live))
  headers = get_headers(live)
  # set status if abbreviated
  status <- c(o = "open", c = "closed", a = "all")[tolower(substr(status, 0, 1))]
  # check if id
  .is_id <- is_id(ticker_id)
  if (.is_id) {
    # if it's an order_id
    .o_id <- ticker_id
  } else {
    # if its a ticker symbol
    .o_id <- NULL
    ticker_id <- toupper(ticker_id)
  }
  if (isTRUE(client_order_id)) {
    .url$path <- purrr::compact(list(paste0("v", v), "orders:by_client_order_id"))
  } else if (isFALSE(client_order_id) || is.null(client_order_id)) {
    .url$path <- purrr::compact(list(paste0("v", v), "orders", .o_id))
  }
  # yogat3ch: Create Query 2020-01-11 2157
  
  if (isTRUE(client_order_id)) {
    .url$query <- list(client_order_id = .o_id)
  } else {
    .url$query <- list(status = status,
                       limit = limit,
                       after = after,
                       until = until,
                       direction = direction,
                       nested = nested)
  }
  # Build the url
  .url <- httr::build_url(.url)
  if (isTRUE(get0(".dbg", envir = .GlobalEnv, mode = "logical", inherits = F))) message(paste0(.url))
  # Query
  out <- httr::GET(.url, headers)
  # Clean
  out <- order_transform(out)
  
  
  if (isTRUE(inherits(ticker_id, "character") && nchar(ticker_id) > 0) && !.is_id && length(out) != 0){       #If the ticker is a character string but not an order id, and results came through then return the orders for the tickers specified.
    out = dplyr::filter(out, symbol %in% ticker_id)
  }
  return(out)
}
#----------------------------------------------------------------------------------------------
#UPDATED for V2
#orders(status = "all",live = TRUE, version = "v2")
#' @family Orders
#' @rdname orders
#' @title get_orders
#' @description `get_orders` is deprecated. Use \code{\link[AlpacaforR]{orders}} instead.
#' @examples get_orders()
#' @export
get_orders <- orders






# order_submit ----
# Wed Apr 22 20:23:21 2020
#' @family Orders
#' @title Submit, Cancel & Replace Orders, 
#' @description Places/Replaces/Cancels an order, or cancels all orders depending on argument to `action`. See parameter documentation and [Orders](https://alpaca.markets/docs/api-documentation/api-v2/orders) for details.Depending on the `action` specified, some arguments are required:
#' \itemize{
#'  \item{\code{action = 'submit'}}{ All arguments can be submitted. See Arguments for which are *required*.}
#'  \item{\code{action = 'replace'}}{ `qty`, `time_in_force`, `limit`, `stop`, `client_order_id` are all eligible. Only one is *required*.}
#'  \item{\code{action = 'cancel'}}{ Only `ticker_id` is *required*.}
#'  \item{\code{action = 'cancel_all'}}{ No arguments necessary.}
#'  }
#' @param ticker_id \code{(character)}  The stock symbol (*Required* when `action = "submit"`) or Order object/ Order ID (*Required* when `action = "cancel"/"replace"`). 
#' To expedite the setting of stops and limits for open positions, an Order ID from a `'buy'` order can be provided when `action = "submit"` to place a `'sell'` order with the following parameters such that they do not need to be set manually:
#' \itemize{
#'   \item{\code{side = 'sell'}}
#'   \item{If \code{qty} is not provided, it will be populated from the buy order}
#'   \item{`ticker_id` will be set to the symbol from the buy order.}
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
#' @param client_order_id \code{(character)}  <= 48 Characters.  A unique identifier for the order. Automatically generated if not sent. 
#' \itemize{
#'   \item{\code{`action = 'replace'/'submit'`}}{ *Optional* character vector}
#'  \item{\code{`action = 'submit'`}}{ If an Order ID is provided to `ticker_id`, this can be a \code{(logical)} to indicate whether the `client_order_id` for the sell order should be set as the Order ID provided from the buy order to effectively link the associated buy & sell orders for your records.}
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
#' @param trail_percent \code{(numeric)} a percent value away from the highest water mark. If you set this to 1.0 for a sell trailing stop, the stop price is always hwm * 0.99
#' @inheritParams account
#' @inherit orders return
#' @examples 
#' # most orders (except limit) must be placed during market hours or they will not be filled until the next trading day. 
#' .c <- clock()
#' (bo <- order_submit("AAPL", qty = 1, side = "buy", type = "market"))
#' # Or you can submit a limit order (`type` is assumed to be `"limit"` when only `limit` is set):
#' (so <- order_submit("AAPL", q = 1, side = "s", lim = 120))
#' if (.c$is_open) {
#' # cancel an order with `action = "cancel"`. ticker_id can be either the id of the order to cancel or the order tbl object.
#' order_submit(so, a = "c")
#' # expedite a simple "sell" order by providing the id of a buy order. This can be linked to it's original buy order on the Alpaca side via the `client_order_id` by simply setting `client_order_id = T`
#' (so <- order_submit(bo$id, stop = 120, cli = TRUE)) # here the id is used
#' so$client_order_id == bo$id
#' # replace `"r"` parameters for simple orders (Alpaca devs are working on replacement for complex orders as of 2020-05-06)
#' order_submit(so$id, a = "r", stop = 123)
#' # Complex orders can be submitted as well
#' # Here is an example of setting a "bracket" order
#' # first retrieve the going price
#' (.lq <- polygon("lq", symbol = "AMZN"))
#' # sell if the price moves up 3% by setting `take_profit`
#' tp <- list(l = .lq$askprice * 1.03)
#' # hedge risk by setting a stop if it loses 5% and limit if it loses 6% with `stop_loss`
#' sl <- list(l = .lq$askprice * .94, s = .lq$askprice * .95)
#' # note that the names of these list items can be partial
#' (br_o <- order_submit("AMZN", order_class = "bracket", qty = 2, take_profit = tp, stop_loss = sl))
#' # side is assumed to be buy, and type is assumed to be market
#' # Set a trailing stop by price
#' m_o <- order_submit("AMZN", side = "buy", type = "market", qty = 1)
#' ts_o <- order_submit(m_o$id, trail_price = 30)
#' # Set a trailing stop by percent
#' m_o <- order_submit("AMZN", side = "buy", type = "market", qty = 1)
#' ts_o <- order_submit(m_o$id, trail_percent = 5)
#' }
#' # all open orders can be canceled with `action = "cancel_all"`
#' order_submit(a = "cancel_all")
#' @importFrom httr POST PATCH DELETE parse_url build_url
#' @importFrom rlang abort list2 `!!`
#' @importFrom purrr modify_depth compact
#' @importFrom jsonlite toJSON
#' @export

order_submit <-
  function(ticker_id,
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
           live = as.logical(Sys.getenv("APCA-LIVE", FALSE)),
           v = 2) {
    
  #Set URL & Headers
  .url = httr::parse_url(get_url(live))
  headers = get_headers(live)
  # NOTE: ticker_id is not referenced when action = "cancel_all"
  if (!grepl("cancel_all", action, ignore.case = T)) {
    action <- substr(tolower(action), 0, 1)
    # if the order tbl is supplied directly, extract the id
    if (!inherits(ticker_id, c("character"))) ticker_id <- ticker_id$id
    # if vector length 2 and duplicated (complex orders), remove the dupes
    if (any(duplicated(ticker_id))) ticker_id <- ticker_id[!duplicated(ticker_id)]
  } else if (grepl("cancel_all", action, ignore.case = T)) {
    action <- tolower(action)
  }
    
  # change args in the event of trailing stop
  if (!missing(trail_price) || !missing(trail_percent)) {
    type <- "trailing_stop"
  }
  if (type == "trailing_stop") {
    .mc <- match.call()
    stop_price <- grep("^trail", names(.mc), value = TRUE)
    stop <- .mc[[stop_price]]
  } else {
    stop_price <- "stop_price"
  }
  
  # smart detect: type, order_class, extended_hours
  # fix names for take_profit, stop_loss if partialed
  # or throw errors/warnings for specific criteria
  if (any(action %in% c("s", "r", "c"))) {
    ot <- order_check(environment()) 
    list2env(ot, envir = environment())
  }
  
  .is_id <- is_id(ticker_id)
  # detect the argument provided to ticker_id
  if (action == "s") {
  
    #Create body with order details if action is submit or replace
    bodyl <-
        append(purrr::modify_depth(purrr::compact(
          rlang::list2(
            symbol = ticker_id,
            qty = qty,
            side = side,
            type = type,
            time_in_force = time_in_force,
            limit_price = limit,
            !!stop_price := stop,
            client_order_id = client_order_id,
            order_class = order_class
            )
        ),-1, .f = as.character),
        purrr::modify_depth(purrr::compact(list(take_profit = take_profit,
        stop_loss = stop_loss)), -1, .f = as.character))
    
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
  # create the base url
  .url$path <- list(paste0("v",v), "orders")
  if (action %in% c("r","c")) {
    # if replacing or canceling, append the order ID
    if (!.is_id) rlang::abort(paste0("`ticker_id` is not an Order ID."))
    .url$path <- append(.url$path, ticker_id)
  }
  .url <- httr::build_url(.url)
  if (action  == "s") {
    out = httr::POST(url = .url, body = bodyl, encode = "raw", headers)
  } else if (action == "r") {
    out = httr::PATCH(url = .url, body = bodyl, encode = "raw", headers)
  } else if (action %in% c("c","cancel_all")) {
    out <- httr::DELETE(url = .url, headers)
  }
  out <- order_transform(out)
  return(out)
}
#----------------------------------------------------------------------------------------------
#UPDATED for V2
#' @family Orders
#' @title submit_orders
#' @rdname order_submit
#' @description `submit_orders` is deprecated. Use \code{\link[AlpacaforR]{order_submit}} instead.
#' @export
submit_orders <- order_submit







# order_cancel ----
# Wed Apr 22 20:22:55 2020
#' @family Orders
#' @title Cancel Order function (Deprecated)
#' 
#' @description order_cancel is Deprecated. Use \code{\link[AlpacaforR]{order_submit}} with `action = "c"` or `action = "cancel_all"`. Cancels any open order by order_id or all open orders. Use "cancel_all" as the order_id to cancel all open orders. See [Orders: DELETE](https://alpaca.markets/docs/api-documentation/api-v2/orders#cancel-all-orders) in the Alpaca API V2 documentation for details.
#' @param ticker_id `(character)` The ticker symbol(s) or order ID(s). Use `"cancel_all"` to cancel all open orders.
#' @inheritParams account
#' @return Order `(tibble)` with details about canceled orders. This function places the request to cancel an order. In most cases the order is canceled instantly, but sometimes it's not. Connect to the [Alpaca Streaming API](https://alpaca.markets/docs/api-documentation/api-v2/streaming/) via the websocket functions to receive order updates (See \code{\link[AlpacaforR]{ws_create}}). 
#' @return Query `(list)` attribute accessed via `attr(df, "query")` on the returned `df` object which contains:
#' \itemize{
#'  \item{`id`}{`(character)` vector of cancel order ids}
#'  \item{`status`}{`(integer)` vector of cancel order statuses}
#' } 
#' @inherit orders return
#' @importFrom httr DELETE parse_url build_url
#' @importFrom purrr compact pmap map 
#' @importFrom dplyr bind_rows
#' @seealso positions
#' @export

order_cancel <- function(ticker_id = NULL, live = FALSE, v = 2){
  #Set URL & Headers
  .url = httr::parse_url(get_url(live))
  headers = get_headers(live)
  message("`cancel_order` & `order_cancel` are deprecated. Please use `order_submit` with `action = 'cancel'/'cancel_all' instead.`")
  return(NULL)
  if (is.null(ticker_id)) stop("order_id is required.")
  #Gather the open order ID for the symbol specified
  open_orders = orders(status = "open", live = live)
  .nrow <- tryCatch({nrow(open_orders)}, error = function(e) 0)
  
  #Check if any open orders before proceeding. 
  if(is.null(open_orders) || .nrow == 0){
    message("There are no orders to cancel at this time.")
    return(NULL)
  } else if (any(ticker_id %in% "cancel_all") && .nrow > 0) { #If order id is cancel_all
    message(paste0("Canceling ALL ", nrow(open_orders)," open orders"))
  } 
  
  .is_id <- is_id(ticker_id)
  if (!.is_id) ticker_id <- toupper(ticker_id)
  # Create a list of order_ids or calls
  .o_ids <- purrr::map(ticker_id, ~{
    if (.x == "cancel_all") {
      .o <- NULL
    } else if (.is_id) { #If order id supplied
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
    cancel <- tryCatch({response_text_clean(cancel)}, error = function(e) NULL)
    if (!is.null(cancel)) {
      # if the order was pending, this endpoint returns data. If the order was new, it doesn't. This generates informative results regardless
      .out <- order_transform(cancel$body)
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
#' @rdname order_submit
#' @title cancel_orders
#' @description `cancel_orders` is deprecated. Use \code{\link[AlpacaforR]{order_submit}} with `action = "c"` or `action = "cancel_all"`.
#' @examples cancel_orders()
#' @export
cancel_orders <- order_cancel


# order_replace ----
# Wed Apr 22 20:22:23 2020
#' @family Orders
#' @title Replace Order Details (Deprecated)
#' @description Use \code{\link[AlpacaforR]{order_submit}} with `action = "replace"`. Cancels any open order by either ticker or order id. If multiple open orders exist for one ticker, then the default is to cancel the most recent order. Useful for updating unprocessed market orders placed outside market hours, or for updating limit, and stop orders. See [Orders](https://alpaca.markets/docs/api-documentation/api-v2/orders#replace-an-order) for details.
#' @param ticker_id `(character)` The ticker symbol or the order id.
#' @param qty `(integer)` The amount of shares to replace. Defaults to orders existing quantity.
#' @param time_in_force `(character)` The type of time order. I.E day, gtc, opg, cls, ioc, fok. Defaults to the replaced order's existing time in force. Please see [Understand Orders](https://alpaca.markets/docs/trading-on-alpaca/orders/#time-in-force) for more info.
#' @param limit_price `(numeric)` If order type was a limit, then enter the limit price here. Defaults to replaced order's existing limit price (if applicable.)
#' @param stop_price `(numeric)` If order type was a stop, then enter the stop price here. Defaults to replaced order's existing stop price (if applicable.)
#' @inheritParams account
#' @inherit orders return
#' @examples
#' if (rlang::is_interactive()) {
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
order_replace <- function(ticker_id, qty = NULL, time_in_force = "day", limit_price = NULL, stop_price = NULL, live = FALSE, v = 2){
  #Set URL & Headers
  message("`replace_order` & `order_replace` are deprecated. Please use `order_submit` with `action = 'replace'`")
  return(NULL)
  url = get_url(live)
  headers = get_headers(live)
  
  open_orders = orders(status = "open", live = live)
  .oo <- tryCatch(nrow(open_orders), error = function(e) 0)
  .is_id <- is_id(ticker_id)
  if (.is_id && .oo > 0) { #If order id supplied then do this
    order_id <- ticker_id
    ticker <- open_orders$symbol[open_orders$id %in% order_id]
  } else if (.oo > 0) { #If ticker supplied then do this
    #Gather the open order ID for the symbol specified
    ticker <- ticker_id 
    open_orders_sym <- grep(ticker, open_orders$symbol, ignore.case = TRUE)
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
    replace <- order_transform(replace)
  }
  return(replace)
}
#----------------------------------------------------------------------------------------------
#' @family Orders
#' @title replace_orders (Deprecated)
#' @description Use \code{\link[AlpacaforR]{order_submit}} with \code{action = "replace"}.
#' @rdname order_replace
#' @examples replace_orders()
#' @export
replace_orders <- order_replace

