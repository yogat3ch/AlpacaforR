

# PACKAGE FUNCTIONS #
#===================================================================================================



#Exported Functions:  Sun Jan 12 10:20:51 2020 ----

#Functions to interact with Alpaca API
#----------------------------------------------------------------------------------------------
#' Get Account function
#'
#' The accounts API serves important information related to an account, including account status, funds available for trade, funds available for withdrawal, and various flags relevant to an account’s ability to trade.
#' @param live TRUE / FALSE if you are connecting to a live account. Default to FALSE, so it will use the paper url if nothing was provided.
#' @return "id" Account ID as a string.
#' @return "status" Account Status as a string.
#' @return "currency" USD as a string.
#' @return "buying_power" Tradable buying power as a string.
#' @return "cash" Cash balance as a string.
#' @return "cash_withdrawable"  Withdrawable cash amount as a string.
#' @return "portfolio_value"  Total value of cash + holding positions as a string.
#' @return "trading_blocked"  If true, the account is not allowed to place orders as a boolean.
#' @return "transfers_blocked"  If true, the account is not allowed to request money transfers as a boolean.
#' @return "account_blocked"  If true, the account activity by user is prohibited as a boolean.
#' @return "created_at"  Timestamap this account was created at as a string.
#' @examples 
#' get_account(live = FALSE, version = "v2")
#' # Which is similar to:
#' get_account()
#' # For access to live accounts, you must submit as live = TRUE
#' get_account(live = TRUE, version = "v2")
#' @export
get_account <- function(live = FALSE, version = "v2"){
  #Set URL & Headers
  url = get_url(live)
  headers = get_headers(live)
  
  #Send Request
  account = httr::GET(url = paste0(url,"/",version,"/account"), headers)
  account = response_text_clean(account)
  return(account)
}
#----------------------------------------------------------------------------------------------
#UPDATED for V2
#get_account(live = TRUE, version = "v2")







#----------------------------------------------------------------------------------------------
#' Get Account Configurations function
#'
#' The account configuration API serves an important role in setting the way you want.......
#' @param live TRUE / FALSE if you are connecting to a live account. Default to FALSE, so it will use the paper url if nothing was provided.
#' @return "dtbp_check" both, entry, or exit. Controls Day Trading Margin Call (DTMC) checks.
#' @return "trade_confirm_email" all or none. If none, emails for order fills are not sent.
#' @return "suspend_trade" If true, new orders are blocked.
#' @return "no_shorting" If true, account becomes long-only mode.
#' @examples 
#' get_config(live = FALSE)
#' # Which is similar to:
#' get_config()
#' # For access to live accounts, you must submit as live = TRUE
#' get_config(live = TRUE)
#' @export
get_config <- function(live = FALSE){
  #Set URL & Headers
  url = get_url(live)
  headers = get_headers(live)
  
  #Send Request
  account_config = httr::GET(url = paste0(url,"/v2/account/configurations"), headers)
  account_config = response_text_clean(account_config)
  return(account_config)
}
#----------------------------------------------------------------------------------------------
#NEW for V2
#get_config(live = TRUE)









#----------------------------------------------------------------------------------------------
#' Send Account Configurations function
#' 
#' 
#' @param live TRUE / FALSE if you are connecting to a live account. Default to FALSE, so it will use the paper url if nothing was provided.
#' @return The response from the server after setting the configuration.
#' @examples 
#' set_config(live = FALSE, dtbp_check = "entry", no_shorting = FALSE, suspend_trade = FALSE, trade_confirm_email = "all")
#' # Which is similar to:
#' set_config(dtbp_check = "entry", no_shorting = FALSE, suspend_trade = FALSE, trade_confirm_email = "all")
#' # For access to live accounts, you must submit as live = TRUE
#' set_config(live = TRUE, dtbp_check = "entry", no_shorting = FALSE, suspend_trade = FALSE, trade_confirm_email = "all")
#' @export
set_config <- function(live = FALSE, dtbp_check = "entry", no_shorting = FALSE, suspend_trade = FALSE, trade_confirm_email = "all"){
  #Set URL & Headers
  url = get_url(live)
  headers = get_headers(live)
  
  
  
  #Create body with order details, most common is a named list 
  bodyl <- list(dtbp_check = dtbp_check, no_shorting = no_shorting, suspend_trade = suspend_trade, trade_confirm_email = trade_confirm_email)
  
  #Send Request
  set_account_config = httr::PATCH(url = paste0(url,"/v2/account/configurations"), body = bodyl, encode = "json", headers)
  set_account_config = response_text_clean(set_account_config)
  return(set_account_config)
}
#----------------------------------------------------------------------------------------------
#NEW for V2
#set_config(live = TRUE, dtbp_check = "entry", no_shorting = FALSE, suspend_trade = FALSE, trade_confirm_email = "all")








#----------------------------------------------------------------------------------------------
#' Get Positions function
#'
#' The positions API provides information about an account’s current open positions. The response will include information such as cost basis, shares traded, and market value, which will be updated live as price information is updated. Character values are returned as a string while integer values are returned as numeric.
#' @param ticker Specify which symbol you want to call by inserting ticker as a string.
#' @param live TRUE / FALSE if you are connecting to a live account. Default to FALSE, so it will use the paper url if nothing was provided.
#' @return "asset_id"  Asset ID.
#' @return "symbol"  Symbol of the asset.
#' @return "exchange"  Exchange name of the asset.
#' @return "asset_class"  Asset class name.
#' @return "avg_entry_price"  Average entry price of the position.
#' @return "qty" The number of shares.
#' @return "side" long/short exposure.
#' @return "market_value"  Total dollar amount of the position.
#' @return "cost_basis"  Total cost basis in dollar.
#' @return "unrealized_pl"  Unrealized profit/loss in dollar.
#' @return "unrealized_plpc"  Unrealized profit/loss percent (by a factor of 1).
#' @return "unrealized_intraday_pl"  Unrealized profit/loss in dollar for the day.
#' @return "unrealized_intraday_plpc"  Unrealized profit/loss percent (by a factor of 1).
#' @return "current_price"  Current asset price per share.
#' @return "lastday_price"  Last day’s asset price per share.
#' @return "change_today"  Percent change from last day price (by a factor of 1).
#' @examples 
#' get_positions(ticker = "AAPL", live = FALSE, version = "v2")
#' get_positions(ticker = "AAPL", live = TRUE, version = "v2")
#' # This gets all positions:
#' get_positions(version = "v2")
#' get_positions(live = TRUE, version = "v2")
#' @import magrittr
#' @export
get_positions <- function(ticker = NULL, live = FALSE, version = "v2"){
  #Set URL, live = FALSE & Headers
  url = get_url(live)
  headers = get_headers(live)
  
  #Send Request
  positions = httr::GET(url = paste0(url,"/",version,"/positions"), headers) 
  positions = response_text_clean(positions)
  
  
  #Check if any positions exist before attempting to return
  if(length(positions) == 0) cat("No positions are open at this time. \n")
  else if(is.null(ticker)){
    positions[,c(5:6,8:ncol(positions))] %<>% purrr::map_dfc(as.numeric)
    return(positions)
  } else {
    positions[,c(5:6,8:ncol(positions))] %<>% purrr::map_dfc(as.numeric)
    positions <- subset(positions,symbol == ticker)
    return(positions)
  }
}
#----------------------------------------------------------------------------------------------
#UPDATED for V2
#get_positions(live = TRUE,version = "v2")










#----------------------------------------------------------------------------------------------
#' Close all positions function
#'
#' The close all positions API liquidates all currently open long and short positions. Character values are returned as a string while integer values are returned as numeric.
#' @param live TRUE / FALSE if you are connecting to a live account. Default to FALSE, so it will use the paper url if nothing was provided.
#' @return "asset_id"  Asset ID.
#' @return "symbol"  Symbol of the asset.
#' @return "exchange"  Exchange name of the asset.
#' @return "asset_class"  Asset class name.
#' @return "avg_entry_price"  Average entry price of the position.
#' @return "qty" The number of shares.
#' @return "side" long/short exposure.
#' @return "market_value"  Total dollar amount of the position.
#' @return "cost_basis"  Total cost basis in dollar.
#' @return "unrealized_pl"  Unrealized profit/loss in dollar.
#' @return "unrealized_plpc"  Unrealized profit/loss percent (by a factor of 1).
#' @return "unrealized_intraday_pl"  Unrealized profit/loss in dollar for the day.
#' @return "unrealized_intraday_plpc"  Unrealized profit/loss percent (by a factor of 1).
#' @return "current_price"  Current asset price per share.
#' @return "lastday_price"  Last day’s asset price per share.
#' @return "change_today"  Percent change from last day price (by a factor of 1).
#' @examples 
#' close_position(ticker = "AAPL", live = FALSE)
#' close_position(ticker = "AAPL")
#' close_position(ticker = "AAPL", live = TRUE)
#' @import magrittr
#' @export
close_position <- function(ticker = NULL, live = FALSE){
  #Set URL, live = FALSE & Headers
  url = get_url(live)
  headers = get_headers(live)
  
  #Send Request
  positions = httr::DELETE(url = paste0(url,"/v2/positions/",ticker), headers) 
  positions = response_text_clean(positions)
  
  
  return(positions)
  
}
#----------------------------------------------------------------------------------------------
#UPDATED for V2
#close_position(ticker = "AAPL", live = TRUE)










#----------------------------------------------------------------------------------------------
#' Close all positions function
#'
#' The close all positions API liquidates all currently open long and short positions. Character values are returned as a string while integer values are returned as numeric.
#' @param live TRUE / FALSE if you are connecting to a live account. Default to FALSE, so it will use the paper url if nothing was provided.
#' @return "asset_id"  Asset ID.
#' @return "symbol"  Symbol of the asset.
#' @return "exchange"  Exchange name of the asset.
#' @return "asset_class"  Asset class name.
#' @return "avg_entry_price"  Average entry price of the position.
#' @return "qty" The number of shares.
#' @return "side" long/short exposure.
#' @return "market_value"  Total dollar amount of the position.
#' @return "cost_basis"  Total cost basis in dollar.
#' @return "unrealized_pl"  Unrealized profit/loss in dollar.
#' @return "unrealized_plpc"  Unrealized profit/loss percent (by a factor of 1).
#' @return "unrealized_intraday_pl"  Unrealized profit/loss in dollar for the day.
#' @return "unrealized_intraday_plpc"  Unrealized profit/loss percent (by a factor of 1).
#' @return "current_price"  Current asset price per share.
#' @return "lastday_price"  Last day’s asset price per share.
#' @return "change_today"  Percent change from last day price (by a factor of 1).
#' @examples 
#' close_all_positions(live = FALSE)
#' close_all_positions()
#' close_all_positions(live = TRUE)
#' @import magrittr
#' @export
close_all_positions <- function(live = FALSE){
  #Set URL, live = FALSE & Headers
  url = get_url(live)
  headers = get_headers(live)
  
  #Send Request
  positions = httr::DELETE(url = paste0(url,"/v2/positions"), headers) 
  positions = response_text_clean(positions)
  
  
  return(positions)
}
#----------------------------------------------------------------------------------------------
#NEW for V2
# Add "no positions to close" like "get positions", if no positions exist.
#close_all_positions(live = TRUE)








#----------------------------------------------------------------------------------------------
#' Get Orders function
#' 
#' The orders API allows a user to monitor, place and cancel their orders with Alpaca. Times are returned as yyyy-mm-dd hh-mm-ss POSIXct, quantity and price as numeric, and all others as a string.
#' @param ticker \code{(character)} Specify which symbol you want to call by inserting ticker as a string.
#' @param status \code{(character)} Order status to be queried. \code{open}, \code{closed} or \code{all}. Defaults to open as a string.
#' @param limit \code{(integer)} The maximum number of orders in response. Defaults to 50 and max is 500.
#' @param after \code{(Date/character)} Date in YYYY-MM-DD \href{https://www.iso.org/iso-8601-date-and-time-format.html}{(ISO8601 Format)} The response will include only orders submitted \emph{after} this date exclusive as a timestamp object.
#' @param until \code{(Date/character)} Date in YYYY-MM-DD \href{https://www.iso.org/iso-8601-date-and-time-format.html}{(ISO8601 Format)} The response will include only orders submitted \emph{before} this date exclusive as a timestamp object.
#' @param direction \code{(character)} The chronological order of response based on the submission time. \code{asc} or \code{desc}. Defaults to \code{desc}.
#' @param silent \code{(logical)} TRUE / FALSE on if you want the "no orders to cancel" message to print to the console. Default to FALSE.
#' @param live \code{(logical)} TRUE / FALSE if you are connecting to a live account. Default to FALSE, so it will use the paper url if nothing was provided.
#' @return "id" order id.
#' @return "client_order_id" client unique order id.
#' @return "created_at" nullable When the order was created.
#' @return "updated_at" nullable When the order was updated.
#' @return "submitted_at" nullable When the order was submitted.
#' @return "filled_at" nullable When the order was filled.
#' @return "expired_at" nullable When the order was expired.
#' @return "canceled_at" nullable When the order was canceled.
#' @return "asset_id" asset ID.
#' @return "symbol" Asset symbol.
#' @return "exchange" Asset exchange.
#' @return "asset_class" Asset class.
#' @return "qty" Ordered quantity.
#' @return "filled_qty" Filled quantity.
#' @return "type" Valid values: market, limit, stop, stop_limit.
#' @return "side" Valid values: buy, sell.
#' @return "time_in_force" time in force.
#' @return "limit_price" Limit price.
#' @return "stop_price" Stop price.
#' @return "status" Status of the order.
#' @examples 
#' get_orders(live = FALSE, version = "v2")
#' get_orders(status = "all", version = "v2")
#' # For a specific ticker:
#' get_orders(ticker = "AAPL", status = "all", version = "v2")
#' @import dplyr stringr
#' @importFrom lubridate ymd_hms
#' @export
get_orders <- function(ticker = NULL, status = "open", limit = NULL, after = NULL, until = NULL, direction = "desc", silent = FALSE, live = FALSE, version = "v2"){
  #Set URL & Headers
  url = httr::parse_url(get_url(live))
  headers = get_headers(live)
  
  # yogat3ch: Create Query 2020-01-11 2157
  url$path <- list(version, "orders")
  url$query <- list(status = status,
                    limit = limit,
                    after = after,
                    until = until,
                    direction = direction)
  # Build the url
  url <- httr::build_url(url)
  # Query
  orders <- httr::GET(url, headers)
  # Clean
  orders <-  response_text_clean(orders)
  if (!is.null(ticker)){       #If the ticker is not null, then return the orders for the tickers specified.
    if(length(orders) != 0) 
      orders = dplyr::filter(orders, symbol %in% ticker)
  } 
  
  if(length(orders) == 0){
    #Make sure there are orders to return before calling return. 
    if(!silent) message(paste("No",status,"orders for the selected query/filter criteria at this time.",'Try removing the ticker filter or setting status = "all" to see all orders.'))
    return(NULL)
  }  
  # Format orders to workable and readable format before returning
  toNum <- function(x){
    as.numeric(stringr::str_replace_all(x, "\\$|\\,", ""))
  }
  orders <- dplyr::mutate_at(orders, dplyr::vars(dplyr::ends_with("at")),list(~lubridate::ymd_hms(., tz = Sys.timezone())))
  orders <- dplyr::mutate_at(orders, dplyr::vars(qty, filled_qty, filled_avg_price, limit_price, stop_price), list(toNum))
  
  return(orders)
}
#----------------------------------------------------------------------------------------------
#UPDATED for V2
#get_orders(status = "all",live = TRUE, version = "v2")








#----------------------------------------------------------------------------------------------
#' Submit Order function
#' 
#' Places a new order of the specified stock, quantity, buy / sell, type of order, time in force, and limit / stop prices if selected.
#' @param ticker \code{(character)} The stock's symbol.
#' @param qty \code{(integer)} The amount of shares to trade.
#' @param side \code{(character)} The side of the trade. I.E "buy" or "sell"
#' @param type \code{(character)} The type of trade order. I.E "market","limit","stop","stop_limit", etc.
#' @param time_in_force \code{(character)} The type of time order. I.E "day", "gtc", "opg". In the V2 API, Immediate Or Cancel (IOC) & Fill or Kill (FOK) is added. Default is "day".
#' @param limit_price \code{(numeric)} If order type was a limit, then enter the limit price here.
#' @param stop_price \code{(numeric)} If order type was a stop, then enter the stop price here.
#' @param extended_hours \code{(logical)} Default: \code{FALSE}. If \code{TRUE}, order will be eligible to execute in premarket/afterhours. Only works with type limit and time_in_force day on the V2 API.
#' @param live \code{(logical)} TRUE / FALSE if you are connecting to a live account. Default to FALSE, so it will use the paper url if nothing was provided.
#' @examples 
#' # For market order:
#' submit_order(ticker = "AAPL", qty = 100, side = "buy", type = "market", version = "v2")
#' # Or you can submit a limit order:
#' submit_order(ticker = "AAPL", qty = 100, side = "buy", type = "limit", limit_price = 120, version = "v2")
#' @export
submit_order <- function(ticker, qty, side, type, time_in_force = "day", limit_price = NULL, stop_price = NULL, extended_hours = FALSE, live = FALSE, version = "v2"){
  #Set URL & Headers
  url = get_url(live)
  headers = get_headers(live)
  
  
  #Convert ticker argument to upper if lower
  if(grepl("^[[:lower:]]+$",ticker)){ticker <- toupper(ticker)}
  
  #Create body with order details, most common is a named list 
  bodyl <- list(symbol=ticker, qty=qty, side = side, type = type, time_in_force = time_in_force, limit_price = limit_price, stop_price = stop_price)
  bodyl <- lapply(bodyl, as.character)
  bodyl$extended_hours <- extended_hours
  
  #Send Request
  orders = httr::POST(url = paste0(url,"/",version,"/orders"), body = bodyl, encode = "json",headers)
  orders = response_text_clean(orders)
  return(orders)
}
#----------------------------------------------------------------------------------------------
#UPDATED for V2
#submit_order(ticker = "DBX", qty = 1, side = "buy", type = "limit", limit_price = 120, version = "v2")









#----------------------------------------------------------------------------------------------
#' Cancel Order function
#' 
#' Cancels any open order by either ticker or order id. If multiple open orders exist for one ticker, then the default is to cancel the most recent order. As of the V2 API update, an "all" arguement is added to cancel all open orders.
#' @param ticker_id The ticker symbol or the order id. If all = TRUE, no ticker_id is needed because ALL orders will be canceled.
#' @param all Default to False. If true, all open orders are cancelled. Only available in the V2 API.
#' @param live TRUE / FALSE if you are connecting to a live account. Default to FALSE, so it will use the paper url if nothing was provided.
#' @param version Use the deprecated V1 API or the newer V2 API.
#' @examples 
#' \dontrun{
#' # \emph{Note} At least one order for AAPL must be placed prior to running this example for the example to work.
#' cancel_order(ticker_id = "AAPL", version = "v2")
#' cancel_order(ticker_id = "aapl", version = "v2")
#' # Or you can instead cancel by the order_id:
#' orders <- get_orders(status="open", silent = TRUE, version = "v2")
#' cancel_order(ticker_id = orders$id[1])
#' }
#' @importFrom lubridate with_tz
#' @export
cancel_order <- function(ticker_id = NULL, all=FALSE, live = FALSE, version = "v2"){
  #Set URL & Headers
  url = get_url(live)
  headers = get_headers(live)
  
  
  #Gather the open order ID for the symbol specified
  open_orders = get_orders(status = "open", live = live, silent = TRUE)
  
  
  #Check if any open orders before proceeding. 
  if(is.null(open_orders)){
    stop("There are no orders to cancel at this time.")
    
  } else if (all == TRUE) { #If order id supplied then do this
    cat("Cancelling ALL open orders \n")
    
    
  } else if (nchar(ticker_id) > 15) { #If order id supplied then do this
    order_id <- ticker_id
    ticker <- open_orders$symbol[open_orders$id == order_id]
    # If more than one order is open print message to notify which order is being cancelled
    if(length(open_orders$symbol %in% ticker) >1) message(paste0("More than one order open for ",ticker,", the order placed at ", lubridate::with_tz(as.POSIXlt(open_orders$submitted_at[open_orders$id %in% order_id], tz = "UTC", tryFormats = c("%Y-%m-%dT%H:%M:%OS")), Sys.timezone())," will be canceled"))
    
  } else { #If ticker supplied then do this
    ticker <- ticker_id 
    open_orders_sym <- grep(ticker, open_orders$symbol, ignore.case = T)
    #If more than one order is open print message to notify which order is being cancelled
    if(length(open_orders_sym) > 1) message(paste0("More than one order open for ",ticker,", the order placed at ", lubridate::with_tz(as.POSIXlt(open_orders$submitted_at[open_orders_sym[1]], tz = "UTC", tryFormats = c("%Y-%m-%dT%H:%M:%OS")), Sys.timezone())," will be canceled"))
    order_id <- open_orders[[open_orders_sym[1], "id"]]
  }
  #Send Request & Cancel the order through the order_id
  if(all == TRUE){
    cancel = httr::DELETE(url = paste0(url,"/",version,"/orders"), headers)
    cat(paste("ALL open orders were successfully canceled."))
  } else{
    cancel = httr::DELETE(url = paste0(url,"/",version,"/orders/",order_id), headers)
    cat(paste("Order ID", order_id,"for", ticker, "was successfully canceled."))
  }
  
}
#----------------------------------------------------------------------------------------------
#UPDATED for V2
#cancel_order(ticker_id = "aapl", version = "v2")










#----------------------------------------------------------------------------------------------
#' Replace Order function
#' 
#' Cancels any open order by either ticker or order id. If multiple open orders exist for one ticker, then the default is to cancel the most recent order.
#' @param ticker_id The ticker symbol or the order id.
#' @param qty The amount of shares to replace.
#' @param time_in_force The type of time order. I.E "day", "gtc", "opg". Default is "day".
#' @param limit_price If order type was a limit, then enter the limit price here.
#' @param stop_price If order tyope was a stop, then enter the stop price here.
#' @param live TRUE / FALSE if you are connecting to a live account. Default to FALSE, so it will use the paper url if nothing was provided.
#' @examples
#' \dontrun{ 
#' replace_order(ticker_id = "AAPL", qty = <new qty amount>, time_in_force = <new time in force>, limit_price=<new limit price, if applicable>, stop_price=<new stop price, if applicable>, live = FALSE)
#' # Or you can instead cancel by the order_id:
#' orders <- get_orders(status="open", silent = TRUE)
#' # replace_order(ticker_id = orders$id[1], qty = <new qty amount>, time_in_force = <new time in force>, limit_price=<new limit price, if applicable>, stop_price=<new stop price, if applicable>, live = FALSE)
#' }
#' @importFrom lubridate with_tz
#' @export
replace_order <- function(ticker_id, qty = NULL, time_in_force = "day", limit_price=NULL, stop_price=NULL, live = FALSE){
  #Set URL & Headers
  url = get_url(live)
  headers = get_headers(live)
  
  
  #Gather the open order ID for the symbol specified
  open_orders = get_orders(status = "open", live = live, silent = TRUE)
  
  if(is.null(qty)) stop("You must provide share qty to replace your order")
  
  if (nchar(ticker_id) > 15) { #If order id supplied then do this
    order_id <- ticker_id
    ticker <- open_orders$symbol[open_orders$id == order_id]
    
    # If more than one order is open print message to notify which order is being cancelled
    #if(length(open_orders$symbol %in% ticker) >1) message(paste0("More than one order open for ",ticker,", the order placed at ", lubridate::with_tz(as.POSIXlt(open_orders$submitted_at[open_orders$id %in% order_id], tz = "UTC", tryFormats = c("%Y-%m-%dT%H:%M:%OS")), Sys.timezone())," will be canceled"))
    
  } else { #If ticker supplied then do this
    ticker <- ticker_id 
    open_orders_sym <- grep(ticker, open_orders$symbol, ignore.case = T)
    #If more than one order is open print message to notify which order is being cancelled
    #if(length(open_orders_sym) > 1) message(paste0("More than one order open for ",ticker,", the order placed at ", lubridate::with_tz(as.POSIXlt(open_orders$submitted_at[open_orders_sym[1]], tz = "UTC", tryFormats = c("%Y-%m-%dT%H:%M:%OS")), Sys.timezone())," will be canceled"))
    order_id <- open_orders[[open_orders_sym[1], "id"]]
  }
  
  
  
  #Send Request & Cancel the order through the order_id
  
  #Create body with order details, most common is a named list 
  bodyl <- list(qty = qty, time_in_force = time_in_force, limit_price = limit_price, stop_price = stop_price)
  replace = httr::PATCH(url = paste0(url,"/v2/orders/",order_id),body = bodyl, encode = "json", headers)
  replace = response_text_clean(replace)
  
  
  
  if(TRUE %in% grepl(pattern = "^4",x = replace)){
    cat(paste("Order ID", order_id,"for",ticker, "was not replaced. Please check syntax and order_id."),"\n")
  } else{
    cat(paste("Order ID", order_id,"for",ticker, "was successfully replaced."),"\n")
  }
  return(replace)
}
#----------------------------------------------------------------------------------------------
#NEW for V2
#eplace_order(ticker_id="AAPL", qty = <new qty amount>, time_in_force = <new time in force>, limit_price=<new limit price, if applicable>, stop_price=<new stop price, if applicable>, live = FALSE)









#----------------------------------------------------------------------------------------------
#' Get Assets function
#' 
#' The assets API serves as the master list of assets available for trade and data consumption from Alpaca. Assets are sorted by asset class, exchange and symbol. Some assets are only available for data consumption via Polygon, and are not tradable with Alpaca. These assets will be marked with the flag tradable=false.
#' @param ticker Check for a specific stock and if its active on Alpaca by inputting ticker as a string.
#' @return "id Asset" ID as a string.
#' @return "asset_class" us_equity as a string.
#' @return "exchange" AMEX, ARCA, BATS, NYSE, NASDAQ or NYSEARCA as a string.
#' @return "symbol" Stock symbol as a string.
#' @return "status" active or inactive as a string.
#' @return "tradeable" Asset is tradable on Alpaca or not as a boolean.
#' @examples 
#' get_assets()
#' # Get a specific asset:
#' get_assets(ticker = "AAPL",version = "v2")
#' @export
get_assets <- function(ticker = NULL, version = "v2"){
  #Set URL & Headers
  url = get_url()
  headers = get_headers()
  
  
  
  #Send Request and ticker if one was supplied. 
  if(is.null(ticker)){
    assets = httr::GET(url = paste0(url,"/",version,"/assets"), headers)
    assets = response_text_clean(assets)
  } else{
    assets = httr::GET(url = paste0(url,"/",version,"/assets/",ticker), headers)
    assets = response_text_clean(assets)
  } 
  return(assets)
}
#----------------------------------------------------------------------------------------------
#UPDATED for V2
#get_assets(ticker = "AAPL",version = "v2")









#----------------------------------------------------------------------------------------------
#' Get Account function
#'
#' The accounts API serves important information related to an account, including account status, funds available for trade, funds available for withdrawl, and various flags relevant to an account’s ability to trade.
#' @param activity_type The activity type you want to view entries for. A list of valid activity types can be found in the examples below. <string>
#' @param date The date for which you want to see activities. <string timestamp>
#' @param until The response will contain only activities submitted before this date. (Cannot be used with date.) <string timestamp>
#' @param after The response will contain only activities submitted after this date. (Cannot be used with date.) <string timestamp>
#' @param direction asc or desc, default is desc. <string>
#' @param page_size The maximum number of entries to return in the response. <int>
#' @param page_token The ID of the end of your current page of results. <string>
#' @param live TRUE / FALSE if you are connecting to a live account. Default to FALSE, so it will use the paper url if nothing was provided.
#' @return "id" Account ID as a string.
#' @return "status" Account Status as a string.
#' @return "currency" USD as a string.
#' @return "buying_power" Tradable buying power as a string.
#' @return "cash" Cash balance as a string.
#' @return "cash_withdrawable"  Withdrawable cash amount as a string.
#' @return "portfolio_value"  Total value of cash + holding positions as a string.
#' @return "trading_blocked"  If true, the account is not allowed to place orders as a boolean.
#' @return "transfers_blocked"  If true, the account is not allowed to request money transfers as a boolean.
#' @return "account_blocked"  If true, the account activity by user is prohibited as a boolean.
#' @return "created_at"  Timestamap this account was created at as a string.
#' @details 
#' Activity Types:
#' \itemize{
#'   \item{FILL: Order fills (both partial and full fills)}
#'   \item{TRANS: Cash transactions (both CSD and CSR)}
#'   \item{MISC: Miscellaneous or rarely used activity types (All types except those in TRANS, DIV, or   FILL)}
#'   \item{ACATC: ACATS IN/OUT (Cash)}
#'   \item{ACATS: ACATS IN/OUT (Securities)}
#'   \item{CSD: Cash disbursement(+)}
#'   \item{CSR: Cash receipt(-)}
#'   \item{DIV: Dividends}
#'   \item{DIVCGL: Dividend (capital gain long term)}
#'   \item{DIVCGS: Dividend (capital gain short term)}
#'   \item{DIVFEE: Dividend fee}
#'   \item{DIVFT: Dividend adjusted (Foreign Tax Withheld)}
#'   \item{DIVNRA: Dividend adjusted (NRA Withheld)}
#'   \item{DIVROC: Dividend return of capital}
#'   \item{DIVTW: Dividend adjusted (Tefra Withheld)}
#'   \item{DIVTXEX: Dividend (tax exempt)}
#'   \item{INT: Interest (credit/margin)}
#'   \item{INTNRA Interest adjusted (NRA Withheld)}
#'   \item{INTTW: Interest adjusted (Tefra Withheld)}
#'   \item{JNL: Journal entry}
#'   \item{JNLC: Journal entry (cash)}
#'   \item{JNLS: Journal entry (stock)}
#'   \item{MA: Merger/Acquisition}
#'   \item{NC: Name change}
#'   \item{OPASN: Option assignment}
#'   \item{OPEXP: Option expiration}
#'   \item{OPXRC: Option exercise}
#'   \item{PTC: Pass Thru Charge}
#'   \item{PTR: Pass Thru Rebate}
#'   \item{REORG: Reorg CA}
#'   \item{SC: Symbol change}
#'   \item{SSO: Stock spinoff}
#'   \item{SSP: Stock split}
#' }
#' @examples 
#' get_account_activities(activity_type = "FILL")
#' @export
get_account_activities <- function(activity_type = c(NULL), date = NULL, until = NULL, after = NULL, direction = "desc", page_size = 50, page_token = NULL, live = FALSE){
  #Set URL & Headers
  url = get_url(live)
  headers = get_headers(live)
  
  #Send Request
  if(is.null(activity_type)){
    account_activities = httr::GET(url = paste0(url,"/v2/account/activities"), headers)
    account_activities = response_text_clean(account_activities)
  } else{
    account_activities = httr::GET(url = paste0(url,"/v2/account/activities/", activity_type, "?date=", date, "&until=", until, "&after=",after,"&direction=",direction,"&page_size=",page_size,"&page_token=",page_token), headers)
    account_activities = response_text_clean(account_activities)
  }

  return(account_activities)
}
#----------------------------------------------------------------------------------------------
#NEW for V2
#get_account_activities(activity_type = "FILL")









#----------------------------------------------------------------------------------------------
#' Create Watchlist function
#'
#' Creates a new watchlist with initial set of assets.
#' @param name arbitrary name string, up to 64 characters. <string REQUIRED>
#' @param tickers Set of symbols. <vector of string>
#' @param live TRUE / FALSE if you are connecting to a live account. Default to FALSE, so it will use the paper url if nothing was provided.
#' @return An array of a watchlist object.
#' @examples 
#' create_watchlist(name = "test", tickers = c("AAPL","WMT"), live = FALSE)
#' @export
create_watchlist <- function(name = NULL, tickers = c(NULL), live = FALSE){
  
  # Set URL & Headers
  url = get_url(live)
  headers = get_headers(live)
  
  bodyl=list(name=name,symbols=tickers)
  watchlist = httr::POST(url = paste0(url, "/v2/watchlists"), body = bodyl, encode = "json", headers)
  watchlist = response_text_clean(watchlist)
  
  return(watchlist)
}
#NEW for V2
#create_watchlist(name = "test", tickers = c("AAPL","WMT"), live = FALSE)








#----------------------------------------------------------------------------------------------
#' Get Watchlist IDs function
#'
#' Returns a list of watchlists and the IDs registered under the account.
#' @param live TRUE / FALSE if you are connecting to a live account. Default to FALSE, so it will use the paper url if nothing was provided.
#' @return An array of watchlist objects.
#' @examples 
#' get_watchlist_ids(live = FALSE)
#' @export
get_watchlist_ids <- function(live = FALSE){
  
  # Set URL & Headers
  url = get_url(live)
  headers = get_headers(live)
  
  
  watchlist = httr::GET(url = paste0(url, "/v2/watchlists"), headers)
  watchlist = response_text_clean(watchlist)
  return(watchlist)
}
#NEW for V2
#get_watchlist_ids(live = FALSE)










#----------------------------------------------------------------------------------------------
#' Get Watchlist function
#'
#' Returns a specific watchlist identified by the watchlist ID provided.
#' @param watchlist_id Watchlist ID <string>
#' @param live TRUE / FALSE if you are connecting to a live account. Default to FALSE, so it will use the paper url if nothing was provided.
#' @return A watchlist object.
#' @examples 
#' get_watchlist(watchlist_id = "ea3cc05b-2844-4985-b0c9-8f412718ac9e", live = FALSE)
#' @export
get_watchlist <- function(watchlist_id = NULL, live = FALSE){
  
  # Set URL & Headers
  url = get_url(live)
  headers = get_headers(live)
  
  
  watchlist = httr::GET(url = paste0(url, "/v2/watchlists/",watchlist_id), headers)
  watchlist = response_text_clean(watchlist)
  return(watchlist)
  
}
#NEW for V2
#get_watchlist(watchlist_id = "ea3cc05b-2844-4985-b0c9-8f412718ac9e", live = FALSE)









#----------------------------------------------------------------------------------------------
#' Update Watchlist function
#'
#' Update the name and/or content of watchlist. For symbols, You must provide current tickers, and new ticker to add.
#' @param watchlist_id Watchlist ID <string>
#' @param name arbitrary name string, up to 64 characters. <string>
#' @param tickers Set of symbols. <vector of strings>
#' @param live TRUE / FALSE if you are connecting to a live account. Default to FALSE, so it will use the paper url if nothing was provided.
#' @return A watchlist object with updated content.
#' @examples 
#' update_watchlist(watchlist_id = "ea3cc05b-2844-4985-b0c9-8f412718ac9e", name = "test_watch", tickers = c("AAPL","WMT"), live=FALSE)
#' @export
update_watchlist <- function(watchlist_id = NULL, name = NULL, tickers = c(NULL), live = FALSE){
  
  # Set URL & Headers
  url = get_url(live)
  headers = get_headers(live)
  
  bodyl= list(name=name,symbols=tickers)
  watchlist = httr::PUT(url = paste0(url, "/v2/watchlists/",watchlist_id), body = bodyl, encode = "json", headers)
  watchlist = response_text_clean(watchlist)
  return(watchlist)
  
}
#NEW for V2
#update_watchlist(watchlist_id = "ea3cc05b-2844-4985-b0c9-8f412718ac9e", name = "test_watch", tickers = c("AAPL","WMT"), live=TRUE)









#----------------------------------------------------------------------------------------------
#' Add to Watchlist function
#'
#' Append an asset for the symbol to the end of watchlist asset list. You must provide current tickers, and new ticker to add.
#' @param watchlist_id Watchlist ID <string>
#' @param tickers Set of symbols. <vector of strings REQUIRED>
#' @param live TRUE / FALSE if you are connecting to a live account. Default to FALSE, so it will use the paper url if nothing was provided.
#' @return A watchlist object with updated content.
#' @examples 
#' add_to_watchlist(watchlist_id = "ea3cc05b-2844-4985-b0c9-8f412718ac9e", tickers = c("AAPL","WMT"), live = FALSE)
#' @export
add_to_watchlist <- function(watchlist_id = NULL, tickers = NULL, live = FALSE){
  
  # Set URL & Headers
  url = get_url(live)
  headers = get_headers(live)
  
  
  bodyl = list(symbols=tickers)
  watchlist = httr::PUT(url = paste0(url, "/v2/watchlists/",watchlist_id), body = bodyl, encode = "json", headers)
  watchlist = response_text_clean(watchlist)
  return(watchlist)
  
}
#NEW for V2
#add_to_watchlist(watchlist_id = "ea3cc05b-2844-4985-b0c9-8f412718ac9e", tickers = c("AAPL","WMT","AMD"), live = FALSE)









#----------------------------------------------------------------------------------------------
#' Delete Watchlist function
#'
#' Delete a watchlist. This is a permantent deletion.
#' @param watchlist_id Watchlist ID <string>
#' @param live TRUE / FALSE if you are connecting to a live account. Default to FALSE, so it will use the paper url if nothing was provided.
#' @examples 
#' delete_watchlist(watchlist_id = "ea3cc05b-2844-4985-b0c9-8f412718ac9e", live = FALSE)
#' @export
delete_watchlist <- function(watchlist_id = NULL, live = FALSE){
  
  # Set URL & Headers
  url = get_url(live)
  headers = get_headers(live)
  
  
  watchlist = httr::DELETE(url = paste0(url, "/v2/watchlists/",watchlist_id), headers)
  
  if(watchlist$status_code == 204){ cat(paste0("Success, Watchlist ID",watchlist_id, " deleted"))}
  
  else{
    cat("Watchlist ID",watchlist_id," not sucessfully deleted")
  }
  
}
#NEW for V2
#delete_watchlist(watchlist_id = "ea3cc05b-2844-4985-b0c9-8f412718ac9e", live = FALSE)









#----------------------------------------------------------------------------------------------
#' Delete from Watchlist function
#'
#' Delete one entry for an asset by symbol name.
#' @param watchlist_id Watchlist ID <string>
#' @param tickers Symbol name to remove from the watchlist content. <string>
#' @param live TRUE / FALSE if you are connecting to a live account. Default to FALSE, so it will use the paper url if nothing was provided.
#' @examples 
#' delete_from_watchlist(watchlist_id = "ea3cc05b-2844-4985-b0c9-8f412718ac9e", ticker = "AAPL", live = FALSE)
#' @export
delete_from_watchlist <- function(watchlist_id = NULL, ticker = NULL, live = FALSE){
  
  # Set URL & Headers
  url = get_url(live)
  headers = get_headers(live)
  
  
  watchlist = httr::DELETE(url = paste0(url, "/v2/watchlists/", watchlist_id, "/", ticker), headers)
  watchlist = response_text_clean(watchlist)
  return(watchlist)
  
}
#NEW for V2
#delete_from_watchlist(watchlist_id = "ea3cc05b-2844-4985-b0c9-8f412718ac9e", ticker = "AAPL", live = FALSE)









#----------------------------------------------------------------------------------------------
#' Get Calendar function
#' 
#' The calendar API serves the full list of market days from 1970 to 2029. It can also be queried by specifying a start and/or end time to narrow down the results. In addition to the dates, the response also contains the specific open and close times for the market days, taking into account early closures.
#' @param from  Starting date of request. Default to NULL so it starts from 1970.
#' @param to  Ending date of request. Default to NULL so it ends in 2029.
#' @return "date" Date string. in date format as a string.
#' @return "open" The time the market opens at on this date in hour:min format as a string.
#' @return "close" The time the market closes at on this date in hour:min format as a string.
#' @examples 
#' # Get all dates:
#' get_calendar(version = "v2")
#' # Get specific date range:
#' get_calendar(from = "2019-01-01", to = "2019-04-01", version = "v2")
#' @export
#' @import lubridate
#' @importFrom httr GET parse_url build_url
#' @importFrom purrr map
#' @importFrom magrittr "%>%"

get_calendar <- function(from = NULL, to = NULL, version = "v2"){
  #Set URL & Headers
  url = httr::parse_url(get_url())
  headers = get_headers()
  .bounds <- list(from = from, to = to)
  #If dates were given, make sure they are in a string format. I add plus 1 to the "to" date, or it will always return one day before the day "to"
    # 2020-03-29T18:25:01 This appears to be fixed, removing the +1
  .null <- purrr::map_lgl(.bounds, is.null)
  # Check for null values and warn if NULL
  if (any(.null)){
    message(paste0(paste0(names(.null)[.null], collapse = ", "), " arg(s) is/are NULL, coercing to ", lubridate::today()))
    .bounds <- purrr::map(.bounds, ~{
      if (is.null(.x)) lubridate::today() - lubridate::days(1) else lubridate::as_date(.x)
    })
  }
  # Check for weekend values and warn if weekend
  purrr::imap(.bounds, ~{
    if (lubridate::wday(.x) %in% c(1,7)) {
      message(paste0(.y, " is a ",lubridate::wday(.x, label = T, abbr = F),", API will return data for the previous Friday or following Monday"))
    }
  })
  url$path <- list(
    version = version,
    endpoint = "calendar"
  )
  url$query <- list(
    start = as.character(.bounds[[1]]),
    end = as.character(.bounds[[2]])
  )
  url <- httr::build_url(url)
  calendar = httr::GET(url = url, headers)
  calendar =  response_text_clean(calendar)
  calendar <- dplyr::mutate_at(calendar, dplyr::vars(date), lubridate::ymd, tz = Sys.timezone()) %>% 
    dplyr::mutate(
      day = lubridate::interval(
        start = lubridate::ymd_hm(paste(date, open), tz = Sys.timezone()),
        end = lubridate::ymd_hm(paste(date, close), tz = Sys.timezone())
      ),
      session = lubridate::interval(
        start = lubridate::ymd_hm(paste(date, session_open), tz = Sys.timezone()),
        end = lubridate::ymd_hm(paste(date, session_close), tz = Sys.timezone())
      ),
      dow = lubridate::wday(lubridate::as_date(date), label = T)
    ) %>%
    dplyr::select(date, dow, day, session, everything())
  return(calendar)
}
#----------------------------------------------------------------------------------------------
#UPDATED for V2
#get_calendar(from = "2019-01-01", to = "2019-04-01", version = "v2")





#----------------------------------------------------------------------------------------------
#' Get Clock function
#' 
#' The clock API serves the current market timestamp, whether or not the market is currently open, as well as the times of the next market open and close.
#' @return "timestamp" Current timestamp as a string.
#' @return "is_open" Whether or not the market is open as a boolean.
#' @return "next_open" Next market open timestamp as a string.
#' @return "next_close" Next market close timestamp as a string.
#' @examples 
#' get_clock(version = "v2")
#' @export
get_clock <- function(version = "v2"){
  #Set URL & Headers
  url = get_url()
  headers = get_headers()
  
  #Send Request
  clock = httr::GET(url = paste0(url,"/",version,"/clock"), headers)
  clock = response_text_clean(clock)
  return(clock)
}
#----------------------------------------------------------------------------------------------
#UPDATED for V2
#get_clock(version = "v2")






#----------------------------------------------------------------------------------------------
#' Get Bars function
#' 
#' The bars API provides time-aggregated price and volume data for a single stock or multiple. **The `'v2'` API is only available for live accounts and accepts the `from`, `to`, `timeframe`, `multiplier`, and `unadjusted` arguments.**
#' @param ticker \code{(character)} The stock or stocks (in vector format) that you want.
#' @param v \code{(integer)} The API version number. If `1`, the `'v1'` \href{https://alpaca.markets/docs/api-documentation/api-v2/market-data/#endpoint}{IEX/Alpaca API}: data.alpaca.markets/v1 will be used, if `2`, the `'v2'` \href{https://alpaca.markets/docs/api-documentation/api-v2/market-data/#polygon-integration}{Polygon/Alpaca API}: api.polygon.io/v2/aggs \href{https://polygon.io/docs/#get_v2_aggs_ticker__ticker__range__multiplier___timespan___from___to__anchor}{Aggregates Endpoint}  will be used. 
#' @param timeframe \code{(character)} For the `'v1'` API, one of
#' \itemize{
#'  \item{`'m'`/`'min'`/`'minute'`} (`multiplier` can be `1`/`5`/`15`)
#'  \\item{`'d'`/`'day'`} (`multiplier` will be `1`)
#' } 
#' Not case-sensitive.
#' For the `'v2'` API, `multiplier` can be any `integer` for any one of the following `timeframe`'s:
#' \itemize{
#'  \item{`'m'`/`'min'`/`'minute'`}
#'  \item{`'h'`/`'hour'`}
#'  \item{`'d'`/`'day'`}
#'  \item{`'w'`/`'week'`}
#'  \item{`'M'`/`'mo'`/`'month'`} (*Note* capitalized M for month)
#'  \item{`'q'`/`'quarter'`}
#'  \item{`'y'`/`'year'`} 
#' } 
#' Case-sensitive
#' @param multiplier For the `'v1'` API, with `'minute'` `timeframe` one of `1`/`5`/`15`. Otherwise, defaults to `1`.
#' For the `'v2'` API, this can be any `integer`, also defaults to `1`.
#' @param from (equivalent to `start` in `'v1'`) \code{(Date/POSIXlt/Datetime(POSIXct)/character)} See Details for formatting guidelines. Return data *equal to or after* this time. Default is 7 days ago. 
#' @param to (equivalent to `end` in `'v1'`) \code{(Date/POSIXlt/Datetime(POSIXct)/character)} See Details for formatting guidelines. Return data *equal to or before* this time. Default is today's date.
#' @param after *`'v1'` only* \code{(Date/POSIXlt/Datetime(POSIXct)/character)} See Details for formatting guidelines. Return data *after* this time. Default is 7 days ago. *Cannot be used with \code{from}*
#' @param until *`'v1'` only* \code{(Date/POSIXlt/Datetime(POSIXct)/character)} See Details for formatting guidelines. Return data *before* this time. Default is today's date. *Cannot be used with \code{from}*
#' @param limit *v1 only* \code{(integer)} The amount of bars to return per ticker. This can range from 1 to 1000. Defaults to 1000. *Note:* If \code{full} is set to T, this parameter is ignored and forced to 1000. 
#' @param full \code{(logical)} If TRUE, the function will attempt to return the entire expected dataset based on the range of dates provided and perform a data completeness check. If the requested from, to dates/times exceed that which can be returned in a single call, the API will be called repeatedly to return the **full** dataset. If FALSE, the request will be submitted to the API as is. *Note:* The `'v1'` API has a call limit of 1000 bars and a rate limit of 200 requests per minute. If the rate limit is reached, queries will pause for 1 minute. Defaults to FALSE.
#' @param unadjusted *'v2' only* \code{(logical)} Set to `TRUE` if the results should **NOT** be adjusted for splits. Defaults to `FALSE`.
#' @details All \code{(Date/POSIXlt)} must be in `YYYY-MM-DD` \href{https://www.iso.org/iso-8601-date-and-time-format.html}{RFC 3339} format. If `(Datetime/POSIXct)`, `YYYY-MM-DD HH:MM` \href{https://www.iso.org/iso-8601-date-and-time-format.html}{ISO8601} format. The `'v2'` API only accepts Dates in YYYY-MM-DD format, any arguments passed to `start` or `end` will be coerced to Date automatically if using `'v2'`. For the `'v2'` API, queries with `timeframe`: `'year'` use `12/31` as an aggregate date for each year. Arguments passed to `from` & `to` will be coerced to their yearly \link[lubridate]{`floor_date`} and \link[lubridate]{ceiling_date} respectively.
#' @return \code{list} object for each ticker symbol containing a \code{data.frame} with the following columns:
#' \itemize{
#'  \item{\code{time}}{  the time of the bar as \code{POSIXct} in yyyy-mm-dd for timeframe = day, and yyyy-mm-dd hh:mm:ss for timeframes < day}
#'  \item{\code{open}}{  open price as a numeric object.}
#'  \item{\code{high}}{  high price as a numeric object.}
#'  \item{\code{low}}{  low price as a numeric object.}
#'  \item{\code{close}}{  close price as a numeric object.}
#'  \item{\code{volume}}{  volume (in millions) as a numeric object.}
#' }
#' @examples
#' # Getting one or more tickers: 
#' get_bars(ticker = c("INTC","MSFT"))
#' # Getting price data with specific date ranges and timeframes, by also limiting the amount of bars returned for each ticker.
#' get_bars(ticker = c("INTC","MSFT"), from = "2019-03-20", to = "2019-04-01", timeframe = "15Min", limit = 175)
#' @import dplyr stringr magrittr lubridate
#' @export

# Generate the list in the docs:
# list(m = c("m","min","minute"), h = c("h", "hour"),d = c("d", "day"), w = c("w", "week"), m = c("M", "mo", "month"), q = c("q", "quarter"), y = c("y", "year")) %>% purrr::map(~{paste0("\\item{",paste0(paste0("`'",.x,"'`"), collapse = '/'),"}")}) %>% do.call(c,.) %>% cat(collapse = "\n")
# For DEBUG
list2env(
  list(
    ticker = c("AMZN"),
    v = 2,
    multiplier = 5,
    timeframe = "m",
    from = lubridate::ymd("2020-02-28"),
    to = lubridate::now() - lubridate::days(1),
    after = NULL,
    until = NULL,
    limit = NULL,
    full = F,
    unadjusted = F
  ),
  envir = .GlobalEnv
)


get_bars <- function(ticker, v = 1, timeframe = "day", multiplier = 1, from = Sys.Date()-7, to = Sys.Date(), after = NULL, until = NULL, limit = NULL, full = F, unadjusted =  F){
  
  #param check:  Thu Mar 26 08:34:13 2020 ----
  if(is.null(ticker)){
    stop("Please enter a stock ticker.")
  }
  if((is.null(from) || is.null(to)) && (is.null(start) || is.null(end))){
    stop("Please enter either 'from' & 'to' or 'after' & 'until' arguments.")
  }
  if (!any(v %in% 1:2)) stop("Version 'v' must be either 1 or 2")
  
  #quick detection of timespan abbreviations:  Thu Mar 26 08:34:00 2020 ----
  .tf_opts <- list(m = c("m","min","minute"), h = c("h", "hour"), d = c("d", "day"), w = c("w", "week"), M = c("M", "mo", "month"), q = c("q", "quarter"), y = c("y", "year"))
  # Create ordered factor or timeframe options
  .tf_order <- purrr::map_chr(.tf_opts, tail, 1) %>% {factor(., levels = .)}
  
  if (v == 1) {
    # Get the timeframe
    timeframe <- tail(.tf_opts[c(1,3)][[grep(stringr::regex(timeframe, ignore_case = T), .tf_opts[c(1,3)], ignore.case = T)]], 1)
    
    # Check args
    if (timeframe == "minute" && !any(multiplier %in% c(1,5,15))) {
      stop("The v1 API only accepts multipliers of 1,5,15 for the minute timeframe")
    } else if (timeframe == "day" && multiplier != 1) {
      message("The v1 API only accepts 1 as a multiplier for day timeframe")
    }
    
  } else if (v == 2){
    timeframe <- tail(.tf_opts[[grep(timeframe, .tf_opts, ignore.case = F)[1]]], 1)
  }
  # Get the timeframe as a numeric
  .tf_num <- which(.tf_order %in% timeframe)
  # Handle date bounds:  Thu Mar 26 08:40:24 2020 ----
  .bounds <- bars_bounds(from = from, to = to, after = after, until = until)
  
  message(paste0("Floor/Ceiling dates are necessary to retrieve inclusive aggregates\n'from' coerced to ", .bounds[[1]], "\n'to' coerced to ", .bounds[[2]]))
  # Stop if malformed date argument with informative message
  if (any(purrr::map_lgl(.bounds, is.na))) stop(paste0("Error: Check the following argument(s) format: `", names(purrr::keep(.bounds, ~{is.null(.x)||is.na(.x)})), "`"))
  
  if (full) {
    if (v == 1) {
      url <- bars_url(.bounds = .bounds)
      
    } else if (v == 2) {
      # Form the URL
      url <- bars_url(.bounds = .bounds)
      # retrieve the data
      bars <- bars_get(url)
      .expected <- bars_expected(bars)
      .missing <- bars_missing(bars)
      bars <- bars_complete(bars, .missing = .missing)
    }
  } else {
    # Submit request as is for full = F:  Tue Mar 17 21:35:54 2020 ----
    # Coerce to ISO8601 for call
    if (v == 1) {
      # build the URL
      url <- bars_url(ticker)
      # retrieve the data
      bars <- bars_get(url)
    } else if (v == 2) {
      # build the URL
      url <- bars_url(ticker)
      # retrieve the data
      bars <- bars_get(url)
    }
  # End case where full = F ---- Fri Mar 27 11:19:05 2020
     
  }
  #Rename columns to quantmod standard and reformat time column
  bars <- bars_transform(bars)
  return(bars)
}
#----------------------------------------------------------------------------------------------
#get_bars(ticker = "DBX")









#Select Polygon Integration functions - Live Brokerage accounts are the only accounts with access to Polygon.

#----------------------------------------------------------------------------------------------


#Best function to use if you want to see a little more than just pricing data for your company. 
#My favorite are the analyst estimates and news endpoints. I am not 100% sure how quickly the news links are updated, but I am very interested and will try to reach out regarding this. 
#I decided to integrate all endpoints into one function and the user can either call a specific endpoint, or call none.


#----------------------------------------------------------------------------------------------
#' Get Polygon Meta Data
#' 
#' This function provides more color on your stock through its available meta data endpoints from Polygon. These endpoints are company, analysts, dividends, earnings, and news.
#' @param ticker Specify which symbol you want to call by inserting ticker as a string.
#' @param endpoint Select either company for a company profile, analysts for all kinds of analyst estimates, dividends to view historic and upcoming dividends, earnings for historic and current earning details , or news for news updates from CNBC, Seeking Alpha, etc.
#' @param perpage This is only used if "news" was your selected endpoint. How many articles do you want to see perpage?
#' @param version The current version for API. Defaults to v1 if no v2 available. 
#' @return A list object containing all information the API responds with. 
#' @examples
#' # Getting default meta for AMZN: 
#' get_meta(ticker = "AMZN")
#' # Getting news information on AMZN: 
#' get_meta(ticker = "AMZN", endpoint = "news", perpage = 100)
#' @export
get_meta <- function(ticker=NULL, endpoint=NULL, perpage=NULL,version="v1"){
  #Set URL 
  path_url = get_url_poly()
  
  
  if(is.null(ticker)){
    stop("Please enter a ticker for the stock that you want.")
  }
  
  #If no endpoint entered, then keep default behavior. If a endpoint was provided, then request a call to that endpoint.
  if(is.null(endpoint)){
    full_path_url = paste0(path_url,"/",version,"/meta/symbols/",ticker,"?apiKey=",Sys.getenv("APCA-LIVE-API-KEY-ID"))
    
  } else if(endpoint == "company"){
    full_path_url = paste0(path_url,"/",version,"/meta/symbols/",ticker,"/company","?apiKey=",Sys.getenv("APCA-LIVE-API-KEY-ID"))
    
  } else if(endpoint == "analysts"){
    full_path_url = paste0(path_url,"/",version,"/meta/symbols/",ticker,"/analysts","?apiKey=",Sys.getenv("APCA-LIVE-API-KEY-ID"))
    
  } else if(endpoint == "dividends"){
    full_path_url = paste0(path_url,"/",version,"/meta/symbols/",ticker,"/dividends","?apiKey=",Sys.getenv("APCA-LIVE-API-KEY-ID"))
    
  } else if(endpoint == "earnings"){
    full_path_url = paste0(path_url,"/",version,"/meta/symbols/",ticker,"/earnings","?apiKey=",Sys.getenv("APCA-LIVE-API-KEY-ID"))
    
  } else if (endpoint == "news"){
    ifelse(is.null(perpage),
           full_path_url <- paste0(path_url,"/",version,"/meta/symbols/",ticker,"/news","?perpage=50","&apiKey=",Sys.getenv("APCA-LIVE-API-KEY-ID")),
           full_path_url <- paste0(path_url,"/",version,"/meta/symbols/",ticker,"/news","?perpage=",perpage,"&apiKey=",Sys.getenv("APCA-LIVE-API-KEY-ID")))
  }
  
  #Send Request
  meta = last_price_details = httr::GET(url = full_path_url)
  meta = response_text_clean(meta)
  return(meta)
}
#----------------------------------------------------------------------------------------------

#get_meta(ticker = "AAPL",endpoint = "news")









#----------------------------------------------------------------------------------------------
#' Get Polygon Aggregate Pricing Data
#' 
#' This function provides aggregate pricing data from Polygon. Polygon provides consolidated market data. Consolidated stock market data is an aggregated reporting of all securities exchanges’ and alternative trading venues’ quote and trade data. It is the most relied upon type of market data, providing investors and traders globally with a unified view of U.S. stock market prices and volumes. It also underpins the National Best Bid and Offer (NBBO), which provides investors with a continuous view of the best available displayed buy and sell prices, and through Rule 611 ensures that investors receive the best available displayed prices on their trades, with a few exceptions. For more info see the \href{https://polygon.io/docs/#get_v2_aggs_ticker__ticker__range__multiplier___timespan___from___to__anchor}{Polygon.io API Documentation for the Aggregate endpoint}
#' @param ticker \code{(character)} Specify which symbol(s) you want to call by inserting ticker(s) as a single string or character vector.
#' @param multiplier \code{(integer)} Size of the timespan multiplier. Defaults to 1.
#' @param timespan \code{(character)} Size of the time window i.e "minute" (m), "hour" (h), "day"(d), "week"(w), month"(M/mo), "quarter"(q), or "year"(y). Defaults to day.
#' @param from \code{(character/Date/POSIXct)} The starting date for the pricing data in the form YYYY-MM-DD. Datetimes/POSIXct will be coerced to Dates. 
#' @param to \code{(character/Date/POSIXct)} The ending date for the pricing data in the form YYYY-MM-DD. Datetimes/POSIXct will be coerced to Dates.
#' @param unadjusted \code{(logical)} Set to true if the results should **NOT** be adjusted for splits. Defaults to FALSE.
#' @return \code{list} object for each ticker symbol containing a \code{data.frame} with the following columns:
#' \itemize{
#'  \item{\code{time}}{  the time of the bar as \code{POSIXct} in yyyy-mm-dd for timeframe = day, and yyyy-mm-dd hh:mm:ss for timeframes < day}
#'  \item{\code{open}}{  open price as a numeric object.}
#'  \item{\code{high}}{  high price as a numeric object.}
#'  \item{\code{low}}{  low price as a numeric object.}
#'  \item{\code{close}}{  close price as a numeric object.}
#'  \item{\code{volume}}{  volume (in millions) as a numeric object.}
#'  \item{\code{vw}}{ weighted volume? *Note:* Polygon does not document what this variable is. }
#'  \item{\code{n}}{ Number of items in aggregate window }
#' }
#' each \code{data.frame} has a "query" attribute with the following metadata for the query returned by the Polygon API (accessed via `attr(df, "query")`):
#' \itemize{
#' \item{\code{ticker}} {\code{(string)} Ticker symbol requested}
#' \item{\code{status}} {\code{(string)} Status of the response}
#' \item{\code{queryCount}} {\code{(integer)} Number of aggregate ( min or day ) used to generate the response}
#' \item{\code{resultsCount}} {\code{(integer)} Total number of results generated}
#' \item{\code{adjusted}} {\code{(logical)} If this response was adjusted for splits}
#' }
#' @examples
#' # Getting default pricing data on AMZN (daily): 
#' get_poly_agg_quote(ticker = "AMZN",from = "2019-04-01", to = "2019-04-12")
#' # Getting minute pricing data on AMZN: 
#' get_poly_agg_quote("AMZN", from = "2019-04-11", to = "2019-04-12", timespan = "minute")
#' # Getting quarterly pricing data on AMZN: 
#' get_poly_agg_quote("AMZN", from = "2018-01-01", to = "2019-04-12", timespan = "quarter")
#' # Getting yearly pricing data on AMZN: 
#' get_poly_agg_quote("AMZN", from = "2015-01-01", to = "2019-12-31", timespan = "year")
#' @importFrom lubridate as_date
#' @importFrom purrr map
#' @export

# For DEBUG
list2env(list(ticker = "AMZN", multiplier = 3, timeframe = "day", from = lubridate::ymd("2019-12-01"), to = lubridate::today(), unadjusted = F), envir = .GlobalEnv)
get_poly_agg_quote <- function(ticker = NULL, multiplier = 1, timespan = "day", from = NULL, to = NULL, unadjusted = FALSE){
  if(is.null(ticker)){
    stop("Please enter a stock ticker.")
  }
  if(is.null(from) | is.null(to)){
    stop("Please enter a date in the 'from' or 'to' argument.")
  }
  # quick detection of timespan abbreviations
  if (timespan == "m") {
    timespan <- "minute"
  } else if (timespan == "h") {
    timespan <- "hour"
  } else if (timespan == "d") {
    timespan <- "day"
  } else if (timespan == "w") {
    timespan <- "week"
  } else if (timespan == "M" || timespan == "mo") {
    timespan <- "month"
  } else if (timespan == "q") {
    timespan <- "quarter"
  } else if (timespan == "y") {
    timespan <- "year"
  }
  #Set URL
  path_url = get_url_poly()
  
  # Add names such that output list is labelled
  names(ticker) <- ticker
  
  agg_quotes <- purrr::map(ticker, ~{
    agg_quote <- list(results = list(n = NA))
    .ctr <- 1
    
      message(paste0("Retrieving data for: ", .x))
      full_path_url = paste0(path_url,"/v2/aggs/ticker/",.x,"/range/",multiplier,"/",timespan,"/",lubridate::as_date(from),"/",lubridate::as_date(to),"?unadjusted=",unadjusted,"&apiKey=",Sys.getenv("APCA-LIVE-API-KEY-ID")) 
      message(paste0("Fetching ", full_path_url))
      #Send Request
      agg_quote = httr::GET(url = full_path_url)
      if (agg_quote$status_code != 200) {
        message(paste("Ticker", .x, "returned status code", agg_quote$status_code, "- Returning NULL"))
        return(NULL)
      }
      agg_quote = response_text_clean(agg_quote)
      if (length(agg_quote$results) == 0) {
        message(paste("Ticker", .x, "returned no data", "- Returning response metadata only"))
        return(agg_quote[1:5])
      }
      # Get the query meta-info returned by the API
      query <- agg_quote[1:5]
      # If there are any gaps in the data indicated by NA values for n
      # if (anyNA(agg_quote$results$n)) {
      #   # transform to to datetime
      #   .t = as.POSIXct(agg_quote$results$t/1000, tz = Sys.timezone(), origin = lubridate::origin)
      #   # Get the cutoff point for full data
      #   .cutoff <- agg_quote$results$n %>% is.na %>% which %>% {min(.) - 1}
      #   # Generate a new from date
      #   from <- lubridate::as_date(.t[.cutoff])
      #   message(paste("Missing ata detected, re-querying for range:", paste0(c(from,to), collapse = " - ")))
      #   if (.ctr == 1) {
      #     results <- agg_quote$results[1:.cutoff,]
      #   } else {
      #     results <- cbind.data.frame(results, agg_quote$results)
      #   }
      #   
      #   .ctr <- .ctr + 1
      # }
    
    
    # Generate new calls, filling in missing data, until no data is missing
    
    # convert from nanoseconds to seconds
    agg_quote$results$t = agg_quote$results$t/1000
    
    agg_quote <- bars_transform(agg_quote["results"])$results
    attr(agg_quote, "query") <- query
    return(agg_quote)
  })
  return(agg_quotes)
}
#----------------------------------------------------------------------------------------------









#----------------------------------------------------------------------------------------------
#' Get Polygon Stock Split Information
#' 
#' This function provides stock split data for the specified ticker from Polygon.
#' @param ticker Specify which symbol you want to call by inserting ticker as a string.
#' @return A list object containing all information the API responds with. 
#' @examples
#' # Getting default pricing data on AMZN (daily): 
#' get_poly_stock_splits(ticker = "AMZN")
#' @export
get_poly_stock_splits <- function(ticker=NULL){
  if(is.null(ticker)){
    stop("Please enter the stocks ticker.")
  }
  
  #Set URL
  path_url = get_url_poly()
  full_path_url = paste0(path_url,"/v2/reference/splits/",ticker,"?apiKey=",Sys.getenv("APCA-LIVE-API-KEY-ID"))
  
  #Send Request
  split_info = last_price_details = httr::GET(url = full_path_url)
  split_info = response_text_clean(split_info)
  return(split_info)
}
#----------------------------------------------------------------------------------------------













#----------------------------------------------------------------------------------------------
#' Get Polygon Historic Trades Information
#' 
#' This function provides historic trade data form Polygon on the ticker specified. It returns a list with values such as date, bid size / ask size, the exchanges for bid / ask, latency, each trade/quote listed for that date, time, etc.
#' @param ticker Specify which symbol you want to call by inserting ticker as a string.
#' @param type Get informaiton on either "trades" or "quotes".
#' @param date Specify the date for which you are requesting.
#' @return A list object containing all information the API responds with. 
#' @examples
#' # Getting historic trade data on AMZN: 
#' get_poly_historic_info(ticker = "AMZN", type = "trades", date = "2019-04-05")
#' # Getting historic pricing data on AMZN: 
#' get_poly_historic_info(ticker = "AMZN", type = "quotes", date = "2019-04-05")
#' @export
get_poly_historic_info <- function(ticker=NULL,type=NULL,date=NULL){
  if(is.null(ticker) | is.null(type) | is.null(date)){
    stop("Please enter values for ticker, type, and date.")
  }
  
  #Set URL 
  path_url = get_url_poly()
  full_path_url = paste0(path_url,"/v1/historic/",type,"/",ticker,"/",date,"?apiKey=",Sys.getenv("APCA-LIVE-API-KEY-ID"))
  
  #Send Request
  historic = last_price_details = httr::GET(url = full_path_url)
  historic = response_text_clean(historic)
  
  
  #Create a column for date/time
  historic$ticks$t = as.POSIXct(historic$ticks$t/1000, origin = "1970-01-01")
  return(historic)
}
#----------------------------------------------------------------------------------------------




#----------------------------------------------------------------------------------------------
#' Get Polygon Last Price
#' 
#' This function provides the last listed price from Polygon. A list is returned with values such as the last price, last size, last exchange, and last timestamp.
#' @param ticker Specify which symbol you want to call by inserting ticker as a string.
#' @return A list object containing all information the API responds with. 
#' @examples
#' # Getting the last listed price for AMZN: 
#' get_poly_last_price("AMZN")
#' @export
get_poly_last_price <- function(ticker = NULL){
  if(is.null(ticker)){
    stop("Please enter the stocks ticker.")
  }
  
  #Set URL 
  path_url = get_url_poly()
  full_path_url = paste0(path_url,"/v1/last/stocks/",ticker,"?apiKey=",Sys.getenv("APCA-LIVE-API-KEY-ID"))
  
  #Send Request
  last_price_details = httr::GET(url = full_path_url)
  last_price_details = response_text_clean(last_price_details)
  
  #Convert unix epoch timestamp to readable date
  last_price_details$last$timestamp = as.POSIXct(last_price_details$last$timestamp/1000, origin= "1970-01-01")
  return(last_price_details)
}
#----------------------------------------------------------------------------------------------











#----------------------------------------------------------------------------------------------
#' Get Polygon Last Trade
#' 
#' This function provides the last listed trade from Polygon for the ticker specified. A list is returned with values such as the last bid / ask price, bid / ask size, bid / ask exchange, and last trade timestamp.
#' @param ticker Specify which symbol you want to call by inserting ticker as a string.
#' @return A list object containing all information the API responds with. 
#' @examples
#' # Getting the last listed trade for AMZN: 
#' get_poly_last_trade("AMZN")
#' @export
get_poly_last_trade <- function(ticker = NULL){
  if(is.null(ticker)){
    stop("Please enter the stocks ticker.")
  }
  
  #Set URL 
  path_url = get_url_poly()
  full_path_url = paste0(path_url,"/v1/last_quote/stocks/",ticker,"?apiKey=",Sys.getenv("APCA-LIVE-API-KEY-ID"))
  
  #Send Request
  last_trade_details = httr::GET(url = full_path_url)
  last_trade_details = response_text_clean(last_trade_details)
  
  #Convert unix epoch timestamp to readable date
  last_trade_details$last$timestamp = as.POSIXct(last_trade_details$last$timestamp/1000, origin= "1970-01-01")
  return(last_trade_details)
}
#----------------------------------------------------------------------------------------------














#----------------------------------------------------------------------------------------------
#' Get Polygon Daily OHLCV & After Hours
#' 
#' This function provides the last OHLCV data, including after hours data from Polygon for the ticker specified. 
#' @param ticker Specify which symbol you want to call by inserting ticker as a string.
#' @param date Specify the date for which you are requesting.
#' @return A list object containing all information the API responds with. 
#' @examples
#' # Getting the last listed trade for AMZN: 
#' get_poly_ohlc("AMZN", date = "2019-03-20")
#' @export
get_poly_ohlc <- function(ticker=NULL, date=NULL){
  if(is.null(ticker)){
    stop("Please enter the stocks ticker.")
  } 
  if(is.null(date)){
    date <- Sys.Date()
  }
  
  #Set URL 
  path_url = get_url_poly()
  full_path_url = paste0(path_url,"/v1/open-close/",ticker,"/",date,"?apiKey=",Sys.getenv("APCA-LIVE-API-KEY-ID"))
  
  #Send Request
  open_close = httr::GET(url = full_path_url)
  open_close = response_text_clean(open_close)
  return(open_close)
}
#----------------------------------------------------------------------------------------------







#----------------------------------------------------------------------------------------------
#' Get Polygon Previous Day Close for Ticker
#' 
#' This function provides the Previous Day close data from Polygon for the ticker specified. 
#' @param ticker Specify which symbol you want to call by inserting ticker as a string.
#' @return A list object containing all information the API responds with. 
#' @examples
#' # Getting the last listed trade for AMZN: 
#' get_poly_prev_dayclose("AMZN")
#' @export
get_poly_prev_dayclose <- function(ticker=NULL){
  if(is.null(ticker)){
    stop("Please enter the stocks ticker.")
  }
  
  #Set URL
  path_url = get_url_poly()
  full_path_url = paste0(path_url,"/v2/aggs/ticker/",ticker,"/prev","?apiKey=",Sys.getenv("APCA-LIVE-API-KEY-ID"))
  
  #Send Request
  prev_close = httr::GET(url = full_path_url)
  prev_close = response_text_clean(prev_close)
  
  #Convert unix epoch timestamp to readable date
  prev_close$results$t = as.POSIXct(prev_close$results$t/1000, origin= "1970-01-01")
  return(prev_close)
}
#----------------------------------------------------------------------------------------------




# PACKAGE FUNCTIONS END #