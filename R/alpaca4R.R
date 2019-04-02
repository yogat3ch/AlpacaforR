# OUR URL & HEADER VARIABLES SENT IN EACH API REQUEST #
#===================================================================================================
#To connect to our paper.account. Live is -> https://api.alpaca.markets
url <- if(Sys.getenv("APCA-LIVE")) "https://api.alpaca.markets" else "https://paper-api.alpaca.markets"

#To access data API
url_data <- "https://data.alpaca.markets"

#Create headers object to send our Key & Secret ID in our request.
headers <- httr::add_headers('APCA-API-KEY-ID' = Sys.getenv("APCA-API-KEY-ID"), 
                             'APCA-API-SECRET-KEY' = Sys.getenv("APCA-API-SECRET-KEY"))
#===================================================================================================




# PACKAGE FUNCTIONS #
#===================================================================================================
#' Clean Text from Server Response function
#' 
#' Clean the response text (usually in unreadable json) and convert to a readable format using fromJSON. 
#' @param dat The response from our server GET request usually in a json format.
#' @export
response_text_clean <- function(dat){
  
  dat = httr::content(dat, as = "text", encoding = "UTF-8")
  dat = jsonlite::fromJSON(dat)
  return(dat)
}





#Functions to interact with Alpaca API

#' Get Account function
#'
#' The accounts API serves important information related to an account, including account status, funds available for trade, funds available for withdrawal, and various flags relevant to an account’s ability to trade.
#' @param url  URL to your user live or paper API.
#' @param headers  Header object containing your KeyID & SecretID to your user API. Use add_headers('APCA-API-KEY-ID' = value, 'APCA-API-SECRET-KEY' = value) from httr package.
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
#' get_account(url = url, headers = headers)
#' @export
get_account <- function(url, headers){
  
  account = httr::GET(url = paste0(url,"/v1/account",collapse = ""), headers)
  account = response_text_clean(account)
  return(account)
}
#get_account(url = url, headers = headers)







#' Get Positions function
#'
#' The positions API provides information about an account’s current open positions. The response will include information such as cost basis, shares traded, and market value, which will be updated live as price information is updated.
#' @param url  URL to your user live or paper API.
#' @param headers  Header object containing your KeyID & SecretID to your user API. Use add_headers('APCA-API-KEY-ID' = value, 'APCA-API-SECRET-KEY' = value) from httr package.
#' @return "asset_id"  Asset ID as a string.
#' @return "symbol"  Symbol of the asset as a string.
#' @return "exchange"  Exchange name of the asset as a string.
#' @return "asset_class"  Asset class name as a string.
#' @return "avg_entry_price"  Average entry price of the position as a string.
#' @return "qty" The number of shares as a string.
#' @return "side" long/short exposure as a string.
#' @return "market_value"  Total dollar amount of the position as a string.
#' @return "cost_basis"  Total cost basis in dollar as a string.
#' @return "unrealized_pl"  Unrealized profit/loss in dollar as a string.
#' @return "unrealized_plpc"  Unrealized profit/loss percent (by a factor of 1) as a string.
#' @return "unrealized_intraday_pl"  Unrealized profit/loss in dollar for the day as a string.
#' @return "unrealized_intraday_plpc"  Unrealized profit/loss percent (by a factor of 1) as a string.
#' @return "current_price"  Current asset price per share as a string.
#' @return "lastday_price"  Last day’s asset price per share as a string.
#' @return "change_today"  Percent change from last day price (by a factor of 1) as a string.
#' @examples 
#' get_positions(url = url, headers = headers) 
#' @export
get_positions <- function(url, headers, ticker = NULL){
  
  positions = httr::GET(url = paste0(url,"/v1/positions",collapse = ""), headers) 
  positions = response_text_clean(positions)
  if (is.null(ticker)) return(positions) else return(subset(positions,symbol == ticker))
}
#get_positions(url = url, headers = headers) 









#' Get Orders function
#' 
#' The orders API allows a user to monitor, place and cancel their orders with Alpaca.
#' @param url  URL to your user live or paper API.
#' @param headers  Header object containing your KeyID & SecretID to your user API. Use add_headers('APCA-API-KEY-ID' = value, 'APCA-API-SECRET-KEY' = value) from httr package.
#' @param status Order status to be queried "open, closed or all". Defaults to open as a string.
#' @param from The response will include only orders submitted after this date exclusive as a timestamp object.
#' @return "id" order id as a string.
#' @return "client_order_id" client unique order id as a string.
#' @return "created_at" nullable When the order was created as a timestamp object.
#' @return "updated_at" nullable When the order was created as a timestamp object.
#' @return "submitted_at" nullable When the order was created as a timestamp object.
#' @return "filled_at" nullable When the order was created as a timestamp object.
#' @return "expired_at" nullable When the order was created as a timestamp object.
#' @return "canceled_at" nullable When the order was created as a timestamp object.
#' @return "asset_id" asset ID as a string.
#' @return "symbol" Asset symbol as a string.
#' @return "exchange" Asset exchange as a string.
#' @return "asset_class" Asset class as a string.
#' @return "qty" Ordered quantity as a string.
#' @return "filled_qty" Filled quantity as a string.
#' @return "type" Valid values: market, limit, stop, stop_limit as a string.
#' @return "side" Valid values: buy, sell as a string.
#' @return "time_in_force" time in force selected asa string.
#' @return "limit_price" Limit price as a string.
#' @return "stop_price" Stop price as a string.
#' @return "status" Status of the order as a string.
#' @examples 
#' get_orders(url = url, headers = headers, status = "open")
#' @export
get_orders <- function(url, headers, status, from=NULL){
  
  if(is.null(from)){
    orders = httr::GET(url = paste0(url,"/v1/orders?status=",status,collapse = ""), headers)
    orders = response_text_clean(orders)
  } else {
    orders = httr::GET(url = paste0(url,"/v1/orders?status=",status,"&after=",from,"T09:30:00-04:00",collapse = ""), headers)
    orders = response_text_clean(orders)
  }
  return(orders)
}
get_orders(url = url, headers = headers, status = "open")








#' Submit Order function
#' 
#' Places a new order of the specified stock, quantity, buy / sell, type of order, time in force, and limit / stop prices if selected.
#' @param url  URL to your user live or paper API.
#' @param headers  Header object containing your KeyID & SecretID to your user API. Use add_headers('APCA-API-KEY-ID' = value, 'APCA-API-SECRET-KEY' = value) from httr package.
#' @param ticker The stock's symbol as a string.
#' @param qty The amount of shares to trade as a string.
#' @param side The side of the trade. I.E buy or sell as a string.
#' @param type The type of trade order. I.E market,limit,stop,stoplimit,etc as a string.
#' @param time_in_force The type of time order. I.E day, gtc, etc as a string.
#' @param limit_price If order type was a limit, then enter the limit price here as a string.
#' @param stop_price If order tyope was a stop, then enter the stop price here as a string.
#' @examples 
#' submit_order(url = url, headers = headers, ticker = "AMZN", qty = "10", side = "buy", type = "limit", time_in_force = "day", limit_price = "1000")
#' @export
submit_order <- function(url, headers, ticker, qty, side, type, time_in_force, limit_price = NULL, stop_price = NULL){
  
  #Create body with order details, most common is a named list 
  bodyl <- list(symbol=ticker, qty=qty, side = side, type = type, time_in_force = time_in_force, limit_price = limit_price, stop_price = stop_price)
  
  orders = httr::POST(url = paste0(url,"/v1/orders",collapse = ""), body = bodyl, encode = "json",headers)
  orders = response_text_clean(orders)
  return(orders)
}
#submit_order(url = url, headers = headers, ticker = "AMZN", qty = "100", side = "buy", type = "limit", time_in_force = "day", limit_price = "120")








#' Cancel Order function
#' 
#' Attempts to cancel an open order. If the order is no longer cancelable (example: status=order_filled), the server will respond with status 422, and reject the request.
#' @param url  URL to your user live or paper API.
#' @param headers  Header object containing your KeyID & SecretID to your user API. Use add_headers('APCA-API-KEY-ID' = value, 'APCA-API-SECRET-KEY' = value) from httr package.
#' @param ticker The stock's symbol as a string.
#' @examples 
#' cancel_order(url = url, headers = headers, ticker = "AMZN")
#' @export
cancel_order <- function(url, headers, ticker){

  #Gather the open order ID for the symbol specified
  open_orders = get_orders(url = url, headers, status = "open")
  order_id = subset(open_orders, symbol == ticker)$id
  
  #Cancel the order through the order_id
  cancel = httr::DELETE(url = paste0(url,"/v1/orders/",order_id,collapse = ""), headers)
  cat(paste("Order ID", order_id, "was successfully canceled"))
}
#cancel_order(url = url, headers = headers, ticker = "AMZN")





#' Get Assets function
#' 
#' The assets API serves as the master list of assets available for trade and data consumption from Alpaca. Assets are sorted by asset class, exchange and symbol. Some assets are only available for data consumption via Polygon, and are not tradable with Alpaca. These assets will be marked with the flag tradable=false.
#' @param url  URL to your user live or paper API.
#' @param headers  Header object containing your KeyID & SecretID to your user API. Use add_headers('APCA-API-KEY-ID' = value, 'APCA-API-SECRET-KEY' = value) from httr package.
#' @param ticker The stock's symbol as a string.
#' @return "id Asset" ID as a string.
#' @return "asset_class" us_equity as a string.
#' @return "exchange" AMEX, ARCA, BATS, NYSE, NASDAQ or NYSEARCA as a string.
#' @return "symbol" Stock symbol as a string.
#' @return "status" active or inactive as a string.
#' @return "tradeable" Asset is tradable on Alpaca or not as a boolean.
#' @examples 
#' get_assets(url = url, headers = headers)
#' @export
get_assets <- function(url, headers, ticker = NULL){
  
  #We could specify symbols instead of getting all assets
  if(is.null(ticker)){
    assets = httr::GET(url = paste0(url,"/v1/assets",collapse = ""), headers)
    assets = response_text_clean(assets)
  } else{
    assets = httr::GET(url = paste0(url,"/v1/assets/",ticker,collapse = ""), headers)
    assets = response_text_clean(assets)
  } 
  return(assets)
}
#get_assets(url = url, headers = headers)









#' Get Calendar function
#' 
#' The calendar API serves the full list of market days from 1970 to 2029. It can also be queried by specifying a start and/or end time to narrow down the results. In addition to the dates, the response also contains the specific open and close times for the market days, taking into account early closures.
#' @param url  URL to your user live or paper API.
#' @param headers  Header object containing your KeyID & SecretID to your user API. Use add_headers('APCA-API-KEY-ID' = value, 'APCA-API-SECRET-KEY' = value) from httr package.
#' @return "date" Date string. in date format as a string.
#' @return "open" The time the market opens at on this date in hour:min format as a string.
#' @return "close" The time the market closes at on this date in hour:min format as a string.
#' @examples 
#' get_calendar(url = url, headers = headers)
#' @export
get_calendar <- function(url, headers){
  
  calendar = httr::GET(url = paste0(url,"/v1/calendar",collapse = ""), headers)
  calendar =  response_text_clean(calendar)
  return(calendar)
}
#get_calendar(url = url, headers = headers)







#' Get Clock function
#' 
#' The clock API serves the current market timestamp, whether or not the market is currently open, as well as the times of the next market open and close.
#' @param url  URL to your user live or paper API.
#' @param headers  Header object containing your KeyID & SecretID to your user API. Use add_headers('APCA-API-KEY-ID' = value, 'APCA-API-SECRET-KEY' = value) from httr package.
#' @return "timestamp" Current timestamp as a string.
#' @return "is_open" Whether or not the market is open as a boolean.
#' @return "next_open" Next market open timestamp as a string.
#' @return "next_close" Next market close timestamp as a string.
#' @examples 
#' get_clock(url = url, headers = headers)
#' @export
get_clock <- function(url, headers){

  clock = httr::GET(url = paste0(url,"/v1/clock",collapse = ""), headers)
  clock = response_text_clean(clock)
  return(clock)
}
#get_clock(url = url, headers = headers)










#' Get Bars function
#' 
#' The bars API provides time-aggregated price and volume data for a single stock or multiple.
#' @param url  URL to your user live or paper API.
#' @param headers  Header object containing your KeyID & SecretID to your user API. Use add_headers('APCA-API-KEY-ID' = value, 'APCA-API-SECRET-KEY' = value) from httr package.
#' @param ticker The stock or stocks (in vector format) that you want.
#' @param from The starting date for the pricing data
#' @return "t" the beginning time of this bar as a Unix epoch in seconds as a integer.
#' @return "o" open price as a numberic object.
#' @return "h" high price as a numberic object.
#' @return "l" low price as a numberic object.
#' @return "c" close price as a numberic object.
#' @return "v" volume as a numberic object.
#' @examples 
#' get_bars(url_data = url_data, headers = headers, ticker = "INTC", from = "2019-03-20")
#' @export
get_bars <- function(url_data, headers, ticker, from){
  
  #Check for multiple tickers or just one
  ticker <- ifelse(length(ticker) > 1, paste0(ticker, collapse = ","), ticker)
                                                                                    #If summer, SUBTRACT 4 HOURS FROM UTC FOR NY, if Winter, SUBTRACT 5 HOURS FROM UTC FOR NY
  bars = httr::GET(url = paste0(url_data,"/v1/bars","/1D","?symbols=",ticker,"&start=",from,"T09:30:00-04:00",collapse = ""), headers)
  bars = response_text_clean(bars)
  return(bars)
}
#get_bars(url_data = url_data, headers = headers, ticker = "INTC", from = "2019-03-20")

#===================================================================================================

