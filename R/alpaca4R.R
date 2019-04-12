
# PACKAGE FUNCTIONS #
#===================================================================================================

#----------------------------------------------------------------------------------------------
#' Clean Text from Server Response function
#' 
#' Clean the response text (usually in unreadable json) and convert to a readable format using fromJSON. 
#' @param dat The response from our server GET request usually in a json format.
#' @return The response in a readable format as a list.
#' @export
response_text_clean <- function(dat){
  
  dat = httr::content(dat, as = "text", encoding = "UTF-8")
  dat = jsonlite::fromJSON(dat)
  return(dat)
}
#----------------------------------------------------------------------------------------------




#----------------------------------------------------------------------------------------------
#' Get URL for Server Request function
#' 
#' Get the correct URL for the Server Request that is sent to interact with the API. If the user is on a paper account, then the paper account URL will be returned. 
#' @param live TRUE / FALSE if you are connecting to a paper account. Default to NULL, so it will use the live url if nothing is provided.
#' @return The correct URL according to account type (live or paper) that will be sent in the API request.
#' @export
get_url <- function(live=NULL){
    
    if(is.null(live)){
      url <- "https://paper-api.alpaca.markets"
    } 
      else{
      url <- ifelse(live, 
                    "https://api.alpaca.markets",
                    "https://paper-api.alpaca.markets")
    }
  return(url)
}
#----------------------------------------------------------------------------------------------









#----------------------------------------------------------------------------------------------
#' Get Headers for Server Request function
#'
#' @return The correct headers that will be sent in the API request.
#' @export
get_headers <- function(){
  headers = httr::add_headers('APCA-API-KEY-ID' = Sys.getenv("APCA-API-KEY-ID"), 
                              'APCA-API-SECRET-KEY' = Sys.getenv("APCA-API-SECRET-KEY"))  
  return(headers)
}
#----------------------------------------------------------------------------------------------











#Functions to interact with Alpaca API
#----------------------------------------------------------------------------------------------
#' Get Account function
#'
#' The accounts API serves important information related to an account, including account status, funds available for trade, funds available for withdrawal, and various flags relevant to an account’s ability to trade.
#' @param live TRUE / FALSE if you are connecting to a live account. Default to FALSE, so it will use the paper url if nothing was specified.
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
#' get_account(live = FALSE)
#' Which is similar to:
#' get_account()
#' For access to live accounts, you must submit as live = TRUE
#' get_account(live = TRUE)
#' @export
get_account <- function(live = FALSE){
  #Set URL & Headers
  url = get_url(live)
  headers = get_headers()
  #Send Request
  account = httr::GET(url = paste0(url,"/v1/account"), headers)
  account = response_text_clean(account)
  return(account)
}
#----------------------------------------------------------------------------------------------









#----------------------------------------------------------------------------------------------
#' Get Positions function
#'
#' The positions API provides information about an account’s current open positions. The response will include information such as cost basis, shares traded, and market value, which will be updated live as price information is updated.
#' @param ticker Specify which symbol you want to call by inserting ticker as a string.
#' @param live TRUE / FALSE if you are connecting to a live account. Default to FALSE, so it will use the paper url if nothing was specified.
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
#' get_positions(ticker = "AAPL", live = FALSE)
#' get_positions(ticker = "AAPL")
#' This gets all positions:
#' get_positions()
#' @export
get_positions <- function(ticker = NULL, live = FALSE){
  #Set U, live = FALSERL & Headers
  url = get_url(live)
  headers = get_headers()
  #Send Request
  positions = httr::GET(url = paste0(url,"/v1/positions"), headers) 
  positions = response_text_clean(positions)
  if(length(positions) == 0) cat("No positions are open at this time.")
  else if(is.null(ticker)) return(positions) else return(subset(positions,symbol == ticker))
}
#----------------------------------------------------------------------------------------------









#----------------------------------------------------------------------------------------------
#' Get Orders function
#' 
#' The orders API allows a user to monitor, place and cancel their orders with Alpaca.
#' @param ticker Specify which symbol you want to call by inserting ticker as a string.
#' @param status Order status to be queried "open, closed or all". Defaults to open as a string.
#' @param from The response will include only orders submitted after this date exclusive as a timestamp object.
#' @param silent A logical TRUE / FALSE on if you want the "no orders to cancel" message to print to the console. Default to FALSE.
#' @param live TRUE / FALSE if you are connecting to a live account. Default to FALSE, so it will use the paper url if nothing was specified.
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
#' get_orders(live = FALSE)
#' get_orders(status = "all")
#' For a specific ticker:
#' get_orders(ticker = "AAPL", status = "all")
#' @export
get_orders <- function(ticker = NULL, status = "open", from = NULL, silent = FALSE, live = FALSE){
  #Set URL & Headers
  url = get_url(live)
  headers = get_headers()
  
  #Send Request
  if(!is.null(ticker)){
      if(!is.null(from)){
        orders = httr::GET(url = paste0(url,"/v1/orders?status=",status,"&after=",from,"T09:30:00-04:00"), headers)
        orders = response_text_clean(orders)
        if(length(orders) != 0) orders = dplyr::filter(orders, symbol %in% ticker)
    } else {
        orders = httr::GET(url = paste0(url,"/v1/orders?status=",status), headers)
        orders = response_text_clean(orders)
        if(length(orders) != 0) orders = dplyr::filter(orders, symbol %in% ticker)
    }
  }else if(is.null(ticker)){
      if(!is.null(from)){
        orders = httr::GET(url = paste0(url,"/v1/orders?status=",status,"&after=",from,"T09:30:00-04:00"), headers)
        orders = response_text_clean(orders)
    } else{
        orders = httr::GET(url = paste0(url,"/v1/orders?status=",status), headers)
        orders = response_text_clean(orders)
    }
  }
  
  if(length(orders) == 0){
    if(silent == FALSE) cat(paste("No",status,"orders",if(!is.null(ticker))paste("for",ticker),"at this time.",'Set status = "all" to see all orders.'))
  }  else return(orders)
}
#----------------------------------------------------------------------------------------------









#----------------------------------------------------------------------------------------------
#' Submit Order function
#' 
#' Places a new order of the specified stock, quantity, buy / sell, type of order, time in force, and limit / stop prices if selected.
#' @param ticker The stock's symbol as a string.
#' @param qty The amount of shares to trade as a string.
#' @param side The side of the trade. I.E buy or sell as a string.
#' @param type The type of trade order. I.E market,limit,stop,stoplimit,etc as a string.
#' @param time_in_force The type of time order. I.E day, gtc, opg as a string. Defaults to "day".
#' @param limit_price If order type was a limit, then enter the limit price here as a string.
#' @param stop_price If order tyope was a stop, then enter the stop price here as a string.
#' @param live TRUE / FALSE if you are connecting to a live account. Default to FALSE, so it will use the paper url if nothing was specified.
#' @examples 
#' For market order:
#' submit_order(ticker = "AAPL", qty = "100", side = "buy", type = "market")
#' Or you can submit a limit order:
#' submit_order(ticker = "AAPL", qty = "100", side = "buy", type = "limit", limit_price = "120")
#' @export
submit_order <- function(ticker, qty, side, type, time_in_force = "day", limit_price = NULL, stop_price = NULL, live = FALSE){
  #Set URL & Headers
  url = get_url(live)
  headers = get_headers()
  
  #Create body with order details, most common is a named list 
  bodyl <- list(symbol=ticker, qty=qty, side = side, type = type, time_in_force = time_in_force, limit_price = limit_price, stop_price = stop_price)
  
  #Send Request
  orders = httr::POST(url = paste0(url,"/v1/orders"), body = bodyl, encode = "json",headers)
  orders = response_text_clean(orders)
  return(orders)
}
#----------------------------------------------------------------------------------------------









#----------------------------------------------------------------------------------------------
#' Cancel Order function
#' 
#' Attempts to cancel an open order. 
#' @param ticker The stock's symbol as a string.
#' @param order_id You can specify which order to cancel by entering order_id. Defaults to most recent open order.
#' @param live TRUE / FALSE if you are connecting to a live account. Default to FALSE, so it will use the paper url if nothing was specified.
#' @examples 
#' cancel_order(ticker = "AAPL")
#' Or you can instead cancel by the order_id:
#' cancel_order(order_id = VALUE)
#' @export
cancel_order <- function(ticker, order_id = NULL, live = FALSE){
  #Set URL & Headers
  url = get_url(live)
  headers = get_headers()
  
  #Gather the open order ID for the symbol specified
  open_orders = get_orders(status = "open", live = live, silent = TRUE)
  
  #Check if any open orders before proceeding. 
  if(is.null(open_orders)){
    cat("There are no orders to cancel at this time.")
  } else if(is.null(order_id)){
    order_id = subset(open_orders, symbol == ticker)$id
    #Send Request & Cancel the order through the order_id
    cancel = httr::DELETE(url = paste0(url,"/v1/orders/",order_id), headers)
    cat(paste("Order ID", order_id,"for",ticker, "was successfully canceled."))
  } else{
    cancel = httr::DELETE(url = paste0(url,"/v1/orders/",order_id), headers)
    cat(paste("Order ID", order_id, "was successfully canceled."))
  }
}
#----------------------------------------------------------------------------------------------









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
#' Get a specific asset:
#' get_assets(ticker = "AAPL")
#' @export
get_assets <- function(ticker = NULL){
  #Set URL & Headers
  url = get_url()
  headers = get_headers()
  
  #Send Request
  if(is.null(ticker)){
    assets = httr::GET(url = paste0(url,"/v1/assets"), headers)
    assets = response_text_clean(assets)
  } else{
    assets = httr::GET(url = paste0(url,"/v1/assets/",ticker), headers)
    assets = response_text_clean(assets)
  } 
  return(assets)
}
#----------------------------------------------------------------------------------------------









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
#' Get all dates:
#' get_calendar()
#' @examples 
#' Get specific date range:
#' get_calendar(from = "2019-01-01", to = "2019-04-01")
#' @export
get_calendar <- function(from = NULL, to = NULL){
  #Set URL & Headers
  url = get_url()
  headers = get_headers()
  if(!is.null(to)) to <- as.character(as.Date(to)+1)
  if(!is.null(from)) from <- as.character(as.Date(from))
  
  if(is.null(from) & is.null(to)){
  calendar = httr::GET(url = paste0(url,"/v1/calendar"), headers)
  calendar =  response_text_clean(calendar)
  } else{ 
  calendar = httr::GET(url = paste0(url,"/v1/calendar","?start=",from,"&end=",to), headers)
  calendar =  response_text_clean(calendar)
  }
  return(calendar)
}
#----------------------------------------------------------------------------------------------









#----------------------------------------------------------------------------------------------
#' Get Clock function
#' 
#' The clock API serves the current market timestamp, whether or not the market is currently open, as well as the times of the next market open and close.
#' @return "timestamp" Current timestamp as a string.
#' @return "is_open" Whether or not the market is open as a boolean.
#' @return "next_open" Next market open timestamp as a string.
#' @return "next_close" Next market close timestamp as a string.
#' @examples 
#' get_clock()
#' @export
get_clock <- function(){
  #Set URL & Headers
  url = get_url()
  headers = get_headers()
  
  #Send Request
  clock = httr::GET(url = paste0(url,"/v1/clock"), headers)
  clock = response_text_clean(clock)
  return(clock)
}
#----------------------------------------------------------------------------------------------









#----------------------------------------------------------------------------------------------
#' Get Bars function
#' 
#' The bars API provides time-aggregated price and volume data for a single stock or multiple.
#' @param ticker The stock or stocks (in vector format) that you want.
#' @param from The starting date for the pricing data. Default is the last 5 trading days.
#' @param to The ending date for the pricing data. Default is todays date.
#' @param timeframe One of "minute", "1Min", "5Min", "15Min", "day" or "1D". minute is an alias of 1Min. Similarly, day is of 1D. Defaults to "1D" as a string.
#' @param limit The amount of bars to return per ticker. This can range from 1 to 1000. Defaults according to timeframe chosen. If timeframe "1D or day" then the limit is set to the # of days. If "15Min" the default is 250, if "5Min" the default is 500, and if "1Min or minute" then the default is the max, 1000.
#' @return "t" the beginning time of this bar as a Unix epoch in seconds as a integer.
#' @return "o" open price as a numberic object.
#' @return "h" high price as a numberic object.
#' @return "l" low price as a numberic object.
#' @return "c" close price as a numberic object.
#' @return "v" volume as a numberic object.
#' @examples
#' Getting one or more tickers: 
#' get_bars(ticker = c("INTC","MSFT"))
#' @examples 
#' Getting price data with specific date ranges and timeframes, by also limiting the amount of bars returned for each ticker.
#' get_bars(ticker = c("INTC","MSFT"), from = "2019-03-20", to = "2019-04-01", timeframe = "15Min", limit = 175)
#' @export
get_bars <- function(ticker, from = Sys.Date()-6, to = Sys.Date(), timeframe = "1D", limit = NULL){
  
  #Set Url & Headers
  url = "https://data.alpaca.markets" #Pricing data uses unique URL, see market data API documentation to learn more.
  headers = get_headers()
  
  
  #Check for multiple tickers or just one
  ticker = ifelse(length(ticker) > 1, paste0(ticker, collapse = ","), ticker)
  
  
  #Get the trading days in between the sequence so we can create date column and return values
  week_dates = get_calendar(from,to)$date
  
  
  
  #Since the max bars requested limit is at 1000, we need to bring the 1000 most recent price dates to match.
  if(length(week_dates) > 1000){
    start <- length(week_dates) - 999
    week_dates <- week_dates[start:length(week_dates)]
  }
  
  #If limit is null then set it according to timeframe.
  
  #Set bar limit by the length of week_dates (for 1D timeframe)
    if((timeframe == "1D" | timeframe == "day") & is.null(limit)){
      limit = length(week_dates)
    } else if(timeframe == "15Min" & is.null(limit)){
      limit = 250
    } else if(timeframe == "5Min" & is.null(limit)){
      limit = 500
    } else if((timeframe == "1Min" | timeframe == "minute") & is.null(limit)){
      limit = 1000
    }
  
  
  #Time fix for min bars
  if(!(timeframe == "1D" | timeframe == "day")){
    from = paste0(from,stringr::str_extract(format(Sys.time(), "%Y-%m-%dT%H:%M:%OS%z"), "T.*"))
    to = paste0(to,stringr::str_extract(format(Sys.time(), "%Y-%m-%dT%H:%M:%OS%z"), "T.*"))
  } else {
    from = paste0(from,"T09:30:00-04:00")
    to = paste0(to,"T09:30:00-04:00")
  }
  
  #Send Request                                                                                 
  bars = httr::GET(url = paste0(url,"/v1/bars/",timeframe,"?symbols=",ticker,"&limit=",limit,"&start=",from,"&end=",to), headers)
  bars = response_text_clean(bars)
  
  
  
  #Check if price data was updated yet and if not, show last 5 trading days ending yesterday.
  if(nrow(bars[[1]]) < limit){
    from = as.Date(from) - 1
    bars = httr::GET(url = paste0(url,"/v1/bars/",timeframe,"?symbols=",ticker,"&limit=",limit,"&start=",from,"&end=",to), headers)
    bars = response_text_clean(bars)
    to <- as.Date(to) - 1
    week_dates <- get_calendar(from,to)$date
  }
  
  
  
  if(timeframe == "1D" | timeframe == "day"){
  
  #week_dates = week_dates[length(week_dates)-starting:length(week_dates)]
  bars = lapply(bars, function(x) transform(x, dates = week_dates))
  return(bars)
  } else return(bars)
}
#----------------------------------------------------------------------------------------------

# PACKAGE FUNCTIONS END #



