


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
#' Get Polygon URL for Server Request function
#' 
#' Get Polygon's URL for the Server Request that is sent to interact with the API.
#' @return The correct URL for Polygon's API.
#' @export
get_url_poly <- function(){
  url = "https://api.polygon.io" 
  return(url)
}
#----------------------------------------------------------------------------------------------






#----------------------------------------------------------------------------------------------
#' Get Headers for Server Request function
#'
#' @return The correct headers that will be sent in the API request.
#' @param live TRUE / FALSE if you are connecting to a live account. Default to NULL, so it will use the key variables set by the user for their respective paper account. Set live = TRUE to find your live key credentials.
#' @export
get_headers <- function(live=NULL){
  
  if(is.null(live)){
    headers <- httr::add_headers('APCA-API-KEY-ID' = Sys.getenv("APCA-API-KEY-ID"), 
                                       'APCA-API-SECRET-KEY' = Sys.getenv("APCA-API-SECRET-KEY"))
  } 
  else{
    ifelse(live, 
                      headers <- httr::add_headers('APCA-API-KEY-ID' = Sys.getenv("APCA-LIVE-API-KEY-ID"), 
                                                  'APCA-API-SECRET-KEY' = Sys.getenv("APCA-LIVE-API-SECRET-KEY")),
                      headers <- httr::add_headers('APCA-API-KEY-ID' = Sys.getenv("APCA-API-KEY-ID"), 
                                                  'APCA-API-SECRET-KEY' = Sys.getenv("APCA-API-SECRET-KEY")))
  }
  return(headers)
}
#----------------------------------------------------------------------------------------------











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
#' get_account(live = FALSE)
#' Which is similar to:
#' get_account()
#' For access to live accounts, you must submit as live = TRUE
#' get_account(live = TRUE)
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
#' Which is similar to:
#' get_config()
#' For access to live accounts, you must submit as live = TRUE
#' get_account(live = TRUE)
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
get_config(live = TRUE)










#----------------------------------------------------------------------------------------------
#' Send Account Configurations function
#' 
#' 
#' @param live TRUE / FALSE if you are connecting to a live account. Default to FALSE, so it will use the paper url if nothing was provided.
#' @return
#' @examples 
#' #' set_config(live = FALSE)
#' Which is similar to:
#' set_config()
#' For access to live accounts, you must submit as live = TRUE
#' set_account(live = TRUE)
#' @export
set_config <- function(live = FALSE){
  #Set URL & Headers
  url = get_url(live)
  headers = get_headers(live)
}
#----------------------------------------------------------------------------------------------











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
#' get_positions(ticker = "AAPL", live = FALSE)
#' get_positions(ticker = "AAPL")
#' This gets all positions:
#' get_positions()
#' @importFrom magrittr
#' @export
get_positions <- function(ticker = NULL, live = FALSE){
  #Set URL, live = FALSE & Headers
  url = get_url(live)
  headers = get_headers(live)
  
  #Send Request
  positions = httr::GET(url = paste0(url,"/v1/positions"), headers) 
  positions = response_text_clean(positions)
  
  
  #Check if any positions exist before attempting to return
  if(length(positions) == 0) cat("No positions are open at this time.")
  else if(is.null(ticker)){
    positions[,c(5:6,8:ncol(positions))] %<>% map_dfc(as.numeric)
    return(positions)
  } else {
    positions[,c(5:6,8:ncol(positions))] %<>% map_dfc(as.numeric)
    positions <- subset(positions,symbol == ticker)
    return(positions)
  }
}
#----------------------------------------------------------------------------------------------









#----------------------------------------------------------------------------------------------
#' Get Orders function
#' 
#' The orders API allows a user to monitor, place and cancel their orders with Alpaca. Times are returned as yyyy-mm-dd hh-mm-ss POSIXct, quantity and price as numeric, and all others as a string.
#' @param ticker Specify which symbol you want to call by inserting ticker as a string.
#' @param status Order status to be queried "open, closed or all". Defaults to open as a string.
#' @param from The response will include only orders submitted after this date exclusive as a timestamp object.
#' @param silent A logical TRUE / FALSE on if you want the "no orders to cancel" message to print to the console. Default to FALSE.
#' @param live TRUE / FALSE if you are connecting to a live account. Default to FALSE, so it will use the paper url if nothing was provided.
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
#' get_orders(live = FALSE)
#' get_orders(status = "all")
#' For a specific ticker:
#' get_orders(ticker = "AAPL", status = "all")
#' @importFrom dplyr stringr lubridate
#' @export
get_orders <- function(ticker = NULL, status = "open", from = NULL, silent = FALSE, live = FALSE){
  #Set URL & Headers
  url = get_url(live)
  headers = get_headers(live)
  
  #Send Request according to the selected arguments.
  
  
  if(!is.null(ticker)){       #If the ticker is not null, then return the orders for the tickers that is specified.
    if(!is.null(from)){     #If the from date is given, then request orders from only that date and on, or else get all orders for that ticker.
      orders = httr::GET(url = paste0(url,"/v1/orders?status=",status,"&after=",from,"T09:30:00-04:00"), headers)
      orders = response_text_clean(orders)
      if(length(orders) != 0) orders = dplyr::filter(orders, symbol %in% ticker)
    } else {
      orders = httr::GET(url = paste0(url,"/v1/orders?status=",status), headers)
      orders = response_text_clean(orders)
      if(length(orders) != 0) orders = dplyr::filter(orders, symbol %in% ticker)
    }
    
    
    
  }else if(is.null(ticker)){  #If the ticker is null, then return all orders.
    if(!is.null(from)){     #If the from date is given, then request orders from only that date and on, or else return all orders.
      orders = httr::GET(url = paste0(url,"/v1/orders?status=",status,"&after=",from,"T09:30:00-04:00"), headers)
      orders = response_text_clean(orders)
    } else{
      orders = httr::GET(url = paste0(url,"/v1/orders?status=",status), headers)
      orders = response_text_clean(orders)
    }
  }
  
  #Make sure there are orders to return before calling return. Format orders to workable and readable format before returning
  if(length(orders) == 0){
    if(silent == FALSE) cat(paste("No",status,"orders",if(!is.null(ticker))paste("for",ticker),"at this time.",'Set status = "all" to see all orders.'))
  }  else {
    toNum <- function(x){
      as.numeric(stringr::str_replace_all(x, "\\$|\\,", ""))
    }
    orders <- dplyr::mutate_at(orders, dplyr::vars(dplyr::ends_with("at")),list(~lubridate::ymd_hms(., tz = Sys.timezone())))
    orders <- dplyr::mutate_at(orders, dplyr::vars(qty, filled_qty, filled_avg_price, limit_price, stop_price), list(toNum))
    return(orders)
    }
}
#----------------------------------------------------------------------------------------------









#----------------------------------------------------------------------------------------------
#' Submit Order function
#' 
#' Places a new order of the specified stock, quantity, buy / sell, type of order, time in force, and limit / stop prices if selected.
#' @param ticker The stock's symbol.
#' @param qty The amount of shares to trade.
#' @param side The side of the trade. I.E "buy" or "sell"
#' @param type The type of trade order. I.E "market","limit","stop","stoplimit", etc.
#' @param time_in_force The type of time order. I.E "day", "gtc", "opg". Default is "day".
#' @param limit_price If order type was a limit, then enter the limit price here.
#' @param stop_price If order tyope was a stop, then enter the stop price here.
#' @param live TRUE / FALSE if you are connecting to a live account. Default to FALSE, so it will use the paper url if nothing was provided.
#' @examples 
#' For market order:
#' submit_order(ticker = "AAPL", qty = 100, side = "buy", type = "market")
#' Or you can submit a limit order:
#' submit_order(ticker = "AAPL", qty = 100, side = "buy", type = "limit", limit_price = 120)
#' @export
submit_order <- function(ticker, qty, side, type, time_in_force = "day", limit_price = NULL, stop_price = NULL, live = FALSE){
  #Set URL & Headers
  url = get_url(live)
  headers = get_headers(live)
  
  
  #Convert ticker argument to upper if lower
  if(grepl("^[[:lower:]]+$",ticker)){ticker <- toupper(ticker)}
  
  #Create body with order details, most common is a named list 
  bodyl <- list(symbol=ticker, qty=qty, side = side, type = type, time_in_force = time_in_force, limit_price = limit_price, stop_price = stop_price)
  bodyl <- lapply(bodyl, as.character)
  
  #Send Request
  orders = httr::POST(url = paste0(url,"/v1/orders"), body = bodyl, encode = "json",headers)
  orders = response_text_clean(orders)
  return(orders)
}
#----------------------------------------------------------------------------------------------







#----------------------------------------------------------------------------------------------
#' Cancel Order function
#' 
#' Cancels any open order by either ticker or order id. If multiple open orders exist for one ticker, then the default is to cancel the most recent order.
#' @param ticker_id The ticker symbol or the order id.
#' @param live TRUE / FALSE if you are connecting to a live account. Default to FALSE, so it will use the paper url if nothing was provided.
#' @examples 
#' cancel_order(ticker_id = "AAPL")
#' cancel_order(ticker_id = "aapl")
#' Or you can instead cancel by the order_id:
#' orders <- get_orders(status="open", silent = TRUE)
#' cancel_order(ticker_id = orders$id[1])
#' @importFrom lubridate with_tz
#' @export
cancel_order <- function(ticker_id,live = FALSE){
  #Set URL & Headers
  url = get_url(live)
  headers = get_headers(live)
  
  
  #Gather the open order ID for the symbol specified
  open_orders = get_orders(status = "open", live = live, silent = TRUE)
  
  
  #Check if any open orders before proceeding. 
  if(is.null(open_orders)){
    cat("There are no orders to cancel at this time.")
    
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
  cancel = httr::DELETE(url = paste0(url,"/v1/orders/",order_id), headers)
  cat(paste("Order ID", order_id,"for",ticker, "was successfully canceled."))
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
  
  
  
  #Send Request and ticker if one was supplied. 
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
  
  
  #If dates were given, make sure they are in a string format. I add plus 1 to the "to" date, or it will always return one day before the day "to"?
  if(!is.null(to)) to <- as.character(as.Date(to)+1)
  if(!is.null(from)) from <- as.character(as.Date(from))
  
  
  
  if(is.null(from) & is.null(to)){  #Check if any dates were given, and if not then return 
    calendar = httr::GET(url = paste0(url,"/v1/calendar"), headers)
    calendar =  response_text_clean(calendar)
  } else{ 
    calendar = httr::GET(url = paste0(url,"/v1/calendar","?start=",from,"&end=",to), headers)
    calendar =  response_text_clean(calendar)
  }
  calendar <- dplyr::mutate_at(calendar, dplyr::vars("date"), ~ lubridate::ymd(.))
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
#' Getting one or more tickers: 
#' get_bars(ticker = c("INTC","MSFT"))
#' @examples 
#' Getting price data with specific date ranges and timeframes, by also limiting the amount of bars returned for each ticker.
#' get_bars(ticker = c("INTC","MSFT"), from = "2019-03-20", to = "2019-04-01", timeframe = "15Min", limit = 175)
#' @importFrom lubridate dplyr stringr magrittr
#' @export
get_bars <- function(ticker, from = Sys.Date()-6, to = Sys.Date(), timeframe = "1D", limit = NULL){
  
  #Set Url & Headers
  url = "https://data.alpaca.markets" #Pricing data uses unique URL, see market data API documentation to learn more.
  headers = get_headers()
  
  if(!is.null(limit)){ #If a limit value was entered then;
    #Ensure the limit is set to 1000 or under.
    if(limit > 1000){
      stop("Max limit is 1000!")
    }
  }
  
  
  # Timeframe handler - adding a little more flexibility to what the argument can take
  if (stringr::str_detect(timeframe, stringr::regex("D|d|days?", ignore_case = T))) {
    timeframe <- "1D"
  } else if (stringr::str_detect(timeframe, stringr::regex("M|mins?|minutes?"))) {
    digit <- ifelse(!is.na(stringr::str_extract(timeframe, "^\\d+")), stringr::str_extract(timeframe, "^\\d+"), "1")
    timeframe <- paste0(digit,"Min")
  }
  
  
  #Check for multiple tickers or just one
  ticker = ifelse(length(ticker) > 1, paste0(trimws(ticker), collapse = ","), ticker)
  
  
  
  #Get the trading days in between the sequence
  week_dates = get_calendar(from,to)$date
  
  
  
  #Since the max bars requested limit is at 1000, we need to bring the 1000 most recent price dates to match.
  if(length(week_dates) > 1000){
    start <- length(week_dates) - 999
    week_dates <- week_dates[start:length(week_dates)]
  }
  
  
  
  #If limit is null then set it according to timeframe.
  if((timeframe == "1D" | timeframe == "day") & is.null(limit)){
    limit = length(week_dates)
  } else if(timeframe == "15Min" & is.null(limit)){
    limit = 250
  } else if(timeframe == "5Min" & is.null(limit)){
    limit = 500
  } else if((timeframe == "1Min" | timeframe == "minute") & is.null(limit)){
    limit = 1000
  }
  
  
  
  #Get date/time with yyyy-mm-dd HH:MM:SS for timeframes < 1day
  if(!(timeframe == "1D" | timeframe == "day") & (lubridate::is.POSIXct(from) | lubridate::is.POSIXct(to))){
    from <- strftime(from, "%Y-%m-%dT%H:%M:%S%z", tz = Sys.timezone())
    to <- strftime(to, "%Y-%m-%dT%H:%M:%S%z", tz = Sys.timezone())
  } else if(!(timeframe == "1D" | timeframe == "day")){
    from = paste0(from,stringr::str_extract(format(Sys.time(), "%Y-%m-%dT%H:%M:%OS%z"), "T.*"))
    to = paste0(to,stringr::str_extract(format(Sys.time(), "%Y-%m-%dT%H:%M:%OS%z"), "T.*"))
  } else {
    from = paste0(from,"T20:30:00-04:00")
    to = paste0(to,"T20:30:00-04:00")
  }
  
  
  
  #Send Request                                                                                 
  bars = httr::GET(url = paste0(url,"/v1/bars/",timeframe,"?symbols=",ticker,"&limit=",limit,"&start=",from,"&end=",to), headers)
  bars = response_text_clean(bars)
  
  #Rename columns to quantmod standard and reformat time column
  bars = lapply(bars, function(l){
    nms <- c(time = "t", open = "o", high = "h", low = "l", close = "c", volume = "v")
    out <- dplyr::mutate_at(l, dplyr::vars("t"), ~as.POSIXct(.,origin = "1970-01-01")) %>% dplyr::mutate_at(dplyr::vars(o,h,c,l,v),~as.numeric(.)) %>%
      dplyr::rename((!!nms))
  })
  return(bars)
}
#----------------------------------------------------------------------------------------------











#Select Polygon Integration functions - Live Brokerage accounts are the only accounts with access to Polygon.

#----------------------------------------------------------------------------------------------


#Best function to use if you want to see a little more than just pricing data for you company. 
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
#' Getting default meta for AMZN: 
#' get_meta(ticker = "AMZN")
#' Getting news information on AMZN: 
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










#----------------------------------------------------------------------------------------------
#' Get Polygon Aggregate Pricing Data
#' 
#' This function provides aggregate pricing data from Polygon. Polygon provides consolidated market data. Consolidated stock market data is an aggregated reporting of all securities exchanges’ and alternative trading venues’ quote and trade data. It is the most relied upon type of market data, providing investors and traders globally with a unified view of U.S. stock market prices and volumes. It also underpins the National Best Bid and Offer (NBBO), which provides investors with a continuous view of the best available displayed buy and sell prices, and through Rule 611 ensures that investors receive the best available displayed prices on their trades, with a few exceptions.
#' @param ticker Specify which symbol you want to call by inserting ticker as a string.
#' @param multiplier Size of the timespan multiplier. Default to 1.
#' @param timespan Size of the time window i.e "minute", "hour", "day", "week", month", "quarter", or "year". Default to day.
#' @param from The starting date for the pricing data. 
#' @param to The ending date for the pricing data. 
#' @param unadjusted Set to true if the results should NOT be adjusted for splits.
#' @return A list object containing all information the API responds with. 
#' @examples
#' Getting default pricing data on AMZN (daily): 
#' get_poly_agg_quote(ticker = "AMZN",from = "2019-04-01", to = "2019-04-12")
#' Getting minute pricing data on AMZN: 
#' get_poly_agg_quote("AMZN", from = "2019-04-11", to = "2019-04-12", timespan = "minute")
#' Getting quarterly pricing data on AMZN: 
#' get_poly_agg_quote("AMZN", from = "2018-01-01", to = "2019-04-12", timespan = "quarter")
#' Getting yearly pricing data on AMZN: 
#' get_poly_agg_quote("AMZN", from = "2015-01-01", to = "2019-12-31", timespan = "year")
#' @export
get_poly_agg_quote <- function(ticker=NULL,multiplier = 1, timespan = "day", from=NULL, to=NULL, unadjusted=FALSE){
  if(is.null(ticker)){
    stop("Please enter a stock ticker.")
  }
  if(is.null(from) | is.null(to)){
    stop("Please enter a date in the 'from' or 'to' argument.")
  }
  
  #Set URL
  path_url = get_url_poly()
  full_path_url = paste0(path_url,"/v2/aggs/ticker/",ticker,"/range/",multiplier,"/",timespan,"/",from,"/",to,"?unadjusted=",unadjusted,"&apiKey=",Sys.getenv("APCA-LIVE-API-KEY-ID"))
  
  #Send Request
  agg_quote = httr::GET(url = full_path_url)
  agg_quote = response_text_clean(agg_quote)
  
  #Create a column for date/time
  agg_quote$results$t = as.POSIXct(agg_quote$results$t/1000, origin = "1970-01-01")
  return(agg_quote)
}
#----------------------------------------------------------------------------------------------









#----------------------------------------------------------------------------------------------
#' Get Polygon Stock Split Information
#' 
#' This function provides stock split data for the specified ticker from Polygon.
#' @param ticker Specify which symbol you want to call by inserting ticker as a string.
#' @return A list object containing all information the API responds with. 
#' @examples
#' Getting default pricing data on AMZN (daily): 
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
#' Getting historic trade data on AMZN: 
#' get_historic_info(ticker = "AMZN", type = "trades", date = "2019-04-05")
#' Getting historic pricing data on AMZN: 
#' get_historic_info(ticker = "AMZN", type = "quotes", date = "2019-04-05")
#' @export
get_historic_info <- function(ticker=NULL,type=NULL,date=NULL){
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
#' Getting the last listed price for AMZN: 
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
#' Getting the last listed trade for AMZN: 
#' get_poly_last_trade("AMZN")
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
#' Getting the last listed trade for AMZN: 
#' get_poly_ohlc("AMZN", date = "2019-03-20")
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
#' Getting the last listed trade for AMZN: 
#' get_poly_prev_dayclose("AMZN")
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