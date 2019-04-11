

#Polygon support wrappers
#---------------------------------------------------------------------------------------------------------------------
get_url_poly <- function(live=NULL, polygon = NULL){
  url = "https://api.polygon.io" 
  return(url)
}



response_text_clean <- function(dat){
  
  dat = httr::content(dat, as = "text", encoding = "UTF-8")
  dat = jsonlite::fromJSON(dat)
  return(dat)
}
#---------------------------------------------------------------------------------------------------------------------









#Please see the Polyogn API documentation for details. I will add details on these soon.



#Please make sure that you have set the appropriate live Key ID. 
#When you call 'Sys.getenv("APCA-API-KEY-ID")' the correct Key ID should be your output.



#Polygon API functions
#------------------------------------------------------------------------------

#Get Meta information
get_meta <- function(version="v1",ticker=NULL, endpoint=NULL, perpage=NULL){
  path_url = get_url_poly()
  if(is.null(ticker)){
    stop("Please enter a ticker for the stock that you want.")
  }
  
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
  meta = last_price_details = httr::GET(url = full_path_url)
  meta = response_text_clean(meta)
  return(meta)
}

#Symbol information
#meta$symbol

#All endpoints 
#meta$endpoints

#get_meta(ticker = "AMZN", endpoint = "company")









#Exchange Information
get_poly_exchanges <- function(){
  path_url = get_url_poly()
  full_path_url = paste0(path_url,"/v1/meta/exchanges?apiKey=",Sys.getenv("APCA-LIVE-API-KEY-ID"))
  exchanges = last_price_details = httr::GET(url = full_path_url)
  exchanges = response_text_clean(exchanges)
  return(exchanges)
}

#get_poly_exchanges()









#Historic Trades Information
#date
#offset - Timestamp offset, used for pagination. This is the offset at which to start the results. Using the timestamp of the last result as the offset will give you the next page of results.
#limit - Limit the size of response, Max 50000
#type - Get informaiton on either "trades" or "quotes".
get_historic_info <- function(ticker=NULL,type=NULL,date=NULL){
  if(is.null(ticker) | is.null(type) | is.null(date)){
    stop("Please enter a value for either ticker,type, or date.")
  }
  path_url = get_url_poly()
  full_path_url = paste0(path_url,"/v1/historic/",type,"/",ticker,"/",date,"?apiKey=",Sys.getenv("APCA-LIVE-API-KEY-ID"))
  historic = last_price_details = httr::GET(url = full_path_url)
  historic = response_text_clean(historic)
  return(historic)
}


#get_historic_info(ticker = "AMZN", type = "trades", date = "2019-04-05")









#Last_price
get_poly_last_price <- function(ticker = NULL){
  if(is.null(ticker)){
    stop("Please enter the stocks ticker.")
  }
  path_url = get_url_poly()
  full_path_url = paste0(path_url,"/v1/last/stocks/",ticker,"?apiKey=",Sys.getenv("APCA-LIVE-API-KEY-ID"))
  last_price_details = httr::GET(url = full_path_url)
  last_price_details = response_text_clean(last_price_details)
  return(last_price_details)
}

#get_poly_last_price("AMZN")






#Last_trade
get_poly_last_trade <- function(ticker = NULL){
  if(is.null(ticker)){
    stop("Please enter the stocks ticker.")
  }
  path_url = get_url_poly()
  full_path_url = paste0(path_url,"/v1/last_quote/stocks/",ticker,"?apiKey=",Sys.getenv("APCA-LIVE-API-KEY-ID"))
  last_trade_details = httr::GET(url = full_path_url)
  last_trade_details = response_text_clean(last_trade_details)
  return(last_trade_details)
}


#get_poly_last_trade("AMZN")








#Daily Open / High / Low / Close / After Hours / Volume
get_poly_ohlc <- function(ticker=NULL, date=NULL){
  if(is.null(ticker)){
    stop("Please enter the stocks ticker.")
  } 
  if(is.null(date)){
    date <- Sys.Date()
  }
  path_url = get_url_poly()
  full_path_url = paste0(path_url,"/v1/open-close/",ticker,"/",date,"?apiKey=",Sys.getenv("APCA-LIVE-API-KEY-ID"))
  open_close = httr::GET(url = full_path_url)
  open_close = response_text_clean(open_close)
  return(open_close)
}


#get_poly_ohlc("AMZN")
#get_poly_ohlc("AMZN", date = "2019-03-20")










#V2 Calls

#All Tickers Worldwide Information
get_poly_all_tickers <- function(sort="ticker",type=NULL,market=NULL,locale=NULL,search=NULL,perpage=50,page=1,active=NULL){
  #fix this url
  path_url = get_url_poly()
  full_path_url = paste0(path_url,"/v2/reference/tickers?sort=",sort,"&perpage=",perpage,"&page=",page,"&apiKey=",Sys.getenv("APCA-LIVE-API-KEY-ID"))
  all_tickers = httr::GET(url = full_path_url)
  all_tickers = response_text_clean(all_tickers)
  return(all_tickers)
}

#get_poly_all_tickers()









#Available Market information
get_poly_markets_information <- function(){
  path_url = get_url_poly()
  full_path_url = paste0(path_url,"/v2/reference/markets?apiKey=",Sys.getenv("APCA-LIVE-API-KEY-ID"))
  market_info = httr::GET(url = full_path_url)
  market_info = response_text_clean(market_info)
  return(market_info)
}

#get_poly_markets_information()









#Available Locales information
get_poly_locales <- function(){
  path_url = get_url_poly()
  full_path_url = paste0(path_url,"/v2/reference/locales?apiKey=",Sys.getenv("APCA-LIVE-API-KEY-ID"))
  locales_avail = last_price_details = httr::GET(url = full_path_url)
  locales_avail = response_text_clean(locales_avail)
  return(locales_avail)
}

#get_poly_locales()









#Types Mapping Information
get_poly_types <- function(){
  path_url = get_url_poly()
  full_path_url = paste0(path_url,"/v2/reference/types?apiKey=",Sys.getenv("APCA-LIVE-API-KEY-ID"))
  map = last_price_details = httr::GET(url = full_path_url)
  map = response_text_clean(map)
  return(map)
}

#get_poly_types()









#Stock Split Information
get_poly_stock_splits <- function(ticker=NULL){
  if(is.null(ticker)){
    stop("Please enter the stocks ticker.")
  }
  path_url = get_url_poly()
  full_path_url = paste0(path_url,"/v2/reference/splits/",ticker,"?apiKey=",Sys.getenv("APCA-LIVE-API-KEY-ID"))
  split_info = last_price_details = httr::GET(url = full_path_url)
  split_info = response_text_clean(split_info)
  return(split_info)
}

#get_poly_stock_splits("AMZN")




#All Tickers
get_poly_all_us_tickers <- function(){
  path_url = get_url_poly()
  full_path_url = paste0(path_url,"/v2/snapshot/locale/us/markets/stocks/tickers?apiKey=",Sys.getenv("APCA-LIVE-API-KEY-ID"))
  all_us_tickers = httr::GET(url = full_path_url)
  all_us_tickers = response_text_clean(all_us_tickers)
  return(all_us_tickers)
}

#get_poly_all_us_tickers()









#Snapshot Single Tickers
get_poly_ticker <- function(ticker=NULL){
  path_url = get_url_poly()
  full_path_url = paste0(path_url,"/v2/snapshot/locale/us/markets/stocks/tickers/",ticker,"?apiKey=",Sys.getenv("APCA-LIVE-API-KEY-ID"))
  ticker = httr::GET(url = full_path_url)
  ticker = response_text_clean(ticker)
  return(ticker)
}

#get_poly_ticker("AMZN")









#Snapshot Top 20Gainers 
get_poly_top20 <- function(type=NULL){
  if(is.null(type)){
    stop("Please enter either 'gainers' or 'losers' in the type arguement.")
  }
  path_url = get_url_poly()
  full_path_url = paste0(path_url,"/v2/snapshot/locale/us/markets/stocks/",ifelse(type == "gainers",type,"losers"),"?apiKey=",Sys.getenv("APCA-LIVE-API-KEY-ID"))
  polytop20 = httr::GET(url = full_path_url)
  polytop20 = response_text_clean(polytop20)
  return(polytop20)
}

#get_poly_top20(type = "losers")
#get_poly_top20(type = "gainers")









#Previous Day Close for Ticker
get_poly_prev_dayclose <- function(ticker=NULL){
  if(is.null(ticker)){
    stop("Please enter the stocks ticker.")
  }
  path_url = get_url_poly()
  full_path_url = paste0(path_url,"/v2/aggs/ticker/",ticker,"/prev","?apiKey=",Sys.getenv("APCA-LIVE-API-KEY-ID"))
  prev_close = httr::GET(url = full_path_url)
  prev_close = response_text_clean(prev_close)
  return(prev_close)
}

#get_poly_prev_dayclose("AMZN")









#Get Aggregate Pricing Data
#Multiplier -> size of the timespan multiplier. Default to 1
#Timespan -> size of the time window i.e "minute", "hour", "day", "week", month", "quarter", or "year". Default to day
get_poly_agg_quote <- function(ticker=NULL,multiplier = 1, timespan = "day", from=NULL, to=NULL, unadjusted=FALSE){
  if(is.null(ticker)){
    stop("Please enter a stock ticker.")
  }
  if(is.null(from) | is.null(to)){
    stop("Please enter a date in the 'from' or 'to' arguement.")
  }
  path_url = get_url_poly()
  full_path_url = paste0(path_url,"/v2/aggs/ticker/",ticker,"/range/",multiplier,"/",timespan,"/",from,"/",to,"?unadjusted=",unadjusted,"&apiKey=",Sys.getenv("APCA-LIVE-API-KEY-ID"))
  agg_quote = httr::GET(url = full_path_url)
  agg_quote = response_text_clean(agg_quote)
  return(agg_quote)
}

#get_poly_agg_quote("AMZN", from = "2019-01-01", to = "2019-03-31", timespan = "hour")









#Get Aggregate daily data for overall markets on a selected date. Defaults to todays date
get_poly_markets_agg_daily <- function(locale="US", market="STOCKS",date=Sys.Date(), unadjusted=FALSE){
  path_url = get_url_poly()
  full_path_url = paste0(path_url,"/v2/aggs/grouped/locale/",locale,"/market/",market,"/",date,"?unadjusted=",unadjusted,"&apiKey=",Sys.getenv("APCA-LIVE-API-KEY-ID"))
  markets_daily = httr::GET(url = full_path_url)
  markets_daily = response_text_clean(markets_daily)
  return(markets_daily)
}

#get_poly_markets_agg_daily()










#POLYGON SOCKETS FOR STREAMING
#wscat wss://socket.polygon.io/stocks

#I cannot authenticate myself with Polygon. Maybe Alpaca users do not have access?
#{"action":"auth","params":"*******"}

#Actions
#{"action":"subscribe","params":"T.MSFT"}
#{"action":"unsubscribe","params":"T.MSFT"}