# AlpacaforR 🦙𝘙
Connecting to [Alpacas](https://alpaca.markets) API and navigating it using R.

## Installation

**AlpacaforR** 🦙𝘙 isn't yet available on CRAN, but you may install the development versions of the package from Github.

Install the [devtools](https://cran.r-project.org/web/packages/devtools/readme/README.html) package if you have yet to do so, and then load it in:

```r
install.packages("devtools")
library(devtools)
```

Now, install **AlpacaforR** 🦙𝘙 using devtools `install_github` and then load it in:

```r
install_github("jagg19/AlpacaforR")
library(AlpacaforR)
```




## User Keys & URL 


#### KEY-ID and SECRET-KEY

You must set your KEY-ID 🔑 and SECRET-KEY 🗝 as specifically named environment variables. They *must* be named as the following:
```r
Sys.setenv('APCA-API-KEY-ID' = VALUE)
Sys.setenv('APCA-API-SECRET-KEY' = VALUE)
```

You can test that it has been properly set by calling:
```r
Sys.getenv('APCA-API-KEY-ID')
Sys.getenv('APCA-API-SECRET-KEY')
```

The output should be the key values you've entered. Once you've set these to your environment, you will be able to use any of the **AlpacaforR** 🦙𝘙 functions. 

<br>

#### Live or Paper URL?
The user does not have to specify the URL, as the functions handle that on its own. The user only needs to specify whether or not they are interacting with a live account. E.g:

```r
#If paper account; user does not need to input anything since live = FALSE is the default.
get_account()

#If live account; user needs to set live = TRUE
get_account(live = TRUE)
```

Not all functions require this since some functions use the same URL regardless of the account type. These functions are `get_assets` 💰, `get_calendar` 🗓, `get_clock` ⏰, and `get_bars` 📊 since the same URL is used for each user.

<br>

## Getting your Account
This is made extremely easy through the `get_account` function, which will return account details such as account id 🆔, portfolio value 💲 , buying power 🔌, cash 💵, cash withdrawable 💸, etc. 

<br>

> 🛑 You *MUST* have your user keys set as the appropietley named environment variables shown above!

<br>

```r
#If paper account: 
get_account()

#If live account:
get_account(live = TRUE)
```

<br>

## Getting your current positions
You can get all your current positions or only the positions specified by ticker by calling `get_positions()`

```r
#If paper account:
get_positions()

#By specific tickers:
get_positions(ticker = c("AAPL","AMZN"))

If live account:
get_positions(ticker = c("AAPL","AMZN"), live = TRUE)
```

<br>

## Managing Orders
Getting, submiting, and cancelling 🚫 orders are also made extremeley easy through `get_orders()`,`submit_order()`,`cancel_order()` but require some specific arguements. See `?` for more details.

<br>

#### Getting orders
To get orders, use `get_orders()` and set the status to your desired option. Status options are "open", "closed", and "all". Default status is set to "open".

```r
#If paper account:
get_orders(status = "all") 

#If live account:
get_orders(status = "all", live = TRUE)
```

<br>

#### Submitting orders
To submit orders, use `submit_order()` with the appropiate arguements and fire away 🚀. These arguements include ticker, qty, side, type, time_in_force, limit_price, stop price. The required arguements are ticker ("AAPL") 🍎, the share qty ("50"), side of trade ("buy" or "sell"), and type of order ("market" or "limit" or "stop" or "stoplimit"). 

The options for time_in_force are ("day" or "gtc" or "opg") but the default is set to "day". If you select "limit" or "stop" as your order type, then you must provide the limit_price or stop_price as inputs as well. Please see [Alpacas Order](https://docs.alpaca.markets/orders/) page to learn more about types of orders and time_in_force options. 

```r
#If paper account:

#A market order
submit_order(ticker = "AAPL", qty = "100", side = "buy", type = "market")

#A market order with "gtc" time_in_force
submit_order(ticker = "AAPL", qty = "100", side = "buy", type = "market", time_in_force = "gtc")

#A limit order
submit_order(ticker = "AAPL", qty = "100", side = "buy", type = "limit", limit_price = "100")


#If live account:
#A market order
submit_order(ticker = "AAPL", qty = "100", side = "buy", type = "market", live = TRUE)

#A market order with "gtc" time_in_force
submit_order(ticker = "AAPL", qty = "100", side = "buy", type = "market", time_in_force = "gtc", live = TRUE)

#A limit order
submit_order(ticker = "AAPL", qty = "100", side = "buy", type = "limit", limit_price = "100", live = TRUE)
```

<br>

#### Cancelling Orders
You can cancel 🚫 any open order using `cancel_order()` by either specifying the ticker or order_id. order_id is one of the many columns when using `get_orders()`, or you just enter the ticker for the order that you want cancelled. The function will search 🕵 for and cancel the most recent open order for the ticker specified.

```r
#If paper account:
#Cancelling by ticker
cancel_order(ticker = "AAPL")

#Cancelling by order_id
cancel_order(order_id = "1n0925a7-aq52-480d-t68f-01d5970182ae")

#If live account:
#Cancelling by ticker
cancel_order(ticker = "AAPL", live = TRUE)

#Cancelling by order_id
cancel_order(order_id = "1n0925a7-aq52-480d-t68f-01d5970182ae", live = TRUE)
```

<br>

## Getting all assets available or specific assets
To get all assets 💰 available or just a specific asset 🍎, we can use `get_assets()` and provide a stocks symbol to the ticker arguement for a specific asset. We do not need to specify account type with this function. See `?` for more details.

```r
#Return ALL assets available on Alpaca
get_assets()

#Return a specific asset
get_assets(ticker = "AAPL")
```

<br>

## Get pricing data from alpacas API
We can use the `get_bars()` function to get pricing data 📈 in OHLCV bar format 📊 for one or multiple tickers. You do not need to specify the account type for this function. The only input needed is the ticker(s) value, and it will return a list 📝 containing pricing data for the last 5 trading days of each ticker. You can easily change the date range as well as the timeframe of the OHLCV bars with the "from", "to", and "timeframe" arguements. 




The options for the timeframe arguement include "minute", "1Min", "5Min", "15Min", "day" or "1D" and has a default value of "1D". The bar limit arguement can range from 1 to 1000 and has various default values according to the timeframe chosen. If timeframe "1D or day" then the limit is set to the # of days. If "15Min" the default is 250, if "5Min" the default is 500, and if "1Min or minute" then the default is the max, 1000. If the date range includes more than 1000 bars, then it will return the 1000 most recent bars. Dates are returned as a column if a daily timeframe is set. See `?` for more details.

```r
#Getting daily pricing data for multiple tickers, and returning the default timeframe (last 5 trading days).
tickers <- c("AAPL","AMZN")
get_bars(ticker = tickers)

#Getting daily pricing data since the start of 2019
get_bars(ticker = tickers, from = "2019-01-01")

#Getting 15Min bar pricing data for the last 5 trading days. Default bar limit is set to the value of 250.
get_bars(ticker = tickers, timeframe= "15Min")

#Getting 1Min pricing data for the last 5 trading days. Default bar limit is set to the max value of 1000.
get_bars(ticker = tickers, timeframe= "1Min")
```

<br>

## Getting open market days and market clock data
One of my favorite requests to make while using the [Alpaca](https://alpaca.markets) API is the calendar 🗓 and clock ⏰ requests. Using the `get_calendar()` and `get_clock()` functions in this package, we can get all the dates and hours from the start of **1970** to the end of **2029** during which the stock market is open while accounting for market holiday's. It is as simple as:

```r
#Getting all dates from 1970 to 2029
get_calendar()

#Getting specific dates using date ranges
get_calendar(from = "2000-01-01", to = "2020-01-01")


#Get market clock and see if the market is currently open as well as the times of the next open and close
get_clock()
```



## Start trading in R!
You're all set! 🥳 Now your ready to begin using **AlpacaforR** 🦙𝘙 functions to send and receive [Alpaca](https://alpaca.markets) API requests using R! See the help calls for details on each function. E.g `?get_account` 