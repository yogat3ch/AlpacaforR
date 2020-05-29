# `AlpacaforR` 🦙𝘙
This brief tutorial covers connecting `AlpacaforR` to the [Alpaca API](https://alpaca.markets) and navigating the package. The [Alpaca API Docs](https://alpaca.markets/docs/api-documentation/api-v2/) provides a more general overview of the authentication, API limits, an explanation of paper & live trading, and release notes. It's worth checking out. If you have never heard of Alpaca, you can learn more [here!](https://alpaca.markets/docs/about-us/) If you want to reference this material later from within R, you can do so with `vignette("AlpacaforR", "Getting Started")`

### Release notes
#### 1.0.0 TBD  CRAN Release!
#### 0.9.0 2020-05-24 Package Overhaul
 - This release includes an overhaul of the entire package that is **not backwards compatible** in some instances, that emphasizes the following:
      - Creates an experience of the package functionality that mirrors that of the Alpaca API documentation and provides for more intuitive navigation of the package for new users.
      - Extensive package documentation that links directly to the appropriate online documentation wherever necessary with attention paid to consistency between the two. Documentation inheritance, families, aliases and see also have been implemented. 
      - Robust error pre-empting and catching such that meaningful info is provided when the function encounters a user error. 
      - Better detection intention based on the combinations of arguments entered rather than having to remember multiple functions or specify each parameter explicitly. Smoother fuzzy detection and autocomplete from RStudio with new consistent function names.
      
#### 0.3.0 2020-03-28 Websockets
- Add support for Alpaca Websockets

## Installing `AlpacaforR` 🦙𝘙

`AlpacaforR` 🦙𝘙 is available on CRAN and can be installed with `install.packages("AlpacaforR")`. The development version of the package can be installed from [Github](https:://github.com/yogat3ch/AlpacaforR).

To install the development version, [devtools](https://cran.r-project.org/web/packages/devtools/readme/README.html) is required:

```r
if (!require("devtools")) install.packages("devtools")
```

The `AlpacaforR` 🦙𝘙 dev version can be installed using  `devtools::install_github` with:

```r
if (!require("AlpacaforR")) {
  devtools::install_github("yogat3ch/AlpacaforR")
}
library(AlpacaforR)
```


## User Keys & URL 

### KEY-ID and SECRET-KEY

Connecting to the Alpaca API requires a KEY-ID 🔑 and SECRET-KEY 🗝  as specifically named environment variables for both live and paper accounts.
These values can be found on the respective Alpaca dashboards. Hit "Regenerate Key" if the secret key is no longer visible.

The simplest way to set these values for this and future R sessions is to use `AlpacaforR::firstrun` to add these to your `.Renviron` file and set them for this session. If you have not yet created an `.Renviron` file - it will be created in your R root folder. This folder is found by calling `path.expand("~")`. 
The current root folder of this R instance is: <code>C:/Users/Administrator/Documents</code>, the `.Renviron` file will reside there if it doesn't already. 

`firstrun` takes two arguments: `paper` and `live`, both of which will be named vectors with key & secret for paper and live accounts respectively.


```r
firstrun(
  paper = c(key = "Your-paper-key", secret = "Your-paper-secret"),
  live = c(key = "Your-live-key", secret = "Your-live-secret")
)
```
If using RStudio, these parameters can be added to the `.Renviron` file another way by typing `usethis::edit_r_environ()` at the console. The keys are added as `name = key` pairs like so:

```
APCA-PAPER-API-KEY-ID = "PAPER-KEY"
APCA-PAPER-API-SECRET-KEY = "PAPER-SECRET"
APCA-LIVE-API-KEY-ID = "LIVE-KEY"
APCA-LIVE-API-SECRET-KEY = "LIVE-SECRET"
```
The following guide details how to [set environment variables permanently](https://stackoverflow.com/questions/49738564/r-set-environment-variable-permanently) if you prefer to do this manually via your file system with a text editor.

Test that these have been properly set by calling:
```r
Sys.getenv('APCA-PAPER-API-KEY-ID')
Sys.getenv('APCA-PAPER-API-SECRET-KEY')
Sys.getenv('APCA-LIVE-API-KEY-ID')
Sys.getenv('APCA-LIVE-API-SECRET-KEY')
```

The output should be the key/secret values entered.
The keys can also be set manually for the session using `Sys.setenv`:
```r
Sys.setenv('APCA-PAPER-API-KEY-ID' = "...")
Sys.setenv('APCA-PAPER-API-SECRET-KEY' = "...")
Sys.setenv('APCA-LIVE-API-KEY-ID' = "...")
Sys.setenv('APCA-LIVE-API-SECRET-KEY' = "...")
```

If `usethis` is not installed, it can be installed from CRAN, or the .REnviron file can be edited manually with the guide linked above.
<br>


Once these environmental variables are set, all `AlpacaforR` 🦙𝘙 functions will work correctly.

> 🛑 User keys & secrets *MUST* be set as the appropriately named environment variables shown above for all demos hereforward to work!
 
### Live or Paper URL?
[Account Plans](https://alpaca.markets/docs/trading-on-alpaca/account-plans/) documents the key differences between the account types. When using `AlpacaforR`, interaction with the live or paper account is indicated by setting the `live = TRUE/FALSE` argument. The argument is set to `FALSE` by default. E.g:



```r
#For a paper account; live = FALSE is the default.
# subset is unnecessary, it is added so as not to expose the developers  account details
account()[-c(1:2)]
#> $status
#> [1] "ACTIVE"
#> 
#> $currency
#> [1] "USD"
#> 
#> $buying_power
#> [1] 387837.5
#> 
#> $regt_buying_power
#> [1] 193871
#> 
#> $daytrading_buying_power
#> [1] 387837.5
#> 
#> $cash
#> [1] 96935.5
#> 
#> $portfolio_value
#> [1] 96935.5
#> 
#> $pattern_day_trader
#> [1] FALSE
#> 
#> $trading_blocked
#> [1] FALSE
#> 
#> $transfers_blocked
#> [1] FALSE
#> 
#> $account_blocked
#> [1] FALSE
#> 
#> $created_at
#> [1] "2019-06-26 20:31:20 EDT"
#> 
#> $trade_suspended_by_user
#> [1] FALSE
#> 
#> $multiplier
#> [1] 4
#> 
#> $shorting_enabled
#> [1] TRUE
#> 
#> $equity
#> [1] 96935.5
#> 
#> $last_equity
#> [1] 96959.38
#> 
#> $long_market_value
#> [1] 0
#> 
#> $short_market_value
#> [1] 0
#> 
#> $initial_margin
#> [1] 0
#> 
#> $maintenance_margin
#> [1] 0
#> 
#> $last_maintenance_margin
#> [1] 0
#> 
#> $sma
#> [1] 0
#> 
#> $daytrade_count
#> [1] 22
```



```r
#If live account; set live = TRUE
account(live = TRUE) [-c(1:2)]
```

Not all functions require this since some functions use the same URL regardless of the account type. These functions are `assets` 💰, `calendar` 🗓, `clock` ⏰, and `market_data` 📊 where the same URLs are used for both account types.

<br>



# Package Functionality
The functionality in the `AlpacaforR` package maps neatly onto the endpoints listed in the [API version 2 Documentation](https://alpaca.markets/docs/api-documentation/api-v2/) for ease of reference. For any function hereforward, you can use `?function_name` at the console to view the function's documentation which will provide a great deal more depth of detail regarding it's arguments and what the function returns.

## Account: Retrieve & change info about your account

### `account`
Accessing account information is made easy through the `account` function, which will return account details such as account id 🆔, portfolio value 💲 , buying power 🔌, cash 💵, cash withdrawable 💸, etc. See `?account` for more details or visit the [Account Endpoint Docs](https://alpaca.markets/docs/api-documentation/api-v2/account/) to learn everything there is to know about the requests and responses for this API. 


```r
#shorthand for live account
account(T)
```

### `account_config`
The [Account Configuration Endpoint](https://alpaca.markets/docs/api-documentation/api-v2/account-configuration/) supports viewing and setting account configuration details. 


```r
# get the account configurations
account_config()
#> $dtbp_check
#> [1] "entry"
#> 
#> $no_shorting
#> [1] FALSE
#> 
#> $pdt_check
#> [1] "entry"
#> 
#> $suspend_trade
#> [1] FALSE
#> 
#> $trade_confirm_email
#> [1] "all"
account_config(live = T)
#> $dtbp_check
#> [1] "entry"
#> 
#> $no_shorting
#> [1] FALSE
#> 
#> $pdt_check
#> [1] "entry"
#> 
#> $suspend_trade
#> [1] FALSE
#> 
#> $trade_confirm_email
#> [1] "all"
```

Change configuration settings as needed.

```r
# change a configuration: block all orders on the live account
account_config(suspend_trade = T, live = T)
#> $dtbp_check
#> [1] "entry"
#> 
#> $no_shorting
#> [1] FALSE
#> 
#> $pdt_check
#> [1] "entry"
#> 
#> $suspend_trade
#> [1] TRUE
#> 
#> $trade_confirm_email
#> [1] "all"
```

Set them back to default with ease.

```r
# and then reset to defaults
account_config("default", live = T)
#> $dtbp_check
#> [1] "entry"
#> 
#> $no_shorting
#> [1] FALSE
#> 
#> $pdt_check
#> [1] "entry"
#> 
#> $suspend_trade
#> [1] FALSE
#> 
#> $trade_confirm_email
#> [1] "all"
```

### `account_activities`
`account_activities` allows you to access the [Account Activities Endpoint](https://alpaca.markets/docs/api-documentation/api-v2/account-activities/) to view all account activities, optionally filtered by type and date range. This endpoint supports paging - advance pages by providing the last ID supplied for a given page to `page_token`.

```r
# retrieve page 1 of account activities
(aa <- account_activities())
#> # A tibble: 50 x 11
#>    id    activity_type transaction_time    type  price   qty side  symbol
#>    <chr> <chr>         <dttm>              <chr> <dbl> <dbl> <chr> <chr> 
#>  1 2020~ FILL          2020-05-29 11:33:38 fill  2411.     1 sell  AMZN  
#>  2 2020~ FILL          2020-05-29 11:33:37 fill   124.     1 buy   BYND  
#>  3 2020~ FILL          2020-05-29 11:33:37 fill  2412.     1 buy   AMZN  
#>  4 2020~ FILL          2020-05-29 11:33:36 fill  2411.     6 sell  AMZN  
#>  5 2020~ FILL          2020-05-29 11:33:36 fill   124.     2 sell  BYND  
#>  6 2020~ FILL          2020-05-29 11:33:03 fill   124.     2 buy   BYND  
#>  7 2020~ FILL          2020-05-29 11:33:02 fill  2411.     2 buy   AMZN  
#>  8 2020~ FILL          2020-05-29 11:33:01 fill  2411.     2 buy   AMZN  
#>  9 2020~ FILL          2020-05-29 11:32:58 fill  2411.     2 buy   AMZN  
#> 10 2020~ FILL          2020-05-29 11:29:54 fill  2411.     2 sell  AMZN  
#> # ... with 40 more rows, and 3 more variables: leaves_qty <dbl>,
#> #   order_id <chr>, cum_qty <dbl>
# retrieve page 2
account_activities(page_token = aa$id[50])
#> # A tibble: 50 x 11
#>    id    activity_type transaction_time    type  price   qty side  symbol
#>    <chr> <chr>         <dttm>              <chr> <dbl> <dbl> <chr> <chr> 
#>  1 2020~ FILL          2020-05-26 15:33:53 fill   133.     2 sell  BYND  
#>  2 2020~ FILL          2020-05-26 15:33:49 fill   133.     2 buy   BYND  
#>  3 2020~ FILL          2020-05-26 15:32:32 fill   133.     2 sell  BYND  
#>  4 2020~ FILL          2020-05-26 15:32:26 fill   133.     2 buy   BYND  
#>  5 2020~ FILL          2020-05-26 15:28:09 fill   133.     2 sell  BYND  
#>  6 2020~ FILL          2020-05-26 15:28:04 fill   133.     2 buy   BYND  
#>  7 2020~ FILL          2020-05-26 15:23:10 fill   133.    10 sell  BYND  
#>  8 2020~ FILL          2020-05-26 15:23:05 fill   133.     1 buy   BYND  
#>  9 2020~ FILL          2020-05-26 15:23:05 part~  133.     1 buy   BYND  
#> 10 2020~ FILL          2020-05-26 15:22:09 fill   133.     2 buy   BYND  
#> # ... with 40 more rows, and 3 more variables: leaves_qty <dbl>,
#> #   order_id <chr>, cum_qty <dbl>
```

Optionally provide a filter - see `?account_activities` or click the link above to see what all types of account activities there are.

```r
account_activities("fill")
#> # A tibble: 50 x 11
#>    id    activity_type transaction_time    type  price   qty side  symbol
#>    <chr> <chr>         <dttm>              <chr> <dbl> <dbl> <chr> <chr> 
#>  1 2020~ FILL          2020-05-29 11:33:38 fill  2411.     1 sell  AMZN  
#>  2 2020~ FILL          2020-05-29 11:33:37 fill   124.     1 buy   BYND  
#>  3 2020~ FILL          2020-05-29 11:33:37 fill  2412.     1 buy   AMZN  
#>  4 2020~ FILL          2020-05-29 11:33:36 fill  2411.     6 sell  AMZN  
#>  5 2020~ FILL          2020-05-29 11:33:36 fill   124.     2 sell  BYND  
#>  6 2020~ FILL          2020-05-29 11:33:03 fill   124.     2 buy   BYND  
#>  7 2020~ FILL          2020-05-29 11:33:02 fill  2411.     2 buy   AMZN  
#>  8 2020~ FILL          2020-05-29 11:33:01 fill  2411.     2 buy   AMZN  
#>  9 2020~ FILL          2020-05-29 11:32:58 fill  2411.     2 buy   AMZN  
#> 10 2020~ FILL          2020-05-29 11:29:54 fill  2411.     2 sell  AMZN  
#> # ... with 40 more rows, and 3 more variables: leaves_qty <dbl>,
#> #   order_id <chr>, cum_qty <dbl>
```

### `account_portfolio`
`account_portfolio` accesses the [Portfolio History Endpoint](https://alpaca.markets/docs/api-documentation/api-v2/portfolio-history/) and returns the timeseries data for equity and profit loss information of the account for a period of time aggregated by a specified timeframe (*optional*) or up to a specific end date (*optional*). 

To take a look at equity & gain/loss for the paper account over the past two weeks: 


```r
account_portfolio("2w")
#> multiplier can be 5 or 15 when `timeframe` is minutes and period or `date_end` to the present is > 7 days & < 30 days. Multiplier set to 5.
#> Timeframe set to 5 Minutes
#> # A tibble: 711 x 4
#>    timestamp           equity profit_loss profit_loss_pct
#>    <dttm>               <dbl>       <dbl>           <dbl>
#>  1 2020-05-18 09:30:00 96751.        -2.1      -0.0000217
#>  2 2020-05-18 09:35:00 96712.       -40.9      -0.000423 
#>  3 2020-05-18 09:40:00 96719.       -33.7      -0.000348 
#>  4 2020-05-18 09:45:00 96718.       -34.4      -0.000355 
#>  5 2020-05-18 09:50:00 96706.       -46.7      -0.000482 
#>  6 2020-05-18 09:55:00 96685.       -67.8      -0.000700 
#>  7 2020-05-18 10:00:00 96706.       -47.1      -0.000487 
#>  8 2020-05-18 10:05:00 96716.       -36.9      -0.000381 
#>  9 2020-05-18 10:10:00 96723.       -29.5      -0.000305 
#> 10 2020-05-18 10:15:00 96720.       -32.8      -0.000339 
#> # ... with 701 more rows
```

When `AlpacaforR` function arguments are omitted, they will be assumed with informative messages indicating what values were used for omitted arguments. In the case above, the most granular `timeframe` allowed for the period is assumed. 

To view the same data with a `timeframe` of hours instead, use the following:


```r
account_portfolio("2w", "1h")
#> # A tibble: 64 x 4
#>    timestamp           equity profit_loss profit_loss_pct
#>    <dttm>               <dbl>       <dbl>           <dbl>
#>  1 2020-05-18 09:30:00 96751.        -2.1      -0.0000217
#>  2 2020-05-18 10:30:00 96737.       -15.9      -0.000165 
#>  3 2020-05-18 11:30:00 96767.        14.2       0.000147 
#>  4 2020-05-18 12:30:00 96766.        13.4       0.000138 
#>  5 2020-05-18 13:30:00 96769.        16.0       0.000166 
#>  6 2020-05-18 14:30:00 96767.        14.4       0.000149 
#>  7 2020-05-18 15:30:00 96744.        -8.4      -0.0000868
#>  8 2020-05-19 09:30:00 96772.        18.8       0.000194 
#>  9 2020-05-19 10:30:00 96806.        53.6       0.000554 
#> 10 2020-05-19 11:30:00 96899.       146.        0.00151  
#> # ... with 54 more rows
```


## Assets: Retrieve all assets or info about a single asset
The [Assets Endpoint](https://alpaca.markets/docs/api-documentation/api-v2/assets/) serves as a queryable master list of assets 💰 available for trade and data consumption from Alpaca. Assets are sorted by asset class, exchange and symbol.
Calling the function without arguments retrieves all assets. Be forewarned; this takes a while.
```r
## NOT RUN
assets()
```
Assets can be retrieved by providing:

1. the asset symbol

```r
(amzn <- assets("AMZN"))
#> # A tibble: 1 x 10
#>   id    class exchange symbol name  status tradable marginable shortable
#>   <chr> <chr> <chr>    <chr>  <chr> <chr>  <lgl>    <lgl>      <lgl>    
#> 1 f801~ us_e~ NASDAQ   AMZN   Amaz~ active TRUE     TRUE       TRUE     
#> # ... with 1 more variable: easy_to_borrow <lgl>
```
2. a vector of asset symbols (symbols need not be capitalized)

```r
assets(c("AMZN", "fb"))
#> # A tibble: 2 x 10
#>   id    class exchange symbol name  status tradable marginable shortable
#>   <chr> <chr> <chr>    <chr>  <chr> <chr>  <lgl>    <lgl>      <lgl>    
#> 1 f801~ us_e~ NASDAQ   AMZN   Amaz~ active TRUE     TRUE       TRUE     
#> 2 fc6a~ us_e~ NASDAQ   FB     Face~ active TRUE     TRUE       TRUE     
#> # ... with 1 more variable: easy_to_borrow <lgl>
```
3. or by the asset id

```r
assets(amzn$id)
#> # A tibble: 1 x 10
#>   id    class exchange symbol name  status tradable marginable shortable
#>   <chr> <chr> <chr>    <chr>  <chr> <chr>  <lgl>    <lgl>      <lgl>    
#> 1 f801~ us_e~ NASDAQ   AMZN   Amaz~ active TRUE     TRUE       TRUE     
#> # ... with 1 more variable: easy_to_borrow <lgl>
```


## Calendar: Retrieve a calendar of trading days & times
The [Calendar Endpoint](https://alpaca.markets/docs/api-documentation/api-v2/calendar/) serves the full list of market days from 1970 to 2029. It can also be queried by specifying a start and/or end time to narrow down the results. In addition to the dates, the response also contains the specific open and close times for the market days, taking into account early closures. The `calendar` function as of `AlpacaforR 0.3.0` will return [intervals](https://lubridate.tidyverse.org/reference/Interval-class.html) spanning the market `day` and `session` for easily subsetting Date type vectors, as well as the three letter abbreviation for the day of the week the date represents.
Visit the [Calendar Endpoint](https://docs.alpaca.markets/api-documentation/web-api/calendar/) to learn everything there is to know about the requests and responses for this API.


```r
#Get today's hours
calendar()
#> `from`, `to` arg(s) is/are NULL, setting from/to to 2020-05-29
#>         date  open close session_open session_close
#> 1 2020-05-29 09:30 16:00        07:00         19:00
#>                                                day
#> 1 2020-05-29 09:30:00 EDT--2020-05-29 16:00:00 EDT
#>                                            session dow
#> 1 2020-05-29 07:00:00 EDT--2020-05-29 19:00:00 EDT Fri

#Get the schedule for the next week
calendar(to = lubridate::today() + lubridate::weeks(1))
#> `from` arg(s) is/are NULL, setting from/to to 2020-05-29
#>         date  open close session_open session_close
#> 1 2020-05-29 09:30 16:00        07:00         19:00
#> 2 2020-06-01 09:30 16:00        07:00         19:00
#> 3 2020-06-02 09:30 16:00        07:00         19:00
#> 4 2020-06-03 09:30 16:00        07:00         19:00
#> 5 2020-06-04 09:30 16:00        07:00         19:00
#> 6 2020-06-05 09:30 16:00        07:00         19:00
#>                                                day
#> 1 2020-05-29 09:30:00 EDT--2020-05-29 16:00:00 EDT
#> 2 2020-06-01 09:30:00 EDT--2020-06-01 16:00:00 EDT
#> 3 2020-06-02 09:30:00 EDT--2020-06-02 16:00:00 EDT
#> 4 2020-06-03 09:30:00 EDT--2020-06-03 16:00:00 EDT
#> 5 2020-06-04 09:30:00 EDT--2020-06-04 16:00:00 EDT
#> 6 2020-06-05 09:30:00 EDT--2020-06-05 16:00:00 EDT
#>                                            session dow
#> 1 2020-05-29 07:00:00 EDT--2020-05-29 19:00:00 EDT Fri
#> 2 2020-06-01 07:00:00 EDT--2020-06-01 19:00:00 EDT Mon
#> 3 2020-06-02 07:00:00 EDT--2020-06-02 19:00:00 EDT Tue
#> 4 2020-06-03 07:00:00 EDT--2020-06-03 19:00:00 EDT Wed
#> 5 2020-06-04 07:00:00 EDT--2020-06-04 19:00:00 EDT Thu
#> 6 2020-06-05 07:00:00 EDT--2020-06-05 19:00:00 EDT Fri
```

Subsetting market data using the intervals returned from this function will be covered in the <a href="#market-data">Market Data</a> section.

#### A Note on Timezones
All Dates/Datetimes are forced (See [`lubridate::force_tz`](https://lubridate.tidyverse.org/reference/force_tz.html)) to America/New York timezone in which the NYSE operates for Market-Data and Calendar functions. This means that if [`lubridate::now`](https://lubridate.tidyverse.org/reference/now.html) is used to specify 3PM in the local timezone, it will be forced to 3PM in the "America/New_York"" timezone. This eliminates the need to consistently account for timezone conversions when providing inputs to retrieve historical data using `market_data`. For real-time quotes, see the Polygon (`polygon`) function (available to Alpaca members with live accounts) as it is best suited for obtaining this information.

## Clock:  Retrieve current market status and info
The `clock` function accesses the [Clock endpoint](https://docs.alpaca.markets/api-documentation/web-api/clock/), used to gain an understanding of how the local time compares to "America/New_York." A timezone can be specified to the `tz` argument to determine how the market hours compare to the specified timezone hours. If no `tz` argument is provided, and the local timezone differs from "America/New_York", `clock` will automatically provide the local conversion and offset. 


```r
clock()
#> $timestamp
#> [1] "2020-05-29 14:48:46 EDT"
#> 
#> $is_open
#> [1] TRUE
#> 
#> $next_open
#> [1] "2020-06-01 09:30:00 EDT"
#> 
#> $next_close
#> [1] "2020-05-29 16:00:00 EDT"
clock(tz = "America/Los_Angeles")
#> $timestamp
#> $timestamp$market
#> [1] "2020-05-29 14:48:46 EDT"
#> 
#> $timestamp$local
#> [1] "2020-05-29 11:48:46 PDT"
#> 
#> $timestamp$offset
#> [1] "3H 0M 0S"
#> 
#> 
#> $is_open
#> [1] TRUE
#> 
#> $next_open
#> $next_open$market
#> [1] "2020-06-01 09:30:00 EDT"
#> 
#> $next_open$local
#> [1] "2020-06-01 06:30:00 PDT"
#> 
#> $next_open$offset
#> [1] "3H 0M 0S"
#> 
#> 
#> $next_close
#> $next_close$market
#> [1] "2020-05-29 16:00:00 EDT"
#> 
#> $next_close$local
#> [1] "2020-05-29 13:00:00 PDT"
#> 
#> $next_close$offset
#> [1] "3H 0M 0S"
```

## Watchlist: Store a list of assets of interest
The `watchlist` function accesses all [Watchlist Endpoints](https://alpaca.markets/docs/api-documentation/api-v2/watchlist/). An account can have multiple watchlists and each is uniquely identified by id but can also be addressed by a user-defined name. Each watchlist is an ordered list of assets.

The current watchlists can be retrieved by calling `watchlist` without arguments:

```r
watchlist()
#> # A tibble: 1 x 5
#>   name   updated_at          id            account_id        created_at         
#>   <chr>  <dttm>              <chr>         <chr>             <dttm>             
#> 1 Prima~ 2020-05-27 07:43:43 f2da8074-cca~ 0ef8f355-319d-4d~ 2020-05-27 07:43:43
```

To start, create a watchlist named test with Microsoft

```r
(wl <- watchlist(name = "test", tickers = "MSFT"))
#>                                     id     class exchange symbol
#> 1 b6d1aa75-5c9c-4353-a305-9e2caa1925ab us_equity   NASDAQ   MSFT
#>                                 name status tradable marginable shortable
#> 1 Microsoft Corporation Common Stock active     TRUE       TRUE      TRUE
#>   easy_to_borrow
#> 1           TRUE
# See it in the list of watchlists
watchlist()
#> # A tibble: 2 x 5
#>   name   updated_at          id            account_id        created_at         
#>   <chr>  <dttm>              <chr>         <chr>             <dttm>             
#> 1 Prima~ 2020-05-27 07:43:43 f2da8074-cca~ 0ef8f355-319d-4d~ 2020-05-27 07:43:43
#> 2 test   2020-05-29 14:48:47 b691f132-3aa~ 0ef8f355-319d-4d~ 2020-05-29 14:48:47
```

Watchlists can be retrieved by the user provided name

```r
(test <- watchlist("test"))
#>                                     id     class exchange symbol
#> 1 b6d1aa75-5c9c-4353-a305-9e2caa1925ab us_equity   NASDAQ   MSFT
#>                                 name status tradable marginable shortable
#> 1 Microsoft Corporation Common Stock active     TRUE       TRUE      TRUE
#>   easy_to_borrow
#> 1           TRUE
identical(test,wl)
#> [1] TRUE
```

Each watchlist `tibble` has an `info` attribute that stores details like when it was created, lasted updated and more.

```r
# Get it's info
attr(test, "info")
#> # A tibble: 1 x 5
#>   name  updated_at          id             account_id        created_at         
#>   <chr> <dttm>              <chr>          <chr>             <dttm>             
#> 1 test  2020-05-29 14:48:47 b691f132-3aa2~ 0ef8f355-319d-4d~ 2020-05-29 14:48:47
```

Watchlist items can be replaced by changing the action to `"replace"` or `"r"` for short. *Note* that the default behavior when using `action = "replace"` is to replace the existing assets in the watchlist with those specified to `tickers` in the call to replace.
Any watchlist indicated with the `watchlist_id` will automatically be renamed when a new `name` is provided.

```r
# Update the watchlist name to test2 and replace the symbols with "FB", "AMZN", "NFLX", "GOOG"
(wl <- watchlist("test", name = "test2", tickers = c("FB", "AMZN", "NFLX", "GOOG", "WMT"), action = "r"))
#>                                     id     class exchange symbol
#> 1 fc6a5dcd-4a70-4b8d-b64f-d83a6dae9ba4 us_equity   NASDAQ     FB
#> 2 f801f835-bfe6-4a9d-a6b1-ccbb84bfd75f us_equity   NASDAQ   AMZN
#> 3 bb2a26c0-4c77-4801-8afc-82e8142ac7b8 us_equity   NASDAQ   NFLX
#> 4 f30d734c-2806-4d0d-b145-f9fade61432b us_equity   NASDAQ   GOOG
#> 5 3f3e0ff9-599f-4fec-8842-6bc53f5129a1 us_equity     NYSE    WMT
#>                                  name status tradable marginable shortable
#> 1 Facebook, Inc. Class A Common Stock active     TRUE       TRUE      TRUE
#> 2       Amazon.com, Inc. Common Stock active     TRUE       TRUE      TRUE
#> 3          Netflix, Inc. Common Stock active     TRUE       TRUE      TRUE
#> 4 Alphabet Inc. Class C Capital Stock active     TRUE       TRUE      TRUE
#> 5                        Walmart Inc. active     TRUE       TRUE      TRUE
#>   easy_to_borrow
#> 1           TRUE
#> 2           TRUE
#> 3           TRUE
#> 4           TRUE
#> 5           TRUE
attr(wl, "info")
#> # A tibble: 1 x 5
#>   name  updated_at          id             account_id        created_at         
#>   <chr> <dttm>              <chr>          <chr>             <dttm>             
#> 1 test2 2020-05-29 14:48:49 b691f132-3aa2~ 0ef8f355-319d-4d~ 2020-05-29 14:48:47
```

Individual assets can be deleted or added to watchlists using actions `"add"` or `"delete"` (`"a"`/`"d"` for short). Here, the watchlist name is updated to better reflect the tickers it will contain after the call is completed.

```r
# Walmart needn't be in there, remove it and rename the watchlist appropriately
(wl <- watchlist("test2", name = "FANG", ticker = "WMT", action = "d"))
#>                                     id     class exchange symbol
#> 1 fc6a5dcd-4a70-4b8d-b64f-d83a6dae9ba4 us_equity   NASDAQ     FB
#> 2 f801f835-bfe6-4a9d-a6b1-ccbb84bfd75f us_equity   NASDAQ   AMZN
#> 3 bb2a26c0-4c77-4801-8afc-82e8142ac7b8 us_equity   NASDAQ   NFLX
#> 4 f30d734c-2806-4d0d-b145-f9fade61432b us_equity   NASDAQ   GOOG
#>                                  name status tradable marginable shortable
#> 1 Facebook, Inc. Class A Common Stock active     TRUE       TRUE      TRUE
#> 2       Amazon.com, Inc. Common Stock active     TRUE       TRUE      TRUE
#> 3          Netflix, Inc. Common Stock active     TRUE       TRUE      TRUE
#> 4 Alphabet Inc. Class C Capital Stock active     TRUE       TRUE      TRUE
#>   easy_to_borrow
#> 1           TRUE
#> 2           TRUE
#> 3           TRUE
#> 4           TRUE
attr(wl, "info")
#> # A tibble: 1 x 5
#>   name  updated_at          id             account_id        created_at         
#>   <chr> <dttm>              <chr>          <chr>             <dttm>             
#> 1 FANG  2020-05-29 14:48:50 b691f132-3aa2~ 0ef8f355-319d-4d~ 2020-05-29 14:48:47
```

Assets can be added to the watchlist, and the name can be updated accordingly when `action = "a"`.

```r
# A new go-to for gains: "FABANGG"
(wl <- watchlist("FANG", "FABANGG", ticker = c("AAPL", "BYND", "GOOGL"), action = "a"))
#>                                     id     class exchange symbol
#> 1 b0b6dd9d-8b9b-48a9-ba46-b9d54906e415 us_equity   NASDAQ   AAPL
#> 2 317faf0e-4956-4d40-b3eb-3fba8af59dca us_equity   NASDAQ   BYND
#> 3 69b15845-7c63-4586-b274-1cfdfe9df3d8 us_equity   NASDAQ  GOOGL
#> 4 fc6a5dcd-4a70-4b8d-b64f-d83a6dae9ba4 us_equity   NASDAQ     FB
#> 5 f801f835-bfe6-4a9d-a6b1-ccbb84bfd75f us_equity   NASDAQ   AMZN
#> 6 bb2a26c0-4c77-4801-8afc-82e8142ac7b8 us_equity   NASDAQ   NFLX
#> 7 f30d734c-2806-4d0d-b145-f9fade61432b us_equity   NASDAQ   GOOG
#>                                  name status tradable marginable shortable
#> 1             Apple Inc. Common Stock active     TRUE       TRUE      TRUE
#> 2      Beyond Meat, Inc. Common Stock active     TRUE       TRUE     FALSE
#> 3  Alphabet Inc. Class A Common Stock active     TRUE       TRUE      TRUE
#> 4 Facebook, Inc. Class A Common Stock active     TRUE       TRUE      TRUE
#> 5       Amazon.com, Inc. Common Stock active     TRUE       TRUE      TRUE
#> 6          Netflix, Inc. Common Stock active     TRUE       TRUE      TRUE
#> 7 Alphabet Inc. Class C Capital Stock active     TRUE       TRUE      TRUE
#>   easy_to_borrow
#> 1           TRUE
#> 2          FALSE
#> 3           TRUE
#> 4           TRUE
#> 5           TRUE
#> 6           TRUE
#> 7           TRUE
```

Arguments (for any R function) can be partialled, or alternatively omitted entirely if they are provided in the order they appear in the function call. While this is not advisable when function arguments (or their order) are likely to change (such as when using development versions) or when programming generally. It's alright to do so when working at the console, or when writing analyses or executing a couple of trading functions from a script. Here's a shorthand approach to deleting assets from the watchlist and renaming it.

```r
# trim it down and rename it appropriately
(wl <- watchlist("FABANGG", "FANG", t = c("BYND", "GOOGL", "AAPL"), a = "d"))
#>                                     id     class exchange symbol
#> 1 fc6a5dcd-4a70-4b8d-b64f-d83a6dae9ba4 us_equity   NASDAQ     FB
#> 2 f801f835-bfe6-4a9d-a6b1-ccbb84bfd75f us_equity   NASDAQ   AMZN
#> 3 bb2a26c0-4c77-4801-8afc-82e8142ac7b8 us_equity   NASDAQ   NFLX
#> 4 f30d734c-2806-4d0d-b145-f9fade61432b us_equity   NASDAQ   GOOG
#>                                  name status tradable marginable shortable
#> 1 Facebook, Inc. Class A Common Stock active     TRUE       TRUE      TRUE
#> 2       Amazon.com, Inc. Common Stock active     TRUE       TRUE      TRUE
#> 3          Netflix, Inc. Common Stock active     TRUE       TRUE      TRUE
#> 4 Alphabet Inc. Class C Capital Stock active     TRUE       TRUE      TRUE
#>   easy_to_borrow
#> 1           TRUE
#> 2           TRUE
#> 3           TRUE
#> 4           TRUE
attr(wl, "info")
#> # A tibble: 1 x 5
#>   name  updated_at          id             account_id        created_at         
#>   <chr> <dttm>              <chr>          <chr>             <dttm>             
#> 1 FANG  2020-05-29 14:48:51 b691f132-3aa2~ 0ef8f355-319d-4d~ 2020-05-29 14:48:47
```

Delete the watchlist to start fresh.

```r
# Delete the watchlist
watchlist("FANG", a = "d")
#> Watchlist deleted successfully
#> # A tibble: 0 x 0
```

## Market Data
The `market_data` function is designed to access market & pricing data 📈 provided by Alpaca via the [Market Data Endpoint](https://alpaca.markets/docs/api-documentation/api-v2/market-data/) and via Alpaca's [Polygon integration](https://alpaca.markets/docs/api-documentation/api-v2/polygon-integration/). Making a request from one or the other is as simple as switching `v = 1` for Alpaca's API (the default) to `v = 2` for [Polygon's Aggregates Endpoint](https://polygon.io/docs/#get_v2_aggs_ticker__ticker__range__multiplier___timespan___from___to__anchor) (available to Alpaca users with a live account).
The Alpaca Data API consolidates data sources from five different exchanges.

 - IEX (Investors Exchange LLC)
 - NYSE National, Inc.
 - Nasdaq BX, Inc.
 - Nasdaq PSX
 - NYSE Chicago, Inc.

By specifying the Polygon API using the argument `v = 2` it is assumed that a live trading account is set-up with Alpaca, as access to the Polygon integration is dependent upon having a live account with Alpaca.
Data is returned as a list of `tibble`s (one for each symbol provided to `ticker`) with OHLCV bar format 📊 in each. The only required input is the ticker(s) as a character vector, and it will return pricing data for the last 7 trading days, by day, for each ticker. In the call below, `v = 1` is the default, thus the data returned is from the Alpaca API: 


```r
market_data("AMZN")
#> `from` argument omitted, setting to 2020-05-22
#> `to` argument omitted, setting to 2020-05-29
#> Floor/Ceiling dates are necessary to retrieve inclusive aggregates
#> 'to'/'until' coerced to 2020-05-30
#> $AMZN
#>         time    open    high      low   close  volume
#> 1 2020-05-22 2455.01 2469.85 2430.130 2436.89 5253352
#> 2 2020-05-26 2458.00 2462.00 2414.060 2423.70 3153936
#> 3 2020-05-27 2404.99 2413.58 2330.000 2409.51 4754020
#> 4 2020-05-28 2384.33 2436.97 2378.230 2401.05 2866537
#> 5 2020-05-29 2415.94 2426.00 2398.197 2409.88 3739096
```

The function accepts different sets of optional arguments depending on whether the Alpaca API (`v=1`) or Polygon Aggregates API (`v=2`) is used, see `?market_data` for full details on which arguments are used with each respective API.
To specify a date range to the `v1` API, the `from`, `to` / `after`, `until` arguments can be used. These are inclusive/exclusive date bounds respectively. Here, hourly data for the first seven days of January 2020 is retrieved inclusive:

```r
market_data("amzn", from = "2020-01-01", to = "2020-01-07")
#> Floor/Ceiling dates are necessary to retrieve inclusive aggregates
#> 'to'/'until' coerced to 2020-01-08
#> $AMZN
#>         time    open     high      low   close  volume
#> 1 2020-01-02 1874.79 1898.000 1864.150 1897.71 3583611
#> 2 2020-01-03 1864.50 1886.197 1864.500 1874.93 3293469
#> 3 2020-01-06 1860.00 1903.690 1860.000 1903.33 3598872
#> 4 2020-01-07 1904.50 1913.890 1892.043 1906.86 3569706
#> 5 2020-01-08 1898.68 1911.000 1886.445 1892.09 3100021
```
Note that the function uses the closest ceiling & floor dates to the dates supplied in order to ensure that the data returned for all time ranges returns *all* of the days that one would expect for a given call.

`after` and `until` can be used when `v = 1` to make exclusive date bounds:

```r
market_data("amzn", after = "2020-01-02", until = "2020-01-07")
#> $AMZN
#>         time   open     high    low   close  volume
#> 1 2020-01-03 1864.5 1886.197 1864.5 1874.93 3293469
#> 2 2020-01-06 1860.0 1903.690 1860.0 1903.33 3598872
```

### Arguments to `market_data` with the V1 API
The options for the `timeframe` argument using the `v = 1` API include `"m"`, `"min"`, `"minute"`, and `"d"`, `"day"` (the default). When using a minute `timeframe`, the `multiplier` can by `1`, `5`, or `15` whereas when using `timeframe = "day"` the only multiplier available is `1`. The bar `limit` argument can range from `1` to `1000` and has various default values according to the timeframe chosen. If left blank, the `limit` will default to `1000`. If the date range includes more than 1000 bars and `full = F`, then the API will return the 1000 most recent bars. 

See `?market_data()` for more details or visit the [Market Data API](https://docs.alpaca.markets/api-documentation/web-api/market-data/) webpage to learn everything there is to know about the requests and responses for this API.

Here, data for multiple tickers is retrieved at a five minute level of `timeframe` aggregation. The results would be quite long, so just the range of time for each day is shown:

```r
tickers <- c("BYND", "FB")
#Getting 5Min bar pricing data for the last 6 trading days. 
market_data(tickers, m = 5, timeframe= "Min") %>% 
  purrr::map(~{
    .dat <- .x
    .x %>% 
    dplyr::mutate(day = lubridate::as_date(time)) %>% 
  {purrr::map(unique(.[["day"]]), ~{
    .dat %>% dplyr::filter(lubridate::as_date(time) == .x) %>% 
      {range(.[["time"]])}
  })}
  })
#> `from` argument omitted, setting to 2020-05-22
#> `to` argument omitted, setting to 2020-05-29
#> Floor/Ceiling dates are necessary to retrieve inclusive aggregates
#> 'to'/'until' coerced to 2020-05-29 00:05:00
#> $BYND
#> $BYND[[1]]
#> [1] "2020-05-22 09:30:00 EDT" "2020-05-22 15:55:00 EDT"
#> 
#> $BYND[[2]]
#> [1] "2020-05-26 08:25:00 EDT" "2020-05-26 15:55:00 EDT"
#> 
#> $BYND[[3]]
#> [1] "2020-05-27 09:05:00 EDT" "2020-05-27 15:55:00 EDT"
#> 
#> $BYND[[4]]
#> [1] "2020-05-28 09:30:00 EDT" "2020-05-28 15:55:00 EDT"
#> 
#> 
#> $FB
#> $FB[[1]]
#> [1] "2020-05-22 07:45:00 EDT" "2020-05-22 16:10:00 EDT"
#> 
#> $FB[[2]]
#> [1] "2020-05-26 07:00:00 EDT" "2020-05-26 15:55:00 EDT"
#> 
#> $FB[[3]]
#> [1] "2020-05-27 07:00:00 EDT" "2020-05-27 16:45:00 EDT"
#> 
#> $FB[[4]]
#> [1] "2020-05-28 07:10:00 EDT" "2020-05-28 16:40:00 EDT"
```

*Note:* The API is returning some data from pre-market hours for both Beyond Meat and Amazon. This is unusual for the Alpaca API, but fairly commonplace from the Polygon Aggregates API, discussed next.

### Arguments to `market_data` with the V2 API (Live trading accounts only)
The [Polygon API Aggregates Endpoint](https://polygon.io/docs/#get_v2_aggs_ticker__ticker__range__multiplier___timespan___from___to__anchor) is called when parameter `v = 2`, Additional arguments are well-documented in the help file (see `?market_data`).
Note that the Polygon API does not have a `limit` argument, but does have a limit on how many bars are returned. The API limit is computed on the API end and it is not predictable as to how much data will be returned before the limit is reached. If the range of times requested from the API exceed what can be returned in a single call, the API will generally return the data from the initial segment of the timeframe, with a large gap, followed by the last few bars of data or it will return the most recent data until the limit is reached leaving off the oldest data. This behavior can be witnessed when `full = F` (the default). There is a guided demo of the behavior of the API with regard to limits in the examples for `?market_data`. This behavior is what inspired the development of the `full = T` feature. 

When `full =  T` (for both `v1` & `v2` APIs) the function will attempt to anticipate what data is expected based on the range of dates requested, and will re-query the API as many times as necessary to fill the request. Any remaining gaps will be filled with `NA` values, allowing for omission or imputation of missing data as needed. If the API is queried with the default `full = F` and upon inspection, large gaps are found in the data, try setting `full = T`. If any issues arise, please submit an [issue](https://github.com/yogat3ch/AlpacaforR/issues).

#### Additional details on the V2 Polygon Aggregates Endpoint
The Polygon API allows for the following timeframes:

 - `'m'`/`'min'`/`'minute'`
 - `'h'`/`'hour'`
 - `'d'`/`'day'`
 - `'w'`/`'week'`
 - `'M'`/`'mo'`/`'month'` (*Note* capitalized M for month if using single letter abbreviation)
 - `'q'`/`'quarter'`
 - `'y'`/`'year'`

Any integer can be supplied as the `multiplier` argument, however, atypical numbers can return unexpected results. The following combinations of `multiplier` and `timeframe` values have been systematically tested and prove to return expected data reliably:

 - `'m'`: `1`, `5`, `15`
 - `'h'`: `1`
 - `'d'`: `1`
 - `'w'`: `1`, `2`, `4`
 - `'M'`: `1`, `2`, `3`
 - `'q'`: `1`
 - `'y'`: `1`

*Note:* With `multiplier` greater than one, based on numerous trials for the various timeframes it appears that the Polygon API takes the nearest floor (previous) date based on the timeframe prior to the `from` date and begins providing data on the date that is `multiplier * timeframe` later.  IE, for the week timeframe, the API will determine the floor (previous) Sunday relative to the `from` date and start on the Sunday `multiplier *` weeks from that floor Sunday.

**Minutes**
When `timeframe = "minute"` the API will return data for the entire session of each trading day beginning in pre-market hours at 4AM and concluding in after-market hours between 7 & 9PM, however, the data outside of standard trading hours has unexpected gaps at a higher frequency than that of data for market hours 9:30A - 4:30P. Only the market hours (rather than session hours) are accounted for when `full = T` for the `"minute"` timeframe


```r
bynd <- market_data("BYND", v = 2, time = "m", m = 5)
#> `from` argument omitted, setting to 2020-05-22
#> `to` argument omitted, setting to 2020-05-29
#> Floor/Ceiling dates are necessary to retrieve inclusive aggregates
#> 'to'/'until' coerced to 2020-05-29 00:05:00
bynd$BYND %>% 
  dplyr::mutate(day = lubridate::as_date(time)) %>% 
  {purrr::map(unique(.[["day"]]), ~{
    bynd$BYND %>% dplyr::filter(lubridate::as_date(time) == .x) %>% 
      {range(.[["time"]])}
  })}
#> [[1]]
#> [1] "2020-05-22 04:00:00 EDT" "2020-05-22 19:55:00 EDT"
#> 
#> [[2]]
#> [1] "2020-05-26 04:00:00 EDT" "2020-05-26 19:55:00 EDT"
#> 
#> [[3]]
#> [1] "2020-05-27 04:05:00 EDT" "2020-05-27 19:55:00 EDT"
#> 
#> [[4]]
#> [1] "2020-05-28 04:00:00 EDT" "2020-05-28 19:55:00 EDT"
#> 
#> [[5]]
#> [1] "2020-05-29 04:05:00 EDT" "2020-05-29 14:45:00 EDT"
```

This returned data demonstrates how pre-market and after-market hours will tend to have gaps:

This can be illustrated by first retrieving the session hours for a random market day

```r
d <- "2020-05-13"
(cal <- calendar(from = d, to  = d))
#>         date  open close session_open session_close
#> 1 2020-05-13 09:30 16:00        07:00         19:00
#>                                                day
#> 1 2020-05-13 09:30:00 EDT--2020-05-13 16:00:00 EDT
#>                                            session dow
#> 1 2020-05-13 07:00:00 EDT--2020-05-13 19:00:00 EDT Wed
```

Second retrieving the market data by minute for that day

```r
(md <- market_data("BYND", v = 2, time = "m", m = 1, from = d, to = d))
#> Floor/Ceiling dates are necessary to retrieve inclusive aggregates
#> 'to'/'until' coerced to 2020-05-13 00:01:00
```

Third, the data is subsetted for the typical trading day hours and those outside:

```r
library(dplyr)
trading_hours <- md$BYND %>% 
  filter(lubridate::`%within%`(time, cal$day))
nontrading_hours <- md$BYND %>% 
  filter(!lubridate::`%within%`(time, cal$day))
```

Fourth, examine the gaps between time points in the trading hours vs those in the nontrading hours by making a frequency table of the time differences between time points in both time periods and compare. The name of each frequency in the table is the number of minutes gap between timepoints, while the value is the frequency of the gaps' occurrence as a decimal. 

Trading hours:

```r
prop.table(table(diff(trading_hours$time)))
#> 
#> 1 
#> 1
```

Non-trading hours:

```r
prop.table(table(diff(nontrading_hours$time)))
#> 
#>           1           2           3           4           5           6 
#> 0.718750000 0.125000000 0.052083333 0.020833333 0.010416667 0.020833333 
#>           8           9          10          11          12          13 
#> 0.010416667 0.010416667 0.003472222 0.003472222 0.003472222 0.006944444 
#>          17          19          22         392 
#> 0.003472222 0.003472222 0.003472222 0.003472222
```


**Hours**
Hours will span 4A to 7P for each trading day. Since this is an aggregate of minute timeframes, most data will be returned with few, if any gaps, unless the range requested exceeds the API limit. 


```r
market_data("BYND", v = 2, time = "h", m = 1, from = "2020-05-01", to = "2020-05-01")
#> Floor/Ceiling dates are necessary to retrieve inclusive aggregates
#> 'to'/'until' coerced to 2020-05-01 01:00:00
#> $BYND
#>     volume       vw    open   close    high   low                time     n
#> 1     1702 94.11173 96.5000 93.8000 96.5000 93.10 2020-05-01 04:00:00    21
#> 2     8924 93.99354 94.0100 93.8000 94.0100 93.80 2020-05-01 05:00:00    29
#> 3     6198 93.18345 93.8000 93.0000 94.0600 92.00 2020-05-01 06:00:00    67
#> 4    20012 93.93236 93.8700 94.2000 94.6000 93.00 2020-05-01 07:00:00   242
#> 5    44637 93.90513 93.8765 94.5100 94.8000 93.50 2020-05-01 08:00:00   618
#> 6  1251064 95.82580 94.7500 97.6015 97.6893 94.00 2020-05-01 09:00:00 11757
#> 7   791669 96.94366 97.7000 95.7900 98.7200 95.22 2020-05-01 10:00:00  9102
#> 8  1183098 93.28927 95.7600 92.2800 95.8000 91.28 2020-05-01 11:00:00 13172
#> 9   649933 91.62302 92.3200 91.3380 92.4800 90.70 2020-05-01 12:00:00  8223
#> 10  485007 91.01418 91.3500 91.6100 91.7700 90.10 2020-05-01 13:00:00  5982
#> 11  644146 92.96492 91.6300 93.7600 94.8900 91.20 2020-05-01 14:00:00  6941
#> 12 1361661 91.81450 93.6654 91.4500 93.9900 89.90 2020-05-01 15:00:00 15244
#> 13    8033 91.61933 91.5300 91.5000 91.9900 91.00 2020-05-01 16:00:00   121
#> 14    9770 91.18613 91.5000 91.2000 91.5998 91.00 2020-05-01 17:00:00    84
#> 15    5906 90.57038 91.0600 90.5500 91.2000 90.00 2020-05-01 18:00:00    93
#> 16    4361 90.93691 90.6001 91.2000 91.2000 90.60 2020-05-01 19:00:00    37
```

*Note:* the v2 API returns `vw`, the weighted volume, in addition to the raw volume. It also returns `n` which indicates the number of datapoints aggregated to calculate the value for the particular timeframe. 

**Days**
Days will span all trading days (generally M-F, `calendar` or the `polygon` "Market Holidays" endpoint can be consulted to find exceptions) for each week. Remember that the `from`/`to` arguments accept Date objects as well as character objects:

```r
market_data("BYND", v = 2, time = "d", m = 1, from = lubridate::today() - lubridate::weeks(1))
#> `to` argument omitted, setting to 2020-05-29
#> Floor/Ceiling dates are necessary to retrieve inclusive aggregates
#> 'to'/'until' coerced to 2020-05-30
#> $BYND
#>     volume       vw   open   close     high    low       time n
#> 1  6634708 136.9495 137.26 136.800 139.3800 135.07 2020-05-22 1
#> 2  4689223 135.2762 139.83 132.870 139.8800 131.50 2020-05-26 1
#> 3 10146197 122.1244 130.71 123.150 131.9800 116.00 2020-05-27 1
#> 4  5047931 123.1887 121.55 120.820 126.5993 120.00 2020-05-28 1
#> 5  4992561 125.2091 122.15 124.585 127.8900 121.50 2020-05-29 1
```



**Weeks**
Weeks will be aggregated from days for the week following each Sunday. The date of the Sunday will correspond to all data for the following trading week. 
The following returns weekly data for each week that has passed since the turn of the last quarter.

```r
market_data("BYND", v = 2, time = "w", m = 1, from = lubridate::floor_date(lubridate::today(), "quarter"))
#> `to` argument omitted, setting to 2020-05-29
#> Floor/Ceiling dates are necessary to retrieve inclusive aggregates
#> 'from'/'after' coerced to 2020-03-29
#> Floor/Ceiling dates are necessary to retrieve inclusive aggregates
#> 'to'/'until' coerced to 2020-06-07
#> $BYND
#>      volume        vw   open   close   high    low       time n
#> 1  16108710  62.81145  67.00  59.910  69.00  57.00 2020-03-29 5
#> 2  14530865  68.69574  62.75  72.300  74.72  62.05 2020-04-05 4
#> 3  34193361  76.59212  73.01  76.910  82.63  71.23 2020-04-12 5
#> 4 164006365  96.31846  76.24 108.780 113.48  76.00 2020-04-19 5
#> 5  76188172 102.12004 111.92  91.530 116.64  89.90 2020-04-26 5
#> 6 135123196 118.14757  89.00 133.510 136.00  88.51 2020-05-03 5
#> 7 102223538 137.61412 129.51 134.160 147.55 127.21 2020-05-10 5
#> 8  66948041 137.57926 137.49 136.800 144.92 128.81 2020-05-17 5
#> 9  24875912 125.43864 139.83 124.585 139.88 116.00 2020-05-24 4
```

**Months**
Months are aggregated by day for the entire month. The day represented in the time series varies based on the dates requested. Based on various inputs, the day might be the 30th, the 1st, or the 23rd of the month. However, if the request spans February, it could give the 30th of the months preceding February and the 1st for February and the months following. It's unclear whether the data aggregated on a day for that month corresponds to all the days in that month, or all the days between that day in one month and that day in the previous month.


```r
market_data("BYND", v = 2, time = "M", m = 1, lubridate::floor_date(lubridate::today(), "year"))
#> `to` argument omitted, setting to 2020-05-29
#> Floor/Ceiling dates are necessary to retrieve inclusive aggregates
#> 'to'/'until' coerced to 2020-07-01 10:30:00
#> $BYND
#>      volume        vw   open close   high   low       time  n
#> 1 160715441 113.12252 117.43 89.65 129.00 85.00 2020-01-31 20
#> 2 121771975  76.15980  95.00 64.18 101.93 48.18 2020-03-02 23
#> 3 296987694  93.18958  63.86 91.53 116.64 57.00 2020-04-02 21
```
Or, the API may omit February entirely, as it does above.

**Quarters**
Quarters will be represented by the following dates for each year:

- Q1: 03-30
- Q2: 06-30
- Q3: 09-30
- Q4: 12-30


```r
market_data("BYND", v = 2, time = "q", m = 1, from = lubridate::floor_date(lubridate::today(), "year"))
#> `to` argument omitted, setting to 2020-05-29
#> Floor/Ceiling dates are necessary to retrieve inclusive aggregates
#> 'from'/'after' coerced to 2019-12-30
#> Floor/Ceiling dates are necessary to retrieve inclusive aggregates
#> 'to'/'until' coerced to 2020-06-30
#> $BYND
#>      volume       vw  open   close   high   low       time  n
#> 1 614793332 105.1749 75.76  66.120 135.23 48.18 2019-12-30 62
#> 2 634198160 111.2729 67.00 124.585 147.55 57.00 2020-03-30 43
```

**Year**
Years are aggregated on 12-31 of each year. 


```r
market_data("FB", v = 2, time = "y", m = 1, from = lubridate::floor_date(lubridate::today(), "year") - lubridate::years(4))
#> `to` argument omitted, setting to 2020-05-29
#> Floor/Ceiling dates are necessary to retrieve inclusive aggregates
#> 'to'/'until' coerced to 2022-01-01 06:00:00
#> $FB
#>       volume       vw    open  close   high      low       time   n
#> 1 6446545749 114.6091 106.000 115.05 135.60  89.3700 2015-12-31 253
#> 2 4211787052 155.6883 116.030 176.46 184.25 115.5100 2016-12-31 251
#> 3 6922919575 168.5861 177.680 133.20 218.62 123.0200 2017-12-31 250
#> 4 4107230872 179.1920 134.450 204.41 208.93 128.5600 2018-12-31 252
#> 5 3160988707 192.4812 203.995 225.70 240.90 137.1006 2019-12-31 104
```

Finally, be sure to checkout the examples for `?market_data` to understand the rationale for and usage of the `full` argument.

## Polygon
Alpaca's integration with the Polygon API allows Alpaca members to use all [Reference](https://polygon.io/docs/#Reference) and [Stocks/Equities](https://polygon.io/docs/#Stocks___Equities) endpoints on the Polygon API. `AlpacaforR` provides a single go-to function to access all of the available Polygon endpoints: `polygon`. This function takes as it's first argument `ep`, short for endpoint, which can be the full name of the endpoint as it appears in the [Docs](https://polygon.io/docs/) or a one to two letter abbreviation of the endpoint which is typically the first letter of each of the first two words (that aren't wrapped in parentheses) of the name of the endpoint. The one exception being Snapshot - Single Ticker (`st`), which would otherwise conflict with Stock Splits (`ss`). 
For ease of referencing all of the Polygon endpoints without leaving R, the documentation for `?polygon` elaborates the names of the endpoints, their descriptions, details and parameters. Additionally, the `polygon` function itself provides reference tibbles of the abbreviations and full names of the endpoints by using `'all'` as the value for `ep` to show all endpoints, `'ref'/'reference'` for all the reference endpoints, `'sto'/'stocks'` for all the stock/equity endpoints.


```r
polygon("all")
#>                         name
#> t                    Tickers
#> tt              Ticker Types
#> td            Ticker Details
#> tn               Ticker News
#> m                    Markets
#> l                    Locales
#> ss              Stock Splits
#> sd           Stock Dividends
#> sf          Stock Financials
#> ms             Market Status
#> mh           Market Holidays
#> e                  Exchanges
#> ht           Historic Trades
#> hq    Historic Quotes (NBBO)
#> lt   Last Trade for a Symbol
#> lq   Last Quote for a Symbol
#> do          Daily Open/Close
#> cm        Condition Mappings
#> sa    Snapshot - All tickers
#> st  Snapshot - Single Ticker
#> sg Snapshot - Gainers/Losers
#> pc            Previous Close
#> a          Aggregates (Bars)
#> gd      Grouped Daily (Bars)
```

A plus (`+`) can be appended to the end of any of these reference keywords, or the abbreviation/name of an endpoint to view a helpful reference list with the following for each endpoint:
 
 - The full name of the endpoint
 - the description
 - the URL for the documentation
 - the URL of the endpoint itself
 - the parameters, with the default always in first position when options are available. When endpoints that take parameters are called without explicitly providing parameters, these defaults are used to call the endpoint.
 

```r
polygon("hq+")
#> $hq
#> $hq$nm
#> [1] "Historic Quotes (NBBO)"
#> 
#> $hq$desc
#> [1] "Get historic NBBO quotes for a ticker."
#> 
#> $hq$href
#> [1] "https://polygon.io/docs/#get_v2_ticks_stocks_nbbo__ticker___date__anchor"
#> 
#> $hq$url
#> [1] "/v2/ticks/stocks/nbbo/{ticker}/{date}"
#> 
#> $hq$params
#> $hq$params$ticker
#> [1] "AAPL"
#> 
#> $hq$params$date
#> [1] "2018-02-02"
#> 
#> $hq$params$timestamp
#> $hq$params$timestamp[[1]]
#> NULL
#> 
#> $hq$params$timestamp[[2]]
#> [1] 1
#> 
#> 
#> $hq$params$timestampLimit
#> $hq$params$timestampLimit[[1]]
#> NULL
#> 
#> $hq$params$timestampLimit[[2]]
#> [1] 1
#> 
#> 
#> $hq$params$reverse
#> $hq$params$reverse[[1]]
#> NULL
#> 
#> $hq$params$reverse[[2]]
#> [1] TRUE
#> 
#> $hq$params$reverse[[3]]
#> [1] FALSE
#> 
#> 
#> $hq$params$limit
#> [1]    10 50000
```

Many endpoints require parameters to be specified. The parameters can be specified as either named arguments passed to the function directly


```r
polygon("hq", ticker = "BYND", date = "2020-04-02")
#> # A tibble: 10 x 11
#>    time                      y     q c         z     p     s     x     P     S
#>    <dttm>                <dbl> <int> <lis> <int> <dbl> <int> <int> <int> <int>
#>  1 2020-04-02 04:00:00 1.59e18  2117 <int~     3  55      10    11     0     0
#>  2 2020-04-02 04:01:03 1.59e18  3597 <int~     3  64.5     1    12    66     2
#>  3 2020-04-02 04:20:39 1.59e18 13319 <int~     3  64.2     1    11    66     2
#>  4 2020-04-02 04:20:39 1.59e18 13320 <int~     3  64.4     1    12    66     2
#>  5 2020-04-02 04:27:54 1.59e18 16526 <int~     3  64.4     5    12    66     2
#>  6 2020-04-02 04:28:06 1.59e18 16648 <int~     3  64.4     1    12    66     2
#>  7 2020-04-02 04:28:06 1.59e18 16649 <int~     3  64.4     6    12    66     2
#>  8 2020-04-02 04:36:16 1.59e18 19827 <int~     3  64.5     1    12    66     2
#>  9 2020-04-02 04:42:13 1.59e18 21705 <int~     3  64.5     2    12    66     2
#> 10 2020-04-02 04:48:30 1.59e18 23979 <int~     3  64.5     3    12    66     2
#> # ... with 1 more variable: X <int>
```


or as a list with values named according to the parameter name.


```r
polygon("Last Quote+")
#> $lq
#> $lq$nm
#> [1] "Last Quote for a Symbol"
#> 
#> $lq$desc
#> [1] "Get the last quote tick for a given stock."
#> 
#> $lq$href
#> [1] "https://polygon.io/docs/#get_v1_last_quote_stocks__symbol__anchor"
#> 
#> $lq$url
#> [1] "/v1/last_quote/stocks/{symbol}"
#> 
#> $lq$params
#> $lq$params$symbol
#> [1] "AAPL"
# the following are equivalent
polygon("lq", params = list(symbol = "BYND"))
#> # A tibble: 1 x 7
#>   askexchange askprice asksize bidexchange bidprice bidsize timestamp          
#>         <int>    <dbl>   <int>       <int>    <dbl>   <int> <dttm>             
#> 1          11     124.       1          12     124.       1 2020-05-29 14:49:03
polygon("lq", symbol = "BYND")
#> # A tibble: 1 x 7
#>   askexchange askprice asksize bidexchange bidprice bidsize timestamp          
#>         <int>    <dbl>   <int>       <int>    <dbl>   <int> <dttm>             
#> 1          11     124.       1          12     124.       1 2020-05-29 14:49:03
```

Some endpoints provide query status info or map details (the data classes of the values in the returned object) and other information that can be accessed using `attr(obj, "query")` or `attr(obj, "map")` respectively (where `obj` is the object returned by `polygon`).

## Orders

Getting, submitting, and cancelling 🚫 orders are also made easy through `orders()` and `order_submit()`, but require some specific arguments. Visit the [Orders API](https://alpaca.markets/docs/api-documentation/api-v2/orders/) webpage to learn everything there is to know about the requests and responses for this API.

### `orders`
To view open orders for the paper account, use `orders()` (the default `status` is set to `"open"` and `live` defaults to `FALSE`)


```r
orders()
#> No orders for the selected query/filter criteria. 
#> Check `ticker_id` or set status = 'all' to see all orders.
#> list()
```


Alternatively, set the `status` to see specific subsets of orders based on their status: `"open"`, `"closed"`, and `"all"`. See `?orders` for more details. *Note* that the default `limit` is `50`. To return more or less than 50, `limit` must be set explicitly.


```r
orders(status = "all", limit = 10)
#> # A tibble: 10 x 28
#>    id    client_order_id created_at          updated_at         
#>    <chr> <chr>           <dttm>              <dttm>             
#>  1 3a3c~ d231c073-d028-~ 2020-05-29 14:27:21 2020-05-29 14:27:21
#>  2 dd38~ 2465fbc3-f07c-~ 2020-05-29 14:27:20 2020-05-29 14:27:20
#>  3 6a4a~ 817b6144-bbf5-~ 2020-05-29 14:27:19 2020-05-29 14:27:19
#>  4 b133~ 1e47b557-ee8b-~ 2020-05-29 14:27:19 2020-05-29 14:27:19
#>  5 8f2d~ 740ecfb3-df69-~ 2020-05-29 14:27:17 2020-05-29 14:27:17
#>  6 bb0d~ 5d471d88-74b5-~ 2020-05-29 14:27:17 2020-05-29 14:27:17
#>  7 a5d0~ cdcefc80-ee2f-~ 2020-05-29 14:26:37 2020-05-29 14:26:37
#>  8 f9c4~ aec33f1d-7cfa-~ 2020-05-29 14:26:36 2020-05-29 14:26:38
#>  9 f9d5~ aad8e0cc-0db7-~ 2020-05-29 14:26:36 2020-05-29 14:26:36
#> 10 3b07~ d2508727-21d1-~ 2020-05-29 14:26:34 2020-05-29 14:26:35
#> # ... with 24 more variables: submitted_at <dttm>, filled_at <dttm>,
#> #   expired_at <dttm>, canceled_at <dttm>, failed_at <dttm>,
#> #   replaced_at <dttm>, replaced_by <lgl>, replaces <lgl>, asset_id <chr>,
#> #   symbol <chr>, asset_class <chr>, qty <dbl>, filled_qty <dbl>,
#> #   filled_avg_price <dbl>, order_class <chr>, order_type <chr>, type <chr>,
#> #   side <chr>, time_in_force <chr>, limit_price <chr>, stop_price <lgl>,
#> #   status <chr>, extended_hours <lgl>, legs <list>
```

All arguments can be partial, ie abbreviated, up to the number of characters necessary to differentiate the argument from other arguments provided to the function. For `orders`, `status` is the only argument starting with s, so it can be partial, `after` can also be partial, and `lim` is all that is necessary to differentiate the argument from `live`. If the first letter of a value to an argument is unique, only the first letter need be provided in most cases. 

Here is the shorthand to view all orders placed since the beginning of the week:


```r
(orders_this_week <- orders(s = "a", a = lubridate::floor_date(lubridate::today(), "week"), lim = 10))
#> # A tibble: 10 x 28
#>    id    client_order_id created_at          updated_at         
#>    <chr> <chr>           <dttm>              <dttm>             
#>  1 3a3c~ d231c073-d028-~ 2020-05-29 14:27:21 2020-05-29 14:27:21
#>  2 dd38~ 2465fbc3-f07c-~ 2020-05-29 14:27:20 2020-05-29 14:27:20
#>  3 6a4a~ 817b6144-bbf5-~ 2020-05-29 14:27:19 2020-05-29 14:27:19
#>  4 b133~ 1e47b557-ee8b-~ 2020-05-29 14:27:19 2020-05-29 14:27:19
#>  5 8f2d~ 740ecfb3-df69-~ 2020-05-29 14:27:17 2020-05-29 14:27:17
#>  6 bb0d~ 5d471d88-74b5-~ 2020-05-29 14:27:17 2020-05-29 14:27:17
#>  7 a5d0~ cdcefc80-ee2f-~ 2020-05-29 14:26:37 2020-05-29 14:26:37
#>  8 f9c4~ aec33f1d-7cfa-~ 2020-05-29 14:26:36 2020-05-29 14:26:38
#>  9 f9d5~ aad8e0cc-0db7-~ 2020-05-29 14:26:36 2020-05-29 14:26:36
#> 10 3b07~ d2508727-21d1-~ 2020-05-29 14:26:34 2020-05-29 14:26:35
#> # ... with 24 more variables: submitted_at <dttm>, filled_at <dttm>,
#> #   expired_at <dttm>, canceled_at <dttm>, failed_at <dttm>,
#> #   replaced_at <dttm>, replaced_by <lgl>, replaces <lgl>, asset_id <chr>,
#> #   symbol <chr>, asset_class <chr>, qty <dbl>, filled_qty <dbl>,
#> #   filled_avg_price <dbl>, order_class <chr>, order_type <chr>, type <chr>,
#> #   side <chr>, time_in_force <chr>, limit_price <chr>, stop_price <lgl>,
#> #   status <chr>, extended_hours <lgl>, legs <list>
```

*Note* complex orders will automatically appear nested in the returned tibble. To change this behavior, set `nested = F`. 

Individual orders can be called by providing their id to `ticker_id` (the first argument):


```r
if (isTRUE(nrow(orders_this_week) > 0)) {
  # Works only if there are existing orders 
  (fo <- orders(orders_this_week[1,]$id))
  identical(unlist(orders_this_week[1,]), unlist(fo))
}
#> [1] FALSE
```

Individual orders can also be called by providing the client order ID to `ticker_id` and setting `client_order_id = T`.


```r
if (isTRUE(nrow(orders_this_week) > 0)) {
  # Works only if there are existing orders
  orders(orders_this_week[1,]$client_order_id, client_order_id = T)
}
#> # A tibble: 1 x 28
#>   id    client_order_id created_at          updated_at         
#>   <chr> <chr>           <dttm>              <dttm>             
#> 1 3a3c~ d231c073-d028-~ 2020-05-29 14:27:21 2020-05-29 14:27:21
#> # ... with 24 more variables: submitted_at <dttm>, filled_at <dttm>,
#> #   expired_at <dttm>, canceled_at <dttm>, failed_at <dttm>,
#> #   replaced_at <dttm>, replaced_by <lgl>, replaces <lgl>, asset_id <chr>,
#> #   symbol <chr>, asset_class <chr>, qty <dbl>, filled_qty <dbl>,
#> #   filled_avg_price <dbl>, order_class <chr>, order_type <chr>, type <chr>,
#> #   side <chr>, time_in_force <chr>, limit_price <lgl>, stop_price <lgl>,
#> #   status <chr>, extended_hours <lgl>, legs <lgl>
```


### `order_submit`
To submit *any kind of order,* whether it be:
 
 - a new order `action = "s"/"submit"`
 - a complex order `action = "s"/"submit"`
 - an order replacement `action = "r"/"replace"`
 - an order cancellation `action = "c"/"cancel"`
 - or canceling all orders `action = "cancel_all"`

use `order_submit()` with the appropriate arguments and fire away 🚀. The value supplied to the `action` argument determines what type of action will be taken and what parameters are required. The default action is `"submit"`.

A simple use case where a buy order for two shares of Beyond Meat is placed is below:

```r
# is the market open?
(.open <- clock()$is_open)
#> [1] TRUE
if (.open) {
  # if the market is open then place a market buy order for "BYND"
(bo <- order_submit("bynd", side = "b", q = 2, type = "m"))
}
#> # A tibble: 1 x 28
#>   id    client_order_id created_at          updated_at         
#>   <chr> <chr>           <dttm>              <dttm>             
#> 1 9e11~ 80828b78-ca0b-~ 2020-05-29 14:49:09 2020-05-29 14:49:09
#> # ... with 24 more variables: submitted_at <dttm>, filled_at <dttm>,
#> #   expired_at <dttm>, canceled_at <dttm>, failed_at <dttm>,
#> #   replaced_at <dttm>, replaced_by <lgl>, replaces <lgl>, asset_id <chr>,
#> #   symbol <chr>, asset_class <chr>, qty <dbl>, filled_qty <dbl>,
#> #   filled_avg_price <lgl>, order_class <chr>, order_type <chr>, type <chr>,
#> #   side <chr>, time_in_force <chr>, limit_price <lgl>, stop_price <lgl>,
#> #   status <chr>, extended_hours <lgl>, legs <lgl>
```


`order_submit` has extensive built-in auto-assumption of omitted arguments where they can be assumed based on other provided arguments. The documentation (`?order_submit`) and the examples therein go into detail as to the the required parameters necessary to invoke accurate auto-assumption for each action. Since traders will often place a stop, limit, or stop limit order following a buy order to mitigate downside risk, an 'expedited sell'  is one such auto-assumption feature. To execute an 'expedited sell', the function needs only the Order ID of the buy order, and the specifics of the sell order. 

But first, to set appropriate stops and limits, it's necessary to know the current price of the stock - this could be retrieved by calling `orders` to see what price the above order filled at, or by using the [Polygon Last Quote endpoint](https://polygon.io/docs/#get_v1_last_quote_stocks__symbol__anchor).


```r
(lq <- polygon("lq", symbol = "bynd"))
#> # A tibble: 1 x 7
#>   askexchange askprice asksize bidexchange bidprice bidsize timestamp          
#>         <int>    <dbl>   <int>       <int>    <dbl>   <int> <dttm>             
#> 1          17     124.       2          12     124.       2 2020-05-29 14:49:08
```
With this information, a stop order can be placed at the price 5% lower than what it was bought at. To connect this sell order to the buy order for cost basis reporting purposes, set `client_order_id = T` and the `client_order_id` for this sell order will be set to the Order ID of the buy order.

```r
if (.open) {
  (so <- order_submit(bo$id, stop = lq$askprice * .95, client_order_id = T))
}
#> `side` set to 'sell'
#> `qty` set to 2
#> `ticker_id` set to BYND
#> `client_order_id` set to 9e117b5a-a2c6-49d7-ac65-7fe99a06e4e6
#> `type` set to 'stop'
#> # A tibble: 1 x 28
#>   id    client_order_id created_at          updated_at         
#>   <chr> <chr>           <dttm>              <dttm>             
#> 1 bfe5~ 9e117b5a-a2c6-~ 2020-05-29 14:49:12 2020-05-29 14:49:12
#> # ... with 24 more variables: submitted_at <dttm>, filled_at <dttm>,
#> #   expired_at <dttm>, canceled_at <dttm>, failed_at <dttm>,
#> #   replaced_at <dttm>, replaced_by <lgl>, replaces <lgl>, asset_id <chr>,
#> #   symbol <chr>, asset_class <chr>, qty <dbl>, filled_qty <dbl>,
#> #   filled_avg_price <lgl>, order_class <chr>, order_type <chr>, type <chr>,
#> #   side <chr>, time_in_force <chr>, limit_price <lgl>, stop_price <dbl>,
#> #   status <chr>, extended_hours <lgl>, legs <lgl>
```

Informative messages indicate where the function made assumptions about the values for other arguments. 

To extend the example, suppose the price of BYND went up since the order was first placed, yet the stop is still set at 5% lower from where the order was bought. It would be wise to move the stop order up a bit to follow the price action. This can be done for simple orders (of which this is one), with `action = 'replace'`. First, retrieve the new price:


```r
(lq <- polygon("lq", symbol = "bynd"))
#> # A tibble: 1 x 7
#>   askexchange askprice asksize bidexchange bidprice bidsize timestamp          
#>         <int>    <dbl>   <int>       <int>    <dbl>   <int> <dttm>             
#> 1          17     124.       2          12     124.       2 2020-05-29 14:49:08
```

The replacement order will have a field `replaces` that will indicate the order it replaced, in this case the previous sell order. The sell order placed above was linked to the buy order via the `client_order_id` such that cost basis could accurately be reported. However, `client_order_id` must be unique for each order. So how does one keep this replacement order connected to the original buy order? 

The simplest way to do so is to provide a custom `client_order_id` with an incremented suffix appended for each successive replacement order. The full length of the `client_order_id` must be under `48` characters and the Alpaca generated IDs are `36` characters, which leaves $48 - 36 = 12$ characters for the suffix. This tracking method can be especially useful if implementing a trailing stop for a given buy order that will refresh often.

Here the `client_order_id` is created:

```r
(client_order_id <- paste0(bo$id,".2"))
#> [1] "9e117b5a-a2c6-49d7-ac65-7fe99a06e4e6.2"
nchar(client_order_id)
#> [1] 38
```

The replacement order can now be placed with a higher stop and remain effectively linked to it's original buy order via the first 36 characters of it's `client_order_id`

```r
if (.open && isTRUE(nrow(so) > 0)) {
  (ro <- order_submit(so$id, a = "r", stop = lq$askprice * .96, client_order_id = client_order_id))
}
#> # A tibble: 1 x 28
#>   id    client_order_id created_at          updated_at         
#>   <chr> <chr>           <dttm>              <dttm>             
#> 1 806d~ 9e117b5a-a2c6-~ 2020-05-29 14:49:12 2020-05-29 14:49:13
#> # ... with 24 more variables: submitted_at <dttm>, filled_at <dttm>,
#> #   expired_at <dttm>, canceled_at <dttm>, failed_at <dttm>,
#> #   replaced_at <dttm>, replaced_by <lgl>, replaces <chr>, asset_id <chr>,
#> #   symbol <chr>, asset_class <chr>, qty <dbl>, filled_qty <dbl>,
#> #   filled_avg_price <lgl>, order_class <chr>, order_type <chr>, type <chr>,
#> #   side <chr>, time_in_force <chr>, limit_price <lgl>, stop_price <dbl>,
#> #   status <chr>, extended_hours <lgl>, legs <lgl>
```

However, it's also possible that the trader would like to take a profit if the price moves up another 5% while simultaneously having a stop in place to prevent losses. An [Advanced Order](https://alpaca.markets/docs/trading-on-alpaca/orders/#oco-orders) called "`O`ne `C`ancels `O`ther" is perfect for this situation. First, the replacement order needs to be canceled.


```r
if (.open && isTRUE(nrow(ro) > 0)) {
  order_submit(ro$id, a = "c")
}
#> Order canceled successfully
#> list()
```

The `oco` order requires two additional parameters, an upper limit order provided to the argument `take_profit` as a named list, with a single item named `'limit_price'/'l'`:

```r
take_profit <- list(l = lq$askprice * 1.05)
```

and a lower limit, stop or both specified to `stop_loss` as a named list with the names `'stop_price'/'s'` & `'limit_price'/'l'`:

```r
stop_loss <- list(s = lq$askprice * .95)
```


Now the `oco` order class can be placed  by providing the id of the original buy order to passivley set argument defaults, while with another increment to the `client_order_id` can link this order to that original buy order.

```r
if (.open) {
  (oco <- order_submit(bo$id, order_class = "oco", time_in_force = "gtc", client_order_id = paste0(bo$id,".3"), take_profit = take_profit, stop_loss = stop_loss))
}
#> `side` set to 'sell'
#> `qty` set to 2
#> `ticker_id` set to BYND
#> `client_order_id` set to 9e117b5a-a2c6-49d7-ac65-7fe99a06e4e6
#> order_class: 'oco' requires type = 'limit'. `type` set to 'limit'.
#> # A tibble: 1 x 28
#>   id    client_order_id created_at          updated_at         
#>   <chr> <chr>           <dttm>              <dttm>             
#> 1 536f~ 9e117b5a-a2c6-~ 2020-05-29 14:49:14 2020-05-29 14:49:14
#> # ... with 51 more variables: submitted_at <dttm>, filled_at <dttm>,
#> #   expired_at <dttm>, canceled_at <dttm>, failed_at <dttm>,
#> #   replaced_at <dttm>, replaced_by <lgl>, replaces <lgl>, asset_id <chr>,
#> #   symbol <chr>, asset_class <chr>, qty <dbl>, filled_qty <dbl>,
#> #   filled_avg_price <lgl>, order_class <chr>, order_type <chr>, type <chr>,
#> #   side <chr>, time_in_force <chr>, limit_price <dbl>, stop_price <lgl>,
#> #   status <chr>, extended_hours <lgl>, legs$id <chr>, $client_order_id <chr>,
#> #   $created_at <dttm>, $updated_at <dttm>, $submitted_at <dttm>,
#> #   $filled_at <dttm>, $expired_at <dttm>, $canceled_at <dttm>,
#> #   $failed_at <dttm>, $replaced_at <dttm>, $replaced_by <lgl>,
#> #   $replaces <lgl>, $asset_id <chr>, $symbol <chr>, $asset_class <chr>,
#> #   $qty <dbl>, $filled_qty <dbl>, $filled_avg_price <lgl>, $order_class <chr>,
#> #   $order_type <chr>, $type <chr>, $side <chr>, $time_in_force <chr>,
#> #   $limit_price <lgl>, $stop_price <dbl>, $status <chr>,
#> #   $extended_hours <lgl>, $legs <lgl>
```

The additional linked orders for any Advanced Order can be viewed as it's `legs`. When submitting an order, the default response is to return the `legs` nested under the top level order. When calling `orders`, order legs can be unnested such that each row is a separate order by setting `nested = F` :

```r
if (.open) {
  oco$legs
}
#>                                     id                      client_order_id
#> 1 b93270a3-d179-4f1c-9c47-56b8388952a2 c385d1b0-ef31-4a37-ad1b-8a32fe957852
#>            created_at          updated_at        submitted_at filled_at
#> 1 2020-05-29 14:49:14 2020-05-29 14:49:14 2020-05-29 14:49:14      <NA>
#>   expired_at canceled_at failed_at replaced_at replaced_by replaces
#> 1       <NA>        <NA>      <NA>        <NA>          NA       NA
#>                               asset_id symbol asset_class qty filled_qty
#> 1 317faf0e-4956-4d40-b3eb-3fba8af59dca   BYND   us_equity   2          0
#>   filled_avg_price order_class order_type type side time_in_force limit_price
#> 1               NA         oco       stop stop sell           gtc          NA
#>   stop_price status extended_hours legs
#> 1    118.142   held          FALSE   NA
```

All open orders can be canceled by using the `"cancel_all"` keyword as the `action`

```r
order_submit(a = "cancel_all")
#> # A tibble: 1 x 28
#>   asset_class asset_id canceled_at         client_order_id created_at         
#>   <chr>       <chr>    <dttm>              <chr>           <dttm>             
#> 1 us_equity   317faf0~ NA                  9e117b5a-a2c6-~ 2020-05-29 14:49:14
#> # ... with 23 more variables: expired_at <dttm>, extended_hours <lgl>,
#> #   failed_at <dttm>, filled_at <dttm>, filled_avg_price <lgl>,
#> #   filled_qty <dbl>, id <chr>, legs <lgl>, limit_price <dbl>,
#> #   order_class <chr>, order_type <chr>, qty <dbl>, replaced_at <dttm>,
#> #   replaced_by <lgl>, replaces <lgl>, side <chr>, status <chr>,
#> #   stop_price <lgl>, submitted_at <dttm>, symbol <chr>, time_in_force <chr>,
#> #   type <chr>, updated_at <dttm>
```


`order_submit` is a versatile function, see it's documentation (`?order_submit`) and examples to learn about all it has to offer.

## Positions
All current positions or only the positions specified by ticker symbol are retrieved by calling `positions()`. See `?positions` for more details. `positions` has multiple actions:

 - `"get"/"g"` positions (the default)
 - `"close"/"c"` a position or positions provided by `ticker`
 - `"close_all"` which will cancel all open orders on currently held positions and then close those positions by selling all shares. Think of `action = "close_all"` as an emergency kill switch that will liquidate all positions. 
 Visit the [Positions API](https://alpaca.markets/docs/api-documentation/api-v2/positions/) webpage to learn everything there is to know about the requests and responses for this API.

Retrieve all positions:

```r
#If paper account:
positions()
#> # A tibble: 1 x 16
#>   asset_id symbol exchange asset_class   qty avg_entry_price side  market_value
#>   <chr>    <chr>  <chr>    <chr>       <dbl>           <dbl> <chr>        <dbl>
#> 1 317faf0~ BYND   NASDAQ   us_equity       2            124. long          249.
#> # ... with 8 more variables: cost_basis <dbl>, unrealized_pl <dbl>,
#> #   unrealized_plpc <dbl>, unrealized_intraday_pl <dbl>,
#> #   unrealized_intraday_plpc <dbl>, current_price <dbl>, lastday_price <dbl>,
#> #   change_today <dbl>
```

If a position exists, it can be closed using `action = "cancel"`

```r
positions("BYND", action = "c")
#> BYND closed successfully.
#> # A tibble: 1 x 28
#>   id    client_order_id created_at          updated_at         
#>   <chr> <chr>           <dttm>              <dttm>             
#> 1 5b1f~ 10524291-c324-~ 2020-05-29 14:49:16 2020-05-29 14:49:16
#> # ... with 24 more variables: submitted_at <dttm>, filled_at <dttm>,
#> #   expired_at <dttm>, canceled_at <dttm>, failed_at <dttm>,
#> #   replaced_at <dttm>, replaced_by <lgl>, replaces <lgl>, asset_id <chr>,
#> #   symbol <chr>, asset_class <chr>, qty <dbl>, filled_qty <dbl>,
#> #   filled_avg_price <lgl>, order_class <chr>, order_type <chr>, type <chr>,
#> #   side <chr>, time_in_force <chr>, limit_price <lgl>, stop_price <lgl>,
#> #   status <chr>, extended_hours <lgl>, legs <lgl>
```

All positions are closed using `action = "close_all"`

```r
positions(a = "close_all")
#> No positions are open at this time.
#> list()
```

## Websockets
The package also supports Alpaca's & Polygon's Websockets/Streaming APIs. See the Websockets vignette for more on how to use these features. `vignette("AlpacaforR", "Websockets")`
