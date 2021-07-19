AlpacaforR
================

# `AlpacaforR` 🦙𝘙

This tutorial covers connecting `AlpacaforR` to the [Alpaca
API](https://alpaca.markets) and navigating the package. The [Alpaca API
Docs](https://alpaca.markets/docs/api-documentation/api-v2/) provide a
more general overview of the authentication, API limits, an explanation
of paper & live trading, and release notes. It’s worth checking out. If
you have never heard of Alpaca, you can learn more
[here!](https://alpaca.markets/docs/about-us/) If you want to reference
this material later from within R, you can do so with
`vignette("AlpacaforR", "Getting Started")`

### Release notes

#### 1.0.0 TBD CRAN Release!

#### 0.9.0 2021-05-28 Updates for Alpaca API v2

-   `market_data` now uses the Alpaca version 2 data API with automatic
    pagination support. Retrieving full data sets for 1 minute, 1 hour,
    and 1 day periods should be very quick.
-   The new Alpaca data websockets are supported by `AlpacaStreams`.
    Polygon websocket support is retained, but will no longer be
    developed.
-   Trailing stop loss orders of all types are supported.

#### 0.5.0 2020-05-24 Package Overhaul

-   This release includes an overhaul of the entire package that is
    **not backwards compatible** in some instances, that emphasizes the
    following:
    -   Creates an experience of the package functionality that mirrors
        that of the Alpaca API documentation and provides for more
        intuitive navigation of the package for new users.
    -   Extensive package documentation that links directly to the
        appropriate online documentation wherever necessary with
        attention paid to consistency between the two. Documentation
        inheritance, families, aliases and see also have been
        implemented.
    -   Robust error pre-empting and catching such that meaningful info
        is provided when the function encounters a user error.
    -   Better intention detection based on the combinations of
        arguments entered rather than having to remember multiple
        functions or specify each parameter explicitly. Smoother fuzzy
        detection and autocomplete from RStudio with new consistent
        function names.

#### 0.3.0 2020-03-28 Websockets

-   Add support for Alpaca Websockets

## Installing `Alpacafor` 🦙𝘙

`AlpacaforR` is available on CRAN and can be installed with
`install.packages("AlpacaforR")`. The development version of the package
can be installed from [Github](https:://github.com/yogat3ch/AlpacaforR).

To install the development version,
[devtools](https://cran.r-project.org/web/packages/devtools/readme/README.html)
is required:

``` r
if (!require("devtools")) install.packages("devtools")
```

The `AlpacaforR` 🦙𝘙 dev version can be installed using
`devtools::install_github` with:

``` r
if (!require("AlpacaforR")) {
  devtools::install_github("yogat3ch/AlpacaforR")
}
library(AlpacaforR)
```

## User Keys & URL

### KEY-ID and SECRET-KEY

Connecting to the Alpaca API requires a KEY-ID 🔑 and SECRET-KEY 🗝 as
specifically named environment variables for both live and paper
accounts. These values can be found on the respective Alpaca dashboards.
Hit “Regenerate Key” if the secret key is no longer visible. <span
style="color:blue">*Note*</span> that this will reset your key.

#### The `live` Option

Alpaca provides all users with a paper account. Users in the United
States have the option of creating a live account after verifying their
financial info. In order to simplify working on a particular account for
an extended period of time, `AlpacaforR` (as of 2020-11-09) supports the
usage of an option in the R session.

To set the current session to only use the live account, simply run:

`Sys.setenv("APCA-LIVE" = TRUE)`

The default value will be `FALSE` if no option is set. All functions
will use the paper account when `live = FALSE`. **Read on to learn how
to set this option permanently.**

The simplest way to set these values for this and future R sessions is
to use `AlpacaforR::firstrun` to add these to the `.Renviron` file. If
the `.Renviron` file does not exist it will be created in the R root
folder (found by running `path.expand("~")`).

`firstrun` has arguments: `paper_api`, `live_api`, `polygon_api`, `pro`
and `live`. `paper_api` and `live_api` are named vectors with key &
secret for paper and live accounts respectively, while `polygon_api` is
the secret key for a Polygon account. The `pro` argument specifies
whether an Alpaca Pro subscription is available and `live` argument is a
logical which sets the default value for `live` in future sessions. See
[Live or Paper](#live-or-paper) and for details.

``` r
firstrun(
  paper_api = c(key = "paper-key", secret = "paper-secret"),
  live_api = c(key = "live-key", secret = "live-secret"),
  polygon_api = "polygon-key",
  pro = FALSE,
  live = FALSE
)
```

If using RStudio, these parameters can be added to the `.Renviron` file
another way by typing `usethis::edit_r_environ()` at the console. The
keys are added as `name = key` pairs like so:

    APCA-PAPER-KEY = 'PAPER-KEY'
    APCA-PAPER-SECRET = 'PAPER-SECRET'
    APCA-LIVE-KEY = 'LIVE-KEY'
    APCA-LIVE-SECRET = 'LIVE-SECRET'
    POLYGON-KEY = 'POLYGON-KEY'
    APCA-LIVE = 'FALSE'
    APCA-PRO = 'FALSE'

The following guide details how to [set environment variables
permanently](https://stackoverflow.com/questions/49738564/r-set-environment-variable-permanently)
if you prefer to do this manually via your file system with a text
editor.

Test that these have been properly set by calling:

``` r
Sys.getenv('APCA-PAPER-KEY')
Sys.getenv('APCA-PAPER-SECRET')
Sys.getenv('APCA-LIVE-KEY')
Sys.getenv('APCA-LIVE-SECRET')
Sys.getenv('POLYGON-KEY')
Sys.getenv('APCA-LIVE')
Sys.getenv('APCA-PRO')
```

The output should be the key/secret values entered. The keys can also be
set manually for the session using `Sys.setenv`:

``` r
Sys.setenv('APCA-PAPER-KEY' = "PAPER-KEY")
...
```

Once these environmental variables are set, all `AlpacaforR` 🦙 functions
will work correctly.

> 🛑 User keys & secrets *MUST* be set as the appropriately named
> environment variables above for all demos hereforward to work!

### Live or Paper URL?

[Account
Plans](https://alpaca.markets/docs/trading-on-alpaca/account-plans/)
documents the key differences between the account types. When using
`AlpacaforR`, interaction with the live or paper account is indicated by
setting the `live = TRUE/FALSE` argument. The default is the value of
the `APCA-LIVE` environment variable. E.g:

``` r
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

For live account details set `live = TRUE`

``` r
account(live = TRUE) [-c(1:2)]
```

Not all functions require this since some functions use the same URL
regardless of the account type. These functions are `assets` 💰,
`calendar` 🗓, `clock` ⏰, and `market_data` 📊 where the same URLs are
used for both account types.

# Package Functionality

The functionality in the `AlpacaforR` package maps neatly onto the
endpoints listed in the [API version 2
Documentation](https://alpaca.markets/docs/api-documentation/api-v2/)
for ease of reference. For any function hereforward, you can use
`?function_name` at the console to view the function’s documentation
which will provide a great deal more depth of detail regarding it’s
arguments and what the function returns.

## Account: Retrieve info & change settings for your account

### `account`

Accessing account information is made easy through the `account`
function which will return account details such as account id 🆔,
portfolio value 💲 , buying power 🔌, cash 💵, cash withdrawable 💸, etc.
See `?account` for more details or visit the [Account Endpoint
Docs](https://alpaca.markets/docs/api-documentation/api-v2/account/) to
learn everything there is to know about the requests and responses for
this endpoint.

``` r
account(live = TRUE)
```

### `account_config`

The [Account Configuration
Endpoint](https://alpaca.markets/docs/api-documentation/api-v2/account-configuration/)
supports viewing and setting account configuration details.

Retrieve the account configuration

``` r
account_config()
```

    ## $dtbp_check
    ## [1] "entry"
    ## 
    ## $fractional_trading
    ## [1] TRUE
    ## 
    ## $max_margin_multiplier
    ## [1] "4"
    ## 
    ## $no_shorting
    ## [1] FALSE
    ## 
    ## $pdt_check
    ## [1] "entry"
    ## 
    ## $suspend_trade
    ## [1] FALSE
    ## 
    ## $trade_confirm_email
    ## [1] "all"
    ## 
    ## attr(,"query")
    ## attr(,"query")$status_code
    ## [1] 200
    ## 
    ## attr(,"query")$url
    ## [1] "https://paper-api.alpaca.markets/v2/account/configurations"
    ## 
    ## attr(,"query")$ts
    ## [1] "2021-06-16 11:08:00 EDT"

Change configuration settings as needed.

``` r
# change a configuration: block all orders on the live account
account_config(suspend_trade = T)
```

    ## $dtbp_check
    ## [1] "entry"
    ## 
    ## $fractional_trading
    ## [1] TRUE
    ## 
    ## $max_margin_multiplier
    ## [1] "4"
    ## 
    ## $no_shorting
    ## [1] FALSE
    ## 
    ## $pdt_check
    ## [1] "entry"
    ## 
    ## $suspend_trade
    ## [1] TRUE
    ## 
    ## $trade_confirm_email
    ## [1] "all"
    ## 
    ## attr(,"query")
    ## attr(,"query")$status_code
    ## [1] 200
    ## 
    ## attr(,"query")$url
    ## [1] "https://paper-api.alpaca.markets/v2/account/configurations"
    ## 
    ## attr(,"query")$ts
    ## [1] "2021-06-16 11:08:01 EDT"

Return all settings back to defaults with ease.

``` r
account_config("default")
```

    ## $dtbp_check
    ## [1] "entry"
    ## 
    ## $fractional_trading
    ## [1] TRUE
    ## 
    ## $max_margin_multiplier
    ## [1] "4"
    ## 
    ## $no_shorting
    ## [1] FALSE
    ## 
    ## $pdt_check
    ## [1] "entry"
    ## 
    ## $suspend_trade
    ## [1] FALSE
    ## 
    ## $trade_confirm_email
    ## [1] "all"
    ## 
    ## attr(,"query")
    ## attr(,"query")$status_code
    ## [1] 200
    ## 
    ## attr(,"query")$url
    ## [1] "https://paper-api.alpaca.markets/v2/account/configurations"
    ## 
    ## attr(,"query")$ts
    ## [1] "2021-06-16 11:08:01 EDT"

### `account_activities`

The [Account Activities
Endpoint](https://alpaca.markets/docs/api-documentation/api-v2/account-activities/)
returns all account activities, optionally filtered by type and date
range. This endpoint supports paging - advance pages by providing the
last ID supplied for a given page to `page_token`.

``` r
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

Optionally provide a filter. See for a list of account activity types.

``` r
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

The [Portfolio History
Endpoint](https://alpaca.markets/docs/api-documentation/api-v2/portfolio-history/)
returns a timeseries with equity and profit/loss summary for a period of
time aggregated by the specified `timeframe` (*optional*) or up to a
specific end date `date_end` (*optional*).

To take a look at equity & gain/loss for the paper account over the past
two weeks:

``` r
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

When `AlpacaforR` function arguments are omitted, they will be assumed
with informative messages indicating what values were used for omitted
arguments. In the case above, the most granular `timeframe` allowed for
the period is assumed.

To view the same data with a `timeframe` of hours instead, use the
following:

``` r
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

The [Assets
Endpoint](https://alpaca.markets/docs/api-documentation/api-v2/assets/)
serves as a queryable master list of assets 💰 available for trade and
data consumption from Alpaca. Assets are sorted by asset class, exchange
and symbol. Calling the function without arguments retrieves all assets.
Be forewarned; this takes a while.

``` r
## NOT RUN
assets()
```

Assets can be retrieved by providing:

1.  the asset symbol

``` r
(amzn <- assets("AMZN"))
```

    ## # A tibble: 1 x 11
    ##   id         class exchange symbol name     status tradable marginable shortable
    ##   <chr>      <chr> <chr>    <chr>  <chr>    <chr>  <lgl>    <lgl>      <lgl>    
    ## 1 f801f835-~ us_e~ NASDAQ   AMZN   Amazon.~ active TRUE     TRUE       TRUE     
    ## # ... with 2 more variables: easy_to_borrow <lgl>, fractionable <lgl>

2.  a vector of asset symbols (not case-sensitive)

``` r
assets(c("AMZN", "fb"))
```

    ## # A tibble: 2 x 11
    ##   id        class  exchange symbol name     status tradable marginable shortable
    ##   <chr>     <chr>  <chr>    <chr>  <chr>    <chr>  <lgl>    <lgl>      <lgl>    
    ## 1 f801f835~ us_eq~ NASDAQ   AMZN   Amazon.~ active TRUE     TRUE       TRUE     
    ## 2 fc6a5dcd~ us_eq~ NASDAQ   FB     Faceboo~ active TRUE     TRUE       TRUE     
    ## # ... with 2 more variables: easy_to_borrow <lgl>, fractionable <lgl>

3.  the asset id

``` r
assets(amzn$id)
```

    ## # A tibble: 1 x 11
    ##   id         class exchange symbol name     status tradable marginable shortable
    ##   <chr>      <chr> <chr>    <chr>  <chr>    <chr>  <lgl>    <lgl>      <lgl>    
    ## 1 f801f835-~ us_e~ NASDAQ   AMZN   Amazon.~ active TRUE     TRUE       TRUE     
    ## # ... with 2 more variables: easy_to_borrow <lgl>, fractionable <lgl>

## Calendar: Retrieve a calendar of trading days & times

The [Calendar
Endpoint](https://alpaca.markets/docs/api-documentation/api-v2/calendar/)
serves the full list of market days from 1970 to 2029, bounded by
optional `from` and/or `to` dates. In addition to the market dates, the
response also contains the specific open and close times for the market
days, taking into account early closures. The `calendar` function as of
`AlpacaforR 0.3.0` will return
[intervals](https://lubridate.tidyverse.org/reference/Interval-class.html)
spanning the market `day` and `session` for easily subsetting Date type
vectors, as well as the three letter abbreviation for the day of the
week the date represents. Visit the [Calendar
Endpoint](https://docs.alpaca.markets/api-documentation/web-api/calendar/)
to learn everything there is to know about the requests and responses
for this endpoint.

``` r
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

Subsetting market data using the intervals returned from this function
will be covered in the <a href="#market-data">Market Data</a> section.

#### A Note on Timezones

All Dates/Datetimes input as arguments are forced (See
[`lubridate::force_tz`](https://lubridate.tidyverse.org/reference/force_tz.html))
to America/New York timezone in which the NYSE operates for
`market_data` and `calendar` functions. This means that if
[`lubridate::now`](https://lubridate.tidyverse.org/reference/now.html)
is used to specify 3PM in the local timezone, it will be forced to 3PM
in the "`America/New_York"` timezone. This eliminates the need to
consistently account for timezone conversions when providing inputs to
retrieve historical data using `market_data`.

## Clock: Retrieve current market status and info

The `clock` function accesses the [Clock
endpoint](https://docs.alpaca.markets/api-documentation/web-api/clock/),
used to gain an understanding of how the local time compares to
“America/New\_York.” A timezone can be specified to the `tz` argument to
determine how the market hours compare to the specified timezone hours.
If no `tz` argument is provided, and the local timezone differs from
“America/New\_York”, `clock` will automatically provide the local
conversion and offset.

``` r
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

The `watchlist` function accesses all [Watchlist
Endpoints](https://alpaca.markets/docs/api-documentation/api-v2/watchlist/).
An account can have multiple watchlists and each is uniquely identified
by id but can also be addressed by a user-defined name. Each watchlist
is an ordered list of assets.

The current watchlists can be retrieved by calling `watchlist` without
arguments:

``` r
watchlist()
```

To start, create a watchlist named `test` with Apple by specifying `"c"`
for create as the `action`

``` r
(wl <- watchlist("test", symbols = "AAPL", action = "c"))
```

    ##   symbol                                   id     class exchange
    ## 1   AAPL b0b6dd9d-8b9b-48a9-ba46-b9d54906e415 us_equity   NASDAQ
    ##                      name status tradable marginable shortable easy_to_borrow
    ## 1 Apple Inc. Common Stock active     TRUE       TRUE      TRUE           TRUE
    ##   fractionable
    ## 1         TRUE

Watchlists can be retrieved by the user provided name

``` r
(test <- watchlist("test"))
```

    ##   symbol                                   id     class exchange
    ## 1   AAPL b0b6dd9d-8b9b-48a9-ba46-b9d54906e415 us_equity   NASDAQ
    ##                      name status tradable marginable shortable easy_to_borrow
    ## 1 Apple Inc. Common Stock active     TRUE       TRUE      TRUE           TRUE
    ##   fractionable
    ## 1         TRUE

``` r
all.equal(test, wl, check.attributes = FALSE)
```

    ## [1] TRUE

Each watchlist `tibble` has an `info` attribute that stores details like
when it was created, lasted updated and more.

``` r
# Get it's info
attr(test, "info")
```

    ## $id
    ## [1] "dc9717bd-fcda-4325-9674-2803b91496c0"
    ## 
    ## $account_id
    ## [1] "0ef8f355-319d-4de7-8d32-03f1e7e5aa69"
    ## 
    ## $created_at
    ## [1] "2021-06-16 11:08:04 EDT"
    ## 
    ## $updated_at
    ## [1] "2021-06-16 11:08:04 EDT"
    ## 
    ## $name
    ## [1] "test"

Add FB, AMZN, NFLX, GOOG and update the watchlist name to FAANG. The
default for `action` when a `new_name` is specified is to `a`dd new
symbols when changing the name. Similarly, if just a `new_name` is
provided, the existing `symbols` will be preserved.

``` r
(wl <- watchlist("test", new_name = "FAANG", symbols = c("FB", "AMZN", "NFLX", "GOOG")))
```

    ##   symbol                                   id     class exchange
    ## 1   AAPL b0b6dd9d-8b9b-48a9-ba46-b9d54906e415 us_equity   NASDAQ
    ## 2     FB fc6a5dcd-4a70-4b8d-b64f-d83a6dae9ba4 us_equity   NASDAQ
    ## 3   AMZN f801f835-bfe6-4a9d-a6b1-ccbb84bfd75f us_equity   NASDAQ
    ## 4   NFLX bb2a26c0-4c77-4801-8afc-82e8142ac7b8 us_equity   NASDAQ
    ## 5   GOOG f30d734c-2806-4d0d-b145-f9fade61432b us_equity   NASDAQ
    ##                                  name status tradable marginable shortable
    ## 1             Apple Inc. Common Stock active     TRUE       TRUE      TRUE
    ## 2 Facebook, Inc. Class A Common Stock active     TRUE       TRUE      TRUE
    ## 3       Amazon.com, Inc. Common Stock active     TRUE       TRUE      TRUE
    ## 4          Netflix, Inc. Common Stock active     TRUE       TRUE      TRUE
    ## 5 Alphabet Inc. Class C Capital Stock active     TRUE       TRUE      TRUE
    ##   easy_to_borrow fractionable
    ## 1           TRUE         TRUE
    ## 2           TRUE         TRUE
    ## 3           TRUE         TRUE
    ## 4           TRUE         TRUE
    ## 5           TRUE         TRUE

Individual assets can be added to or deleted from watchlists using
`action = "add"` or `"delete"` respectively (`"a"`/`"d"` for short).

``` r
(wl <- watchlist("FAANG", symbol = "GOOGL"))
```

    ##   symbol                                   id     class exchange
    ## 1   AAPL b0b6dd9d-8b9b-48a9-ba46-b9d54906e415 us_equity   NASDAQ
    ## 2     FB fc6a5dcd-4a70-4b8d-b64f-d83a6dae9ba4 us_equity   NASDAQ
    ## 3   AMZN f801f835-bfe6-4a9d-a6b1-ccbb84bfd75f us_equity   NASDAQ
    ## 4   NFLX bb2a26c0-4c77-4801-8afc-82e8142ac7b8 us_equity   NASDAQ
    ## 5   GOOG f30d734c-2806-4d0d-b145-f9fade61432b us_equity   NASDAQ
    ## 6  GOOGL 69b15845-7c63-4586-b274-1cfdfe9df3d8 us_equity   NASDAQ
    ##                                  name status tradable marginable shortable
    ## 1             Apple Inc. Common Stock active     TRUE       TRUE      TRUE
    ## 2 Facebook, Inc. Class A Common Stock active     TRUE       TRUE      TRUE
    ## 3       Amazon.com, Inc. Common Stock active     TRUE       TRUE      TRUE
    ## 4          Netflix, Inc. Common Stock active     TRUE       TRUE      TRUE
    ## 5 Alphabet Inc. Class C Capital Stock active     TRUE       TRUE      TRUE
    ## 6  Alphabet Inc. Class A Common Stock active     TRUE       TRUE      TRUE
    ##   easy_to_borrow fractionable
    ## 1           TRUE         TRUE
    ## 2           TRUE         TRUE
    ## 3           TRUE         TRUE
    ## 4           TRUE         TRUE
    ## 5           TRUE         TRUE
    ## 6           TRUE         TRUE

``` r
(wl <- watchlist("FAANG", action = "d", symbols = "GOOGL"))
```

    ##   symbol                                   id     class exchange
    ## 1   AAPL b0b6dd9d-8b9b-48a9-ba46-b9d54906e415 us_equity   NASDAQ
    ## 2     FB fc6a5dcd-4a70-4b8d-b64f-d83a6dae9ba4 us_equity   NASDAQ
    ## 3   AMZN f801f835-bfe6-4a9d-a6b1-ccbb84bfd75f us_equity   NASDAQ
    ## 4   NFLX bb2a26c0-4c77-4801-8afc-82e8142ac7b8 us_equity   NASDAQ
    ## 5   GOOG f30d734c-2806-4d0d-b145-f9fade61432b us_equity   NASDAQ
    ##                                  name status tradable marginable shortable
    ## 1             Apple Inc. Common Stock active     TRUE       TRUE      TRUE
    ## 2 Facebook, Inc. Class A Common Stock active     TRUE       TRUE      TRUE
    ## 3       Amazon.com, Inc. Common Stock active     TRUE       TRUE      TRUE
    ## 4          Netflix, Inc. Common Stock active     TRUE       TRUE      TRUE
    ## 5 Alphabet Inc. Class C Capital Stock active     TRUE       TRUE      TRUE
    ##   easy_to_borrow fractionable
    ## 1           TRUE         TRUE
    ## 2           TRUE         TRUE
    ## 3           TRUE         TRUE
    ## 4           TRUE         TRUE
    ## 5           TRUE         TRUE

To replace all the symbols in a watchlist while renaming, specify
`action = "update"` or `"u"` for short.

``` r
(wl <- watchlist("FAANG", new_name = "FANG", symbols = c("FB", "AAPL", "NFLX", "GOOG"), action = "u"))
```

    ##   symbol                                   id     class exchange
    ## 1     FB fc6a5dcd-4a70-4b8d-b64f-d83a6dae9ba4 us_equity   NASDAQ
    ## 2   AAPL b0b6dd9d-8b9b-48a9-ba46-b9d54906e415 us_equity   NASDAQ
    ## 3   NFLX bb2a26c0-4c77-4801-8afc-82e8142ac7b8 us_equity   NASDAQ
    ## 4   GOOG f30d734c-2806-4d0d-b145-f9fade61432b us_equity   NASDAQ
    ##                                  name status tradable marginable shortable
    ## 1 Facebook, Inc. Class A Common Stock active     TRUE       TRUE      TRUE
    ## 2             Apple Inc. Common Stock active     TRUE       TRUE      TRUE
    ## 3          Netflix, Inc. Common Stock active     TRUE       TRUE      TRUE
    ## 4 Alphabet Inc. Class C Capital Stock active     TRUE       TRUE      TRUE
    ##   easy_to_borrow fractionable
    ## 1           TRUE         TRUE
    ## 2           TRUE         TRUE
    ## 3           TRUE         TRUE
    ## 4           TRUE         TRUE

Delete the watchlist to start fresh.

``` r
watchlist("FANG", a = "d")
```

    ## Watchlist deleted successfully

    ## data frame with 0 columns and 0 rows

## Market Data

The `market_data` function is designed to access market & pricing data 📈
provided by Alpaca or Polygon. Alpaca now provides data via the [API
version 1 Market Data
Endpoint](https://alpaca.markets/docs/api-documentation/api-v2/market-data/alpaca-data-api-v1/)
& the [API version 2 Market Data
Endpoint](https://alpaca.markets/docs/api-documentation/api-v2/market-data/alpaca-data-api-v2/historical/).
Data is also provided from [Polygon’s Aggregates
Endpoint](https://polygon.io/docs/#get_v2_aggs_ticker__ticker__range__multiplier___timespan___from___to__anchor)
with a valid `POLYGON-KEY`. Choose the API via the `v` parameter:

1.  `v = 1` for Alpaca’s v1 API.
2.  `v = 2` for Alpaca’s v2 API (default).
3.  `v = "p"` for the Polygon API.

The Alpaca v1 Data API consolidates data sources from five different
exchanges.

-   IEX (Investors Exchange LLC)
-   NYSE National, Inc.
-   Nasdaq BX, Inc.
-   Nasdaq PSX
-   NYSE Chicago, Inc.

The v2 API uses solely IEX, but provides more complete data for requests
that exceed the `limit` via pagination.

Data is returned as a list of `tsymble`s (one for each symbol provided
to `ticker`) in OHLCV format 📊. *Note:* the Polygon API returns `vw`,
the weighted volume, in addition to the raw volume. It also returns `n`
which indicates the number of datapoints aggregated to calculate the
value for the particular timeframe. (See the \[endpoint docs for
details\]\[Polygon’s Aggregates
Endpoint\](<https://polygon.io/docs/#get_v2_aggs_ticker__ticker__range__multiplier___timespan___from___to__anchor>)).

A `tsymble` is an S3 object with the symbol name as an attribute and
query info that can be retrieved like so

``` r
md <- market_data("AMZN", from = "2021-05-25", to = "2021-05-27")
```

    ## `limit` coerced to 10000

``` r
class(md)
```

    ## [1] "tsymble_ts" "tbl_ts"     "tbl_df"     "tbl"        "data.frame"

``` r
get_sym(md)
```

    ## [1] "AMZN"

``` r
get_query(md)
```

    ## $status_code
    ## [1] 200
    ## 
    ## $url
    ## [1] "https://data.alpaca.markets/v2/stocks/AMZN/bars?start=2021-05-25T00%3A00%3A00-00%3A00&end=2021-05-27T00%3A00%3A00-00%3A00&limit=10000&timeframe=1Day"
    ## 
    ## $ts
    ## [1] "2021-06-16 11:08:08 EDT"
    ## 
    ## $symbol
    ## [1] "AMZN"
    ## 
    ## $next_page_token
    ## NULL

The only required input is the symbol(s) as a character vector, and it
will return pricing data for the last day (if it’s a trading day) by
day.

``` r
market_data("AMZN")
```

    ## `from` omitted. Set to 2021-06-15

    ## `to` omitted. Set to 2021-06-17

    ## `limit` coerced to 10000

    ## # A tsibble: 1 x 6 [1D]
    ##   time        open  high   low close  volume
    ##   <date>     <dbl> <dbl> <dbl> <dbl>   <dbl>
    ## 1 2021-06-15 3383. 3397. 3363. 3383. 2424699

The function accepts different sets of optional arguments depending on
whether the Alpaca v1 API (`v=1`) v2 API (`v=2`) or Polygon Aggregates
API (`v="p"`) is used, see `?market_data` for full details on which
arguments are used with each respective API.

To specify a date range to the v1 API, the `from`, `to` / `after`,
`until` arguments can be used. These are inclusive/exclusive date bounds
respectively. Here, hourly data for the first seven days of January 2020
is retrieved inclusive:

``` r
market_data("amzn", v = 1, from = "2020-01-01", to = "2020-01-07")
#> 'limit' coerced to 1000
#> $AMZN
#>         time    open     high      low   close  volume
#> 1 2020-01-02 1874.79 1898.000 1864.150 1897.71 3583611
#> 2 2020-01-03 1864.50 1886.197 1864.500 1874.93 3293469
#> 3 2020-01-06 1860.00 1903.690 1860.000 1903.33 3598872
#> 4 2020-01-07 1904.50 1913.890 1892.043 1906.86 3569706
```

`after` and `until` can be used when `v = 1` to make exclusive date
bounds.

``` r
market_data("amzn", after = "2020-01-02", until = "2020-01-07")
#> $AMZN
#>         time   open     high    low   close  volume
#> 1 2020-01-03 1864.5 1886.197 1864.5 1874.93 3293469
#> 2 2020-01-06 1860.0 1903.690 1860.0 1903.33 3598872
```

The v2 & Polygon APIs do not have exclusive date bound options, if
`after/until` are used for these APIs they are considered `from`/`to`
inclusive when sent to the API.

### Arguments to `market_data` with the V1 API

The options for the `timeframe` argument using the `v = 1` API include:

-   `"m"`, `"min"`, `"minute"`
-   `"d"`, `"day"` (the default)

When using a minute `timeframe`, the `multiplier` can by `1`, `5`, or
`15` whereas when using `timeframe = "day"` the only multiplier
available is `1`. The bar `limit` argument can range from `1` to `1000`
and has various default values according to the timeframe chosen. If
left blank, the `limit` will default to `1000`. If the date range
includes more than 1000 bars and `full = FALSE`, then the API will
return the 1000 most recent bars.

The v1 data API has two endpoints for retrieving the most recent quote
and trade data which are accessed by setting `timeframe` to
`"q", "qu", "quote", "lq", "last_quote"` or
`"t","tr","trade", "lt","last_trade"` respectively.

### Arguments to `market_data` for the V2 API

The v2 API offers three timeframes, each with the default multiplier of
one:

-   `"m"`, `"min"`, `"minute"`
-   `"h"`, `"hour"`
-   `"d"`, `"day"` (the default)

#### Additional endpoints

The V2 API also offers quote, trade and snapshot endpoints that retrieve
quote and trade data for a given time period or a snapshot for a given
time period. <span style="color:red">**WARNING**</span> These endpoints
return an enormous amount of data for each day. For example: a request
spanning a single day (ie 5/26-5/27) can take \~ 3m to retrieve. Use
`timeframe`:

-   `'tr'/'trade'` For historical trade data for a given ticker symbol
    on a specified date. See
    [Trades](https://alpaca.markets/docs/api-documentation/api-v2/market-data/alpaca-data-api-v2/historical/#trades).

``` r
market_data("BYND", timeframe = "t", from = "2021-06-09")
```

    ## `to` omitted. Set to 2021-06-10

    ## `limit` coerced to 10000

    ## 2345Binding 6 pages...

    ## # A tsibble: 32,935 x 7 [0.0149021014775059us] <America/New_York>
    ##    time                x         p     s c             i z    
    ##    <dttm>              <chr> <dbl> <int> <list>    <int> <chr>
    ##  1 2021-06-09 04:00:05 P      153.     2 <chr [4]>     1 C    
    ##  2 2021-06-09 04:03:05 Q      153.     2 <chr [4]>     1 C    
    ##  3 2021-06-09 04:03:28 P      153.     1 <chr [4]>     2 C    
    ##  4 2021-06-09 04:08:44 Q      153.     5 <chr [4]>     2 C    
    ##  5 2021-06-09 04:12:36 Q      153.     7 <chr [3]>     3 C    
    ##  6 2021-06-09 04:14:40 Q      153.    18 <chr [4]>     5 C    
    ##  7 2021-06-09 04:15:37 K      153.     6 <chr [3]>     1 C    
    ##  8 2021-06-09 04:15:43 Q      153.     6 <chr [3]>     6 C    
    ##  9 2021-06-09 04:16:25 Q      153.     1 <chr [3]>     7 C    
    ## 10 2021-06-09 04:17:11 P      153.    25 <chr [3]>     3 C    
    ## # ... with 32,925 more rows

-   `'qu'/'quote'` For NBBO quotes for a given ticker symbol at a
    specified date. See
    [Quotes](https://alpaca.markets/docs/api-documentation/api-v2/market-data/alpaca-data-api-v2/historical/#quotes)

``` r
market_data("BYND", timeframe = "q", from = "2021-06-09")
```

    ## `to` omitted. Set to 2021-06-10

    ## `limit` coerced to 10000

    ## 2345678Binding 9 pages...

    ## # A tsibble: 77,708 x 8 [0.99999999999987us] <America/New_York>
    ##    time                ax       ap    as bx       bp    bs c        
    ##    <dttm>              <chr> <dbl> <int> <chr> <dbl> <int> <list>   
    ##  1 2021-06-09 04:00:00 P       177    10 " "     0       0 <chr [1]>
    ##  2 2021-06-09 04:00:00 P       177    10 "P"    90.6     1 <chr [1]>
    ##  3 2021-06-09 04:00:00 P       177    10 "P"   110       1 <chr [1]>
    ##  4 2021-06-09 04:00:00 P       177    10 "P"   120.      1 <chr [1]>
    ##  5 2021-06-09 04:00:00 P       175     1 "P"   120.      1 <chr [1]>
    ##  6 2021-06-09 04:00:00 P       170     1 "P"   120.      1 <chr [1]>
    ##  7 2021-06-09 04:00:00 P       170     1 "P"   120       1 <chr [1]>
    ##  8 2021-06-09 04:00:00 P       168     1 "P"   120       1 <chr [1]>
    ##  9 2021-06-09 04:00:00 P       160     1 "P"   120       1 <chr [1]>
    ## 10 2021-06-09 04:00:00 P       158     1 "P"   120       1 <chr [1]>
    ## # ... with 77,698 more rows

-   `'ss'/'snapshot'` The V2 API also offers a `snapshot` endpoint that
    provides the latest trade, latest quote, minute bar daily bar and
    previous daily bar data for a given ticker symbol or symbols.

``` r
market_data(c("BYND", "VEGN"), timeframe = "ss")
```

    ## `from` omitted. Set to 2021-06-15

    ## `to` omitted. Set to 2021-06-17

    ## `limit` coerced to 10000

    ## $BYND
    ## $BYND$latestTrade
    ## # A tibble: 1 x 7
    ##   time                x         p     s c         i z    
    ##   <dttm>              <chr> <dbl> <int> <chr> <int> <chr>
    ## 1 2021-06-16 11:09:00 V      149.   100 @       719 C    
    ## 
    ## $BYND$latestQuote
    ## # A tibble: 1 x 8
    ##   time                ax       ap    as bx       bp    bs c    
    ##   <dttm>              <chr> <int> <int> <chr> <dbl> <int> <chr>
    ## 1 2021-06-16 11:10:00 V       170     2 V      143.     1 R    
    ## 
    ## $BYND$minuteBar
    ## # A tibble: 1 x 6
    ##   time                 open  high   low close volume
    ##   <dttm>              <dbl> <dbl> <dbl> <dbl>  <dbl>
    ## 1 2021-06-16 11:08:00  149.  149.  149.  149.    291
    ## 
    ## $BYND$dailyBar
    ## # A tibble: 1 x 6
    ##   time                 open  high   low close volume
    ##   <dttm>              <dbl> <dbl> <dbl> <dbl>  <dbl>
    ## 1 2021-06-16 00:00:00  146.  151.  145.  149.  42979
    ## 
    ## $BYND$prevDailyBar
    ## # A tibble: 1 x 6
    ##   time                 open  high   low close volume
    ##   <dttm>              <dbl> <dbl> <dbl> <dbl>  <dbl>
    ## 1 2021-06-15 00:00:00  150.  151.  143.  145. 123261
    ## 
    ## 
    ## $VEGN
    ## $VEGN$latestTrade
    ## # A tibble: 1 x 7
    ##   time                x         p     s c           i z    
    ##   <dttm>              <chr> <dbl> <int> <chr>   <dbl> <chr>
    ## 1 2021-06-07 15:02:00 V      38.2   100 " "   5.53e13 B    
    ## 
    ## $VEGN$latestQuote
    ## # A tibble: 1 x 8
    ##   time                ax       ap    as bx       bp    bs c    
    ##   <dttm>              <chr> <int> <int> <chr> <int> <int> <chr>
    ## 1 2021-06-15 16:00:00 V         0     0 V         0     0 R    
    ## 
    ## $VEGN$minuteBar
    ## # A tibble: 1 x 6
    ##   time                 open  high   low close volume
    ##   <dttm>              <dbl> <dbl> <dbl> <dbl>  <dbl>
    ## 1 2021-06-07 15:02:00  38.2  38.2  38.2  38.2    100
    ## 
    ## $VEGN$dailyBar
    ## # A tibble: 1 x 6
    ##   time                 open  high   low close volume
    ##   <dttm>              <dbl> <dbl> <dbl> <dbl>  <dbl>
    ## 1 2021-06-07 00:00:00  38.2  38.2  38.2  38.2    100
    ## 
    ## $VEGN$prevDailyBar
    ## # A tibble: 1 x 6
    ##   time                 open  high   low close volume
    ##   <dttm>              <dbl> <dbl> <dbl> <dbl>  <dbl>
    ## 1 2021-04-30 00:00:00  38.2  38.2  38.1  38.1    985
    ## 
    ## 
    ## attr(,"query")
    ## attr(,"query")$status_code
    ## [1] 200
    ## 
    ## attr(,"query")$url
    ## [1] "https://data.alpaca.markets/v2/stocks/snapshots?symbols=BYND%2CVEGN"
    ## 
    ## attr(,"query")$ts
    ## [1] "2021-06-16 11:09:38 EDT"

### Arguments to `market_data` for the Polygon API

The [Polygon API Aggregates
Endpoint](https://polygon.io/docs/#get_v2_aggs_ticker__ticker__range__multiplier___timespan___from___to__anchor)
is called when parameter `v = "p"`, Additional arguments are
well-documented in the help file (see `?market_data`). Note that the
Polygon API does not have a `limit` argument but has an implicit limit
of 50000 data points computed on the API end for which it is not easy to
predict the data that will be returned. If the range of times requested
from the API exceed what can be returned in a single call, the API will
generally return the data from the initial segment of the timeframe,
with a large gap, followed by the last few bars of data or it will
return the most recent data until the limit is reached leaving off the
oldest data. This behavior can be witnessed when `full = F` (the
default). This behavior is what inspired the development of the
`full = T` feature.

When `full =  T` (for both Alpaca v1 & Polygon APIs) the function will
attempt to anticipate what data is expected based on the range of dates
requested, and will re-query the API as many times as necessary to fill
the request. Any remaining gaps will be filled with `NA` values,
allowing for omission or imputation of missing data as needed. If the
API is queried with the default `full = F` and upon inspection, large
gaps are found in the data, try setting `full = T`. If any issues arise,
please submit an [issue](https://github.com/yogat3ch/AlpacaforR/issues).
<span style="color:blue">*Note*</span> Free accounts for the Polygon API
are limited to five requests per minute. If the rate limit is reached, a
cooldown timer of 60s will be triggered before the next query is sent -
be forewarned that this can result in long retrieval times for large
queries.

#### Additional details on the Polygon Aggregates Endpoint

For a great primer on how the Polygon Aggregates Endpoint works, check
out [this article](https://polygon.io/blog/aggs-api-updates/) from the
Polygon blog. The Polygon API allows for the following timeframes:

-   `'m'`/`'min'`/`'minute'`
-   `'h'`/`'hour'`
-   `'d'`/`'day'`
-   `'w'`/`'week'`
-   `'M'`/`'mo'`/`'month'` (<span style="color:blue">*Note*</span>
    capitalized M for month if using single letter abbreviation)
-   `'q'`/`'quarter'`
-   `'y'`/`'year'`

Any integer can be supplied as the `multiplier` argument, however,
atypical numbers can return unexpected results. The following
combinations of `multiplier` and `timeframe` values have been
systematically tested and prove to return expected data reliably:

-   `'m'`: `1`, `5`, `15`
-   `'h'`: `1`
-   `'d'`: `1`
-   `'w'`: `1`, `2`, `4`
-   `'M'`: `1`, `2`, `3`
-   `'q'`: `1`
-   `'y'`: `1`

*Note:* With `multiplier` greater than one, based on numerous trials for
the various timeframes it appears that the Polygon API takes the nearest
floor (previous) date based on the timeframe prior to the `from` date
and begins providing data on the date that is `multiplier * timeframe`
later. For example, with the week timeframe the API will determine the
floor (previous) Sunday relative to the `from` date and start on the
Sunday `multiplier *` weeks from that floor Sunday.

### Minutes

When `timeframe = "minute"` the API will return data for the entire
session of each trading day beginning in pre-market hours at 4AM
(Polygon) or 7AM (Alpaca) and concluding in after-market hours between
7PM (Alpaca) & 9PM (Polygon), however, the data outside of standard
trading hours has unexpected gaps at a higher frequency than that of
data for market hours 9:30A - 4:30P.

``` r
(bynd <- market_data("BYND", v = 2, time = "m", from = "2021-06-09"))
```

    ## `to` omitted. Set to 2021-06-10 04:00:00

    ## `limit` coerced to 10000

    ## # A tsibble: 428 x 6 [1m] <America/New_York>
    ##    time                 open  high   low close volume
    ##    <dttm>              <dbl> <dbl> <dbl> <dbl>  <dbl>
    ##  1 2021-06-09 07:02:00  153.  154.  153.  154.    490
    ##  2 2021-06-09 07:09:00  154.  154.  154.  154.    100
    ##  3 2021-06-09 07:22:00  154   154   154   154     334
    ##  4 2021-06-09 07:23:00  154.  154.  154.  154.    372
    ##  5 2021-06-09 08:01:00  154.  154.  154.  154.    200
    ##  6 2021-06-09 08:12:00  154.  154.  154.  154.    154
    ##  7 2021-06-09 08:14:00  154.  154.  154.  154.    120
    ##  8 2021-06-09 08:17:00  154.  154.  154.  154.   4202
    ##  9 2021-06-09 08:18:00  154.  155.  154.  155.   5002
    ## 10 2021-06-09 08:19:00  155.  155.  155.  155.    222
    ## # ... with 418 more rows

The returned data demonstrates how pre-market and after-market hours
will tend to have gaps.

This can be illustrated by first retrieving the session hours for the
day:

``` r
d <- "2021-06-09"
(cal <- calendar(from = d, to  = d))
```

    ##         date  open close session_open session_close
    ## 1 2021-06-09 09:30 16:00        07:00         19:00
    ##                                                day
    ## 1 2021-06-09 09:30:00 EDT--2021-06-09 16:00:00 EDT
    ##                                            session dow
    ## 1 2021-06-09 07:00:00 EDT--2021-06-09 19:00:00 EDT Wed

and subsetting the typical trading day hours and those outside:

``` r
trading_hours <- bynd %>% 
  filter(lubridate::`%within%`(time, cal$day))
nontrading_hours <- bynd %>% 
  filter(!lubridate::`%within%`(time, cal$day))
```

We can the examine the gaps between time points by making a frequency
table of the time differences between time points in market and non
market hours. The name of each frequency in the table is the number of
minutes of the gap while the value is the frequency of the gaps’
occurrence as a decimal.

Trading hours:

``` r
prop.table(table(diff(trading_hours$time)))
```

    ## 
    ##           1           2           3 
    ## 0.994832041 0.002583979 0.002583979

Non-trading hours:

``` r
prop.table(table(diff(nontrading_hours$time)))
```

    ## 
    ##          1          2          3          4          5          6          7 
    ## 0.12820513 0.15384615 0.05128205 0.07692308 0.02564103 0.05128205 0.02564103 
    ##          8          9         11         12         13         14         18 
    ## 0.02564103 0.07692308 0.02564103 0.02564103 0.10256410 0.02564103 0.02564103 
    ##         19         22         31         32         35         38        394 
    ## 0.02564103 0.02564103 0.02564103 0.02564103 0.02564103 0.02564103 0.02564103

### Hours

Hours will span 4A (Polygon) 7A (Alpaca) to 9P (Polygon) or 7P (Alpaca)
for each trading day. Since this is an aggregate of minute timeframes,
most data will be returned with few, if any gaps, unless the range
requested exceeds the API limit. The `v=2` API is the best source for
this data.

``` r
market_data("BYND", v = 2, time = "h", m = 1, from = "2020-05-01", to = "2020-05-02")
```

    ## to is a Saturday, Calendar API will return data for the previous Friday or following Monday

    ## `limit` coerced to 10000

    ## # A tsibble: 16 x 6 [1h] <America/New_York>
    ##    time                 open  high   low close  volume
    ##    <dttm>              <dbl> <dbl> <dbl> <dbl>   <dbl>
    ##  1 2020-05-01 04:00:00  96.5  96.5  93.1  93.8    1702
    ##  2 2020-05-01 05:00:00  94.0  94.0  93.8  93.8    8924
    ##  3 2020-05-01 06:00:00  93.8  94.1  92    93      6198
    ##  4 2020-05-01 07:00:00  93.9  94.6  93    94.2   20012
    ##  5 2020-05-01 08:00:00  93.9  94.8  93.5  94.5   44637
    ##  6 2020-05-01 09:00:00  94.8  97.7  94    97.6 1251479
    ##  7 2020-05-01 10:00:00  97.7  98.7  95.2  95.8  791669
    ##  8 2020-05-01 11:00:00  95.8  95.8  91.3  92.3 1183098
    ##  9 2020-05-01 12:00:00  92.3  92.5  90.7  91.3  649933
    ## 10 2020-05-01 13:00:00  91.4  91.8  90.1  91.6  485007
    ## 11 2020-05-01 14:00:00  91.6  94.9  91.2  93.8  644146
    ## 12 2020-05-01 15:00:00  93.7  94.0  89.9  91.4 1361661
    ## 13 2020-05-01 16:00:00  91.5  92.0  91    91.5   71471
    ## 14 2020-05-01 17:00:00  91.5  91.6  91    91.2    9770
    ## 15 2020-05-01 18:00:00  91.1  91.2  90    90.6    5906
    ## 16 2020-05-01 19:00:00  90.6  91.2  90.6  91.2    4361

### Days

Days will span all trading days (generally M-F). `calendar` or the
`polygon` “Market Holidays” endpoint can be consulted to find
exceptions) for each week. Remember that the `from`/`to` arguments
accept Date objects as well as character objects. Any API version can be
used to retrieve day data.

``` r
market_data("BYND", v = 2, time = "d", m = 1, from = lubridate::as_date(d) - lubridate::weeks(1), to = lubridate::as_date(d))
```

    ## `limit` coerced to 10000

    ## # A tsibble: 5 x 6 [1D]
    ##   time        open  high   low close  volume
    ##   <date>     <dbl> <dbl> <dbl> <dbl>   <dbl>
    ## 1 2021-06-02  135.  151.  135.  150. 9487590
    ## 2 2021-06-03  145.  150.  140.  143. 4861363
    ## 3 2021-06-04  144.  148.  142   146. 3026297
    ## 4 2021-06-07  148.  156.  146.  152. 6741454
    ## 5 2021-06-08  152.  157.  147.  153. 4085677

### Weeks

> For all timeframes weeks and above, the polygon API must be used.

Weeks will be aggregated from days for the week following each Sunday.
The date of the Sunday will correspond to all data for the following
trading week. The following returns weekly data for each week that has
passed since the turn of the last quarter.

``` r
market_data("BYND", v = "p", time = "w", m = 1, from = lubridate::floor_date(lubridate::as_date(d), "quarter"))
```

    ## `to` omitted. Set to 2021-04-05

    ## `limit` coerced to 50000

    ## # A tsibble: 2 x 8 [1W]
    ##       time  open  high   low close   volume      n    vw
    ##     <week> <dbl> <dbl> <dbl> <dbl>    <dbl>  <dbl> <dbl>
    ## 1 2021 W12  130.  135.  124.  131.  9066535 151547  129.
    ## 2 2021 W13  132.  141.  130.  130. 14186337 220190  134.

### Months

Months are aggregated by day for the entire month. The day represented
in the time series varies based on the dates requested. Based on various
inputs, the day might be the 30th, the 1st, or the 23rd of the month.
However, if the request spans February, it could give the 30th of the
months preceding February and the 1st for February and the months
following. It’s unclear whether the data aggregated on a day for that
month corresponds to all the days in that month, or all the days between
that day in one month and that day in the previous month.

``` r
market_data("BYND", v = "p", time = "M", m = 1, from = lubridate::floor_date(lubridate::as_date(d), "year"), to = lubridate::as_date(d))
```

    ## `limit` coerced to 50000

    ## # A tsibble: 6 x 8 [1M]
    ##       time  open  high   low close    volume       n    vw
    ##      <mth> <dbl> <dbl> <dbl> <dbl>     <dbl>   <dbl> <dbl>
    ## 1 2021 Jan  126.  221  114.   178. 132218535 1795635  158.
    ## 2 2021 Feb  180.  184. 139.   145.  60570731  938746  163.
    ## 3 2021 Mar  150   155  124.   130.  72302298 1104803  138.
    ## 4 2021 Apr  132.  141. 127.   132.  44340921  729995  134.
    ## 5 2021 May  131.  154.  99.9  145. 114468101 1627645  123.
    ## 6 2021 Jun  143.  157. 132.   149.  51131411  774496  146.

### Quarters

Quarters will be represented by the following dates for each year:

-   Q1: 03-30
-   Q2: 06-30
-   Q3: 09-30
-   Q4: 12-30

``` r
market_data("BYND", v = "p", time = "q", m = 1, from = lubridate::floor_date(lubridate::as_date(d), "year"))
```

    ## `to` omitted. Set to 2021-04-02 07:30:00

    ## `limit` coerced to 50000

    ## # A tsibble: 2 x 8 [1Q]
    ##      time  open  high   low close    volume       n    vw
    ##     <qtr> <dbl> <dbl> <dbl> <dbl>     <dbl>   <dbl> <dbl>
    ## 1 2021 Q1  126.  221  114.   130. 265091564 3839184  154.
    ## 2 2021 Q2  132.  157.  99.9  149. 209940433 3132136  131.

### Year

Years are aggregated on 12-31 of each year.

``` r
market_data("BYND", v = "p", time = "y", m = 1, from = lubridate::floor_date(lubridate::as_date(d), "year") - lubridate::years(4), to = d)
```

    ## `limit` coerced to 50000

    ## # A tsibble: 3 x 8 [1Y]
    ##    time  open  high   low close     volume        n    vw
    ##   <dbl> <dbl> <dbl> <dbl> <dbl>      <dbl>    <dbl> <dbl>
    ## 1  2019  46    240.  45    75.6 1012290571  9478751  127.
    ## 2  2020  76.2  198.  48.2 125   1746648233 18916299  124.
    ## 3  2021 126.   221   99.9 149.   475031997  6971320  144.

### Using `full = TRUE`

Due to the API limits, the returned dataset may be missing substantial
amounts of data. This feature was developed to fetch complete datasets
before the V2 API was released. The V2 API now supports pagination and
`AlpacaforR` will automatically fetch all pages associated with a
request. Using the V2 API is recommended for fetching large datasets.

The `full` argument can be used to fetch full datasets with the V1 API
that has a `limit` of 1000 bars, .

``` r
fr <- "2021-01-01"
to <- "2021-06-01"
(bars <- market_data("BYND", v = 1, time = "m", m = 5, from = fr, to = to))
```

    ## `limit` coerced to 1000

    ## # A tsibble: 1,000 x 6 [5m] <America/New_York>
    ##    time                 open  high   low close volume
    ##    <dttm>              <dbl> <dbl> <dbl> <dbl>  <dbl>
    ##  1 2021-05-12 09:50:00  110.  111.  109.  109.   4119
    ##  2 2021-05-12 09:55:00  109.  109.  107.  107.  10557
    ##  3 2021-05-12 10:00:00  107   108.  107   107.   8889
    ##  4 2021-05-12 10:05:00  107.  108.  107.  107.   3408
    ##  5 2021-05-12 10:10:00  107.  107.  107.  107.   2009
    ##  6 2021-05-12 10:15:00  107.  107.  107.  107.   1210
    ##  7 2021-05-12 10:20:00  107.  108.  107.  107.   1680
    ##  8 2021-05-12 10:25:00  107.  107.  107.  107.   1438
    ##  9 2021-05-12 10:30:00  107.  107.  106.  106.    764
    ## 10 2021-05-12 10:35:00  106.  106.  106.  106.   1191
    ## # ... with 990 more rows

The returned data has 1000 bars, which is unlikely to contain the full
dataset.

We can see what’s missing using a helper function called
`expand_calendar` that provides a full timeseries of expected time
points for a given timeframe returned by `calendar`. `expand_calendar`
has `market_hours = TRUE` which will only return the expected time
points contained within market hours. Set to `FALSE` to return the full
time panel.

``` r
cal <- calendar(from = fr, to  = to)
expected <- tsibble::interval(bars) %>%
  {expand_calendar(cal, timeframe = period_units(.), multiplier = period_multiplier(.))}
```

With the expected hours, we can see what’s missing:

``` r
missing <- setdiff(expected$time, bars$time)
length(missing)
```

    ## [1] 7147

``` r
lubridate::as_datetime(range(missing))
```

    ## [1] "2021-01-04 14:30:00 UTC" "2021-06-01 20:00:00 UTC"

By setting `full = TRUE` we can expect to get a dataset with virtually
all the market hours (rather than session hours) accounted for. Due to
the multiple queries it will take more time.

``` r
bars <- market_data("BYND", v = 1, time = "m", m = 5, from = fr, to = to, full = TRUE)
```

    ## `limit` coerced to 1000

``` r
missing <- setdiff(expected$time, bars$time) %>% 
subset(subset =  . < lubridate::as_datetime(to)) %>% 
  lubridate::as_datetime(tz = "America/New_York")
length(missing)
```

    ## [1] 549

``` r
head(missing, 20)
```

    ##  [1] "2021-01-04 13:35:00 EST" "2021-01-04 14:10:00 EST"
    ##  [3] "2021-01-04 14:15:00 EST" "2021-01-04 16:00:00 EST"
    ##  [5] "2021-01-05 12:35:00 EST" "2021-01-05 13:20:00 EST"
    ##  [7] "2021-01-05 13:45:00 EST" "2021-01-05 16:00:00 EST"
    ##  [9] "2021-01-06 16:00:00 EST" "2021-01-07 16:00:00 EST"
    ## [11] "2021-01-08 16:00:00 EST" "2021-01-11 16:00:00 EST"
    ## [13] "2021-01-12 12:25:00 EST" "2021-01-12 16:00:00 EST"
    ## [15] "2021-01-13 16:00:00 EST" "2021-01-14 16:00:00 EST"
    ## [17] "2021-01-15 16:00:00 EST" "2021-01-19 16:00:00 EST"
    ## [19] "2021-01-20 16:00:00 EST" "2021-01-21 13:00:00 EST"

``` r
tail(missing, 20)
```

    ##  [1] "2021-05-18 16:00:00 EDT" "2021-05-19 10:50:00 EDT"
    ##  [3] "2021-05-19 13:00:00 EDT" "2021-05-19 13:10:00 EDT"
    ##  [5] "2021-05-19 16:00:00 EDT" "2021-05-20 13:25:00 EDT"
    ##  [7] "2021-05-20 16:00:00 EDT" "2021-05-21 10:20:00 EDT"
    ##  [9] "2021-05-21 11:20:00 EDT" "2021-05-21 12:10:00 EDT"
    ## [11] "2021-05-21 12:20:00 EDT" "2021-05-21 12:45:00 EDT"
    ## [13] "2021-05-21 12:55:00 EDT" "2021-05-21 15:25:00 EDT"
    ## [15] "2021-05-21 16:00:00 EDT" "2021-05-24 16:00:00 EDT"
    ## [17] "2021-05-25 16:00:00 EDT" "2021-05-26 16:00:00 EDT"
    ## [19] "2021-05-27 16:00:00 EDT" "2021-05-28 16:00:00 EDT"

Note that there is still missing data which are likely time points where
the price did not change or for which the API simply doesn’t have a
record.

See `?market_data` for more details or visit the [Market Data Endpoint
docs](https://docs.alpaca.markets/api-documentation/web-api/market-data/)
to learn more.

## Polygon

> <span style="color:blue">*Note*</span> Alpaca’s agreement with Polygon
> ended in January 2021. A [Polygon
> subscription](https://polygon.io/pricing) is required to use the
> Polygon API, and the subscription level determines what Polygon
> endpoints are available. The `polygon` function and docs are up to
> date as of 2021-06-11 but will no longer be maintained. If you have a
> Polygon subscription and wish to help maintain this functionality
> please [email the maintainer](mailto:sholsen@alumni.emory.edu).

`AlpacaforR` provides a single go-to function to access all of the
available Polygon endpoints: `polygon`. This function takes as it’s
first argument `ep`, short for endpoint, which can be the full name of
the endpoint as it appears in the [Docs](https://polygon.io/docs/) or a
one to two letter abbreviation of the endpoint which is typically the
first letter of each of the first two words (that aren’t wrapped in
parentheses) of the name of the endpoint. The one exception being
Snapshot - Single Ticker (`st`), which would otherwise conflict with
Stock Splits (`ss`). For ease of referencing all of the Polygon
endpoints without leaving R, the documentation for `?polygon` elaborates
the names of the endpoints, their descriptions, details and parameters.
Additionally, the `polygon` function itself provides reference tibbles
of the abbreviations and full names of the endpoints by using `'all'` as
the value for `ep` to show all endpoints, `'ref'/'reference'` for all
the reference endpoints, `'sto'/'stocks'` for all the stock/equity
endpoints.

``` r
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

A plus (`+`) can be appended to the end of any of these reference
keywords, or the abbreviation/name of an endpoint to view a helpful
reference list with the following for each endpoint:

-   The full name of the endpoint
-   the description
-   the URL for the documentation
-   the URL of the endpoint itself
-   the parameters, with the default always in first position when
    options are available. When endpoints that take parameters are
    called without explicitly providing parameters, these defaults are
    used to call the endpoint.

``` r
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

Many endpoints require parameters to be specified. The parameters can be
specified as either named arguments passed to the function directly

``` r
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

``` r
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

Some endpoints provide query status info or map details (the data
classes of the values in the returned object) and other information that
can be accessed using `get_query(obj)` or `attr(obj, "map")`
respectively (where `obj` is the object returned by `polygon`).

## Orders

Getting, submitting, and canceling 🚫 orders are also made easy through
`orders` and `order_submit`. Visit the [Orders Endpoint
docs](https://alpaca.markets/docs/api-documentation/api-v2/orders/) to
learn everything there is to know about the requests and responses for
this API.

### `orders`

To view open orders for the paper account, use `orders()` as the default
`status` is set to `"open"`.

``` r
orders()
```

    ## No orders for the selected query/filter criteria. 
    ## Check `symbol_id` or set status = 'all' to see all orders.

    ## list()
    ## attr(,"query")
    ## attr(,"query")$status_code
    ## [1] 200
    ## 
    ## attr(,"query")$url
    ## [1] "https://paper-api.alpaca.markets/v2/orders?status=open&direction=desc&nested=TRUE"
    ## 
    ## attr(,"query")$ts
    ## [1] "2021-06-16 11:10:03 EDT"

Alternatively, set the `status` to `"open"`, `"closed"`, or `"all"` to
see specific subsets of orders based on their status. <span
style="color:blue">*Note*</span> that the default `limit` is `50`. To
return more or less than 50, `limit` must be set explicitly.

``` r
orders(status = "all", limit = 10)
```

    ## # A tibble: 10 x 32
    ##    id              client_order_id       created_at          updated_at         
    ##    <chr>           <chr>                 <dttm>              <dttm>             
    ##  1 8037e248-e151-~ 39af777b-2b42-458d-8~ 2021-06-12 14:45:12 2021-06-14 16:00:32
    ##  2 4bc78105-476a-~ 34182540-97f9-4de4-8~ 2021-06-10 15:20:14 2021-06-10 15:20:14
    ##  3 d7c2694b-0af2-~ e86cd748-8881-4107-a~ 2021-06-10 15:20:13 2021-06-10 15:20:14
    ##  4 40198680-491f-~ e86cd748-8881-4107-a~ 2021-06-10 15:19:41 2021-06-10 15:20:13
    ##  5 acf051d0-033a-~ e86cd748-8881-4107-a~ 2021-06-10 15:19:41 2021-06-10 15:20:13
    ##  6 e86cd748-8881-~ 5d7a90d2-6a91-48cd-9~ 2021-06-10 15:19:40 2021-06-10 15:19:41
    ##  7 aa6b2e40-1f8b-~ 82da945e-3160-4c9a-8~ 2021-06-10 15:14:09 2021-06-10 15:14:09
    ##  8 82da945e-3160-~ 3782868a-87f7-4b88-a~ 2021-06-10 15:14:08 2021-06-10 15:14:08
    ##  9 7a2baf81-f295-~ 2b4900e9-9fd5-409c-8~ 2021-06-10 13:35:43 2021-06-10 13:35:43
    ## 10 d0262758-d9f6-~ 243e3f1f-248d-46bc-a~ 2021-06-10 13:10:56 2021-06-10 13:19:03
    ## # ... with 28 more variables: submitted_at <dttm>, filled_at <dttm>,
    ## #   expired_at <dttm>, canceled_at <dttm>, failed_at <dttm>,
    ## #   replaced_at <dttm>, replaced_by <chr>, replaces <chr>, asset_id <chr>,
    ## #   symbol <chr>, asset_class <chr>, notional <lgl>, qty <dbl>,
    ## #   filled_qty <dbl>, filled_avg_price <chr>, order_class <chr>,
    ## #   order_type <chr>, type <chr>, side <chr>, time_in_force <chr>,
    ## #   limit_price <chr>, stop_price <dbl>, status <chr>, extended_hours <lgl>,
    ## #   legs <list>, trail_percent <lgl>, trail_price <lgl>, hwm <lgl>

In R, all arguments can be partial, ie abbreviated, up to the number of
characters necessary to differentiate the argument from other arguments
provided to the function. Here is the shorthand to view all orders
placed since the beginning of the week:

``` r
(orders_this_week <- orders(st = "a", a = lubridate::floor_date(lubridate::today(), "week"), lim = 10))
```

    ## No orders for the selected query/filter criteria. 
    ## Check `symbol_id` or set status = 'all' to see all orders.

    ## list()
    ## attr(,"query")
    ## attr(,"query")$status_code
    ## [1] 200
    ## 
    ## attr(,"query")$url
    ## [1] "https://paper-api.alpaca.markets/v2/orders?status=all&limit=10&after=2021-06-13&direction=desc&nested=TRUE"
    ## 
    ## attr(,"query")$ts
    ## [1] "2021-06-16 11:10:06 EDT"

<span style="color:blue">*Note*</span> complex orders will automatically
appear nested in the returned tibble. To change this behavior, set
`nested = F`.

Individual orders can be called by providing their id to `symbol_id`
(the first argument):

``` r
if (isTRUE(nrow(orders_this_week) > 0)) {
  # Works only if there are existing orders 
  (fo <- orders(orders_this_week[1,]$id))
  all.equal(unlist(orders_this_week[1,]), unlist(fo), check.attributes = FALSE)
}
```

Individual orders can also be called by providing the client order ID to
`symbol_id` and setting `client_order_id = T`.

``` r
if (isTRUE(nrow(orders_this_week) > 0)) {
  # Works only if there are existing orders
  orders(orders_this_week[1,]$client_order_id, client_order_id = T)
}
```

### `order_submit`

`order_submit` handles *any kind of order*. The value supplied to the
`action` argument determines what type of action will be taken and what
parameters are required. The types or orders and their corresponding
`action` are:

-   a new order `action = "s"/"submit"` *Default*
-   a complex or trailing stop order `action = "s"/"submit"`
-   an order replacement `action = "r"/"replace"`
-   an order cancellation `action = "c"/"cancel"`
-   or canceling all orders `action = "cancel_all"`

A simple use case where a buy order for two shares of Beyond Meat is
placed is below:

``` r
# is the market open?
(.open <- clock()$is_open)
```

    ## [1] TRUE

``` r
#> [1] TRUE
if (.open) {
  # if the market is open then place a market buy order for "BYND"
(bo <- order_submit("bynd", side = "b", q = 2, type = "m"))
}
```

    ## # A tibble: 1 x 32
    ##   id               client_order_id       created_at          updated_at         
    ##   <chr>            <chr>                 <dttm>              <dttm>             
    ## 1 f18d28c7-452f-4~ bf984110-0cc0-4a97-a~ 2021-06-16 11:10:06 2021-06-16 11:10:06
    ## # ... with 28 more variables: submitted_at <dttm>, filled_at <dttm>,
    ## #   expired_at <dttm>, canceled_at <dttm>, failed_at <dttm>,
    ## #   replaced_at <dttm>, replaced_by <lgl>, replaces <lgl>, asset_id <chr>,
    ## #   symbol <chr>, asset_class <chr>, notional <lgl>, qty <dbl>,
    ## #   filled_qty <dbl>, filled_avg_price <lgl>, order_class <chr>,
    ## #   order_type <chr>, type <chr>, side <chr>, time_in_force <chr>,
    ## #   limit_price <lgl>, stop_price <lgl>, status <chr>, extended_hours <lgl>,
    ## #   legs <lgl>, trail_percent <lgl>, trail_price <lgl>, hwm <lgl>

`order_submit` has extensive built-in auto-assumption of omitted
arguments where they can be assumed based on other provided arguments.
The documentation (`?order_submit`) and the examples therein go into
detail as to the required parameters necessary to invoke accurate
auto-assumption for each action. Since traders will often place a stop,
limit, or stop limit order following a buy order to mitigate downside
risk, an ‘expedited sell’ is one such auto-assumption feature. To
execute an ‘expedited sell’, the function needs only the Order tibble
(assuming it contains the `id` row) of the buy order, and the specifics
of the sell order.

To set appropriate stops and limits it’s necessary to know the current
price of the stock.

``` r
(lq <- market_data(timeframe = "lq", symbol = "bynd"))
```

    ## `from` omitted. Set to 2021-06-15

    ## `to` omitted. Set to 2021-06-17

    ## `limit` coerced to 10000

    ## # A tibble: 1 x 8
    ##   time                ax       ap    as bx       bp    bs c    
    ##   <dttm>              <chr> <int> <int> <chr> <dbl> <int> <chr>
    ## 1 2021-06-16 11:10:03 V       152     1 V      143.     1 R

With this information, a stop order can be placed at the price 5% lower
than what it was bought at. To connect this sell order to the buy order
for cost basis reporting purposes, set `client_order_id = T` and the
`client_order_id` for this sell order will be set to the Order ID of the
buy order.

``` r
if (.open) {
  (so <- order_submit(bo$id, stop = lq$ap * .95, client_order_id = T))
}
```

    ## `side` set to 'sell'
    ## `qty` set to 2
    ## `symbol_id` set to BYND
    ## `client_order_id` set to f18d28c7-452f-474e-8b9d-3584ab5de7aa

    ## `type` set to 'stop'

    ## # A tibble: 1 x 32
    ##   id               client_order_id       created_at          updated_at         
    ##   <chr>            <chr>                 <dttm>              <dttm>             
    ## 1 467c03d2-9adb-4~ f18d28c7-452f-474e-8~ 2021-06-16 11:10:09 2021-06-16 11:10:09
    ## # ... with 28 more variables: submitted_at <dttm>, filled_at <dttm>,
    ## #   expired_at <dttm>, canceled_at <dttm>, failed_at <dttm>,
    ## #   replaced_at <dttm>, replaced_by <lgl>, replaces <lgl>, asset_id <chr>,
    ## #   symbol <chr>, asset_class <chr>, notional <lgl>, qty <dbl>,
    ## #   filled_qty <dbl>, filled_avg_price <lgl>, order_class <chr>,
    ## #   order_type <chr>, type <chr>, side <chr>, time_in_force <chr>,
    ## #   limit_price <lgl>, stop_price <dbl>, status <chr>, extended_hours <lgl>,
    ## #   legs <lgl>, trail_percent <lgl>, trail_price <lgl>, hwm <lgl>

Informative messages indicate where the function made assumptions about
the values for other arguments.

To extend the example, suppose the price of BYND went up since the order
was first placed, yet the stop is still set at 5% lower from where the
order was bought. It would be wise to move the stop order up a bit to
follow the price action. This can be done for simple orders (of which
this is one), with `action = 'replace'`. The replacement order will have
a field `replaces` that will indicate the order it replaced, in this
case the previous sell order. The sell order placed above was linked to
the buy order via the `client_order_id` such that cost basis can
accurately be reported. However, `client_order_id` must be unique for
each order. So how does one keep this replacement order connected to the
original buy order?

The simplest way to do so is to provide a custom `client_order_id` with
an incremented suffix appended for each successive replacement order.
The full length of the `client_order_id` must be under `48` characters
and the Alpaca generated IDs are `36` characters, which leaves
48 − 36 = 12 characters for the suffix. This tracking method can be
especially useful if implementing a trailing stop for a given buy order
that will refresh often.

Here the `client_order_id` is created:

``` r
(client_order_id <- paste0(bo$id,".2"))
```

    ## [1] "f18d28c7-452f-474e-8b9d-3584ab5de7aa.2"

``` r
nchar(client_order_id)
```

    ## [1] 38

The replacement order can now be placed with a higher stop and remain
effectively linked to it’s original buy order via the first 36
characters of it’s `client_order_id`

``` r
if (.open && isTRUE(nrow(so) > 0)) {
  (ro <- order_submit(so$id, a = "r", stop = lq$ap * .96, client_order_id = client_order_id))
}
```

    ## # A tibble: 1 x 32
    ##   id              client_order_id        created_at          updated_at         
    ##   <chr>           <chr>                  <dttm>              <dttm>             
    ## 1 efd4e5fb-1c84-~ f18d28c7-452f-474e-8b~ 2021-06-16 11:10:09 2021-06-16 11:10:41
    ## # ... with 28 more variables: submitted_at <dttm>, filled_at <dttm>,
    ## #   expired_at <dttm>, canceled_at <dttm>, failed_at <dttm>,
    ## #   replaced_at <dttm>, replaced_by <lgl>, replaces <chr>, asset_id <chr>,
    ## #   symbol <chr>, asset_class <chr>, notional <lgl>, qty <dbl>,
    ## #   filled_qty <dbl>, filled_avg_price <lgl>, order_class <chr>,
    ## #   order_type <chr>, type <chr>, side <chr>, time_in_force <chr>,
    ## #   limit_price <lgl>, stop_price <dbl>, status <chr>, extended_hours <lgl>,
    ## #   legs <lgl>, trail_percent <lgl>, trail_price <lgl>, hwm <lgl>

However, it’s also possible that the trader would like to take a profit
if the price moves up another 5% while simultaneously having a stop in
place to prevent losses. An [Advanced
Order](https://alpaca.markets/docs/trading-on-alpaca/orders/#oco-orders)
called “`O`ne `C`ancels `O`ther” is perfect for this situation. First,
the replacement order needs to be canceled.

``` r
if (.open && isTRUE(nrow(ro) > 0)) {
  order_submit(ro$id, a = "c")
}
```

    ## Order canceled successfully

    ## list()
    ## attr(,"query")
    ## attr(,"query")$status_code
    ## [1] 204
    ## 
    ## attr(,"query")$url
    ## [1] "https://paper-api.alpaca.markets/v2/orders/efd4e5fb-1c84-4f8e-96d3-70e12a91db46"
    ## 
    ## attr(,"query")$ts
    ## [1] "2021-06-16 11:10:42 EDT"

The `oco` order requires two additional parameters, an upper limit order
provided to the argument `take_profit` as a named list, with a single
item named `'limit_price'/'l'`:

``` r
take_profit <- list(l = lq$ap * 1.05)
```

and a lower limit, stop or both specified to `stop_loss` as a named list
with the names `'stop_price'/'s'` & `'limit_price'/'l'`:

``` r
stop_loss <- list(s = lq$ap * .95)
```

Now the `oco` order class can be placed by providing the id of the
original buy order to passively set argument defaults. Another increment
to the `client_order_id` can links this order to the original buy order.

``` r
if (.open) {
  (oco <- order_submit(bo$id, order_class = "oco", time_in_force = "gtc", client_order_id = paste0(bo$id,".3"), take_profit = take_profit, stop_loss = stop_loss))
}
```

    ## `side` set to 'sell'
    ## `qty` set to 2
    ## `symbol_id` set to BYND

    ## order_class: 'oco' requires type = 'limit'. `type` set to 'limit'.

    ## # A tibble: 1 x 32
    ##   id              client_order_id        created_at          updated_at         
    ##   <chr>           <chr>                  <dttm>              <dttm>             
    ## 1 5e9eb216-61e9-~ f18d28c7-452f-474e-8b~ 2021-06-16 11:10:43 2021-06-16 11:10:43
    ## # ... with 28 more variables: submitted_at <dttm>, filled_at <dttm>,
    ## #   expired_at <dttm>, canceled_at <dttm>, failed_at <dttm>,
    ## #   replaced_at <dttm>, replaced_by <lgl>, replaces <lgl>, asset_id <chr>,
    ## #   symbol <chr>, asset_class <chr>, notional <lgl>, qty <dbl>,
    ## #   filled_qty <dbl>, filled_avg_price <lgl>, order_class <chr>,
    ## #   order_type <chr>, type <chr>, side <chr>, time_in_force <chr>,
    ## #   limit_price <dbl>, stop_price <lgl>, status <chr>, extended_hours <lgl>,
    ## #   legs <df[,32]>, trail_percent <lgl>, trail_price <lgl>, hwm <lgl>

The additional linked orders for any Advanced Order can be viewed as
it’s `legs`. When submitting an order, the default response is to return
the `legs` nested under the top level order. When calling `orders`,
order legs can be unnested such that each row is a separate order by
setting `nested = F` :

``` r
if (.open) {
  oco$legs
}
```

    ##                                     id                      client_order_id
    ## 1 f57e27fa-f0ad-4eb0-9219-017d5c68ec4d 965da2b5-9a02-4f51-b05e-60d433197989
    ##            created_at          updated_at        submitted_at filled_at
    ## 1 2021-06-16 11:10:43 2021-06-16 11:10:43 2021-06-16 11:10:43      <NA>
    ##   expired_at canceled_at failed_at replaced_at replaced_by replaces
    ## 1       <NA>        <NA>      <NA>        <NA>          NA       NA
    ##                               asset_id symbol asset_class notional qty
    ## 1 317faf0e-4956-4d40-b3eb-3fba8af59dca   BYND   us_equity       NA   2
    ##   filled_qty filled_avg_price order_class order_type type side time_in_force
    ## 1          0               NA         oco       stop stop sell           gtc
    ##   limit_price stop_price status extended_hours legs trail_percent trail_price
    ## 1          NA      144.4   held          FALSE   NA            NA          NA
    ##   hwm
    ## 1  NA

All open orders can be canceled by using the `"cancel_all"` keyword as
the `action`

``` r
order_submit(action = "cancel_all")
```

    ## # A tibble: 1 x 32
    ##   asset_class asset_id   canceled_at         client_order_id created_at         
    ##   <chr>       <chr>      <dttm>              <chr>           <dttm>             
    ## 1 us_equity   317faf0e-~ NA                  f18d28c7-452f-~ 2021-06-16 11:10:43
    ## # ... with 27 more variables: expired_at <dttm>, extended_hours <lgl>,
    ## #   failed_at <dttm>, filled_at <dttm>, filled_avg_price <lgl>,
    ## #   filled_qty <dbl>, hwm <lgl>, id <chr>, legs <lgl>, limit_price <dbl>,
    ## #   notional <lgl>, order_class <chr>, order_type <chr>, qty <dbl>,
    ## #   replaced_at <dttm>, replaced_by <lgl>, replaces <lgl>, side <chr>,
    ## #   status <chr>, stop_price <lgl>, submitted_at <dttm>, symbol <chr>,
    ## #   time_in_force <chr>, trail_percent <lgl>, trail_price <lgl>, type <chr>,
    ## #   updated_at <dttm>

`order_submit` is a versatile function, see it’s documentation and
examples to learn about all it has to offer.

## Positions

All current positions or only the positions specified by `symbols` are
retrieved by calling `positions()`. `positions` has multiple actions:

-   `"get"/"g"` positions (the default)
-   `"close"/"c"` a position or positions provided by `ticker`
-   `"close_all"` which will cancel all open orders on currently held
    positions and then close those positions by selling all shares.
    Think of `action = "close_all"` as an emergency kill switch that
    will liquidate all positions.

Visit the [Positions endpoint
docs](https://alpaca.markets/docs/api-documentation/api-v2/positions/)
to learn more.

Retrieve all positions:

``` r
#If paper account:
positions()
```

    ## # A tibble: 1 x 16
    ##   asset_id  symbol exchange asset_class   qty avg_entry_price side  market_value
    ##   <chr>     <chr>  <chr>    <chr>       <dbl>           <dbl> <chr>        <dbl>
    ## 1 317faf0e~ BYND   NASDAQ   us_equity       2            149. long          298.
    ## # ... with 8 more variables: cost_basis <dbl>, unrealized_pl <dbl>,
    ## #   unrealized_plpc <dbl>, unrealized_intraday_pl <dbl>,
    ## #   unrealized_intraday_plpc <dbl>, current_price <dbl>, lastday_price <dbl>,
    ## #   change_today <dbl>

If a position exists, it can be closed using `action = "cancel"`

``` r
positions("BYND", action = "c")
```

    ## # A tibble: 1 x 32
    ##   id               client_order_id       created_at          updated_at         
    ##   <chr>            <chr>                 <dttm>              <dttm>             
    ## 1 ffe7b215-3bfa-4~ b1fec025-b90f-468f-a~ 2021-06-16 11:10:45 2021-06-16 11:10:45
    ## # ... with 28 more variables: submitted_at <dttm>, filled_at <dttm>,
    ## #   expired_at <dttm>, canceled_at <dttm>, failed_at <dttm>,
    ## #   replaced_at <dttm>, replaced_by <lgl>, replaces <lgl>, asset_id <chr>,
    ## #   symbol <chr>, asset_class <chr>, notional <lgl>, qty <dbl>,
    ## #   filled_qty <dbl>, filled_avg_price <lgl>, order_class <chr>,
    ## #   order_type <chr>, type <chr>, side <chr>, time_in_force <chr>,
    ## #   limit_price <lgl>, stop_price <lgl>, status <chr>, extended_hours <lgl>,
    ## #   legs <lgl>, trail_percent <lgl>, trail_price <lgl>, hwm <lgl>

All positions are closed using `action = "close_all"`

``` r
positions(a = "close_all")
#> No positions are open at this time.
#> list()
#> attr(,"query")
#> attr(,"query")$ts
#> [1] "2021-06-10 13:35:53 EDT"
#> 
#> attr(,"query")$status_code
#> [1] 207
#> 
#> attr(,"query")$url
#> [1] "https://paper-api.alpaca.markets/v2/positions?cancel_orders=TRUE"
```

## Websockets

The package also supports Alpaca’s & Polygon’s Websockets/Streaming
APIs. See the Websockets vignette for more on how to use Alpaca’s
streaming service. `vignette("AlpacaforR", "Websockets")`
