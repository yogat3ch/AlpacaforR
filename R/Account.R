# account ----
# Wed Apr 22 20:26:32 2020
#' @family Account
#' @title Get Account Details
#'
#' @description The accounts API serves important information related to an account, including account status, funds available for trade, funds available for withdrawal, and various flags relevant to an account's ability to trade. See the [Account](https://alpaca.markets/docs/api-documentation/api-v2/account/) Endpoint in the API v2 Docs for full details.
#' @param live \code{(logical)} TRUE / FALSE if you are connecting to a live account. Default to FALSE, so it will use the paper url if nothing was provided.
#' @return Account \code{(list)} An [Account](https://alpaca.markets/docs/api-documentation/api-v2/account/#account-entity) object list of length 26:
#' \itemize{
#'  \item{\code{id}}{ \code{(character)} Account ID}
#'  \item{\code{account_number}}{ \code{(character)} Account number}
#'  \item{\code{status}}{ \code{(character)} See [Account Endpoint Properties](https://alpaca.markets/docs/api-documentation/api-v2/account/#properties) for details}
#'  \item{\code{currency}}{ \code{(character)} USD}
#'  \item{\code{buying_power}}{ \code{(numeric)} Current available $ buying power; If multiplier = 4, this is your daytrade buying power which is calculated as \code{(last_equity - (last) maintenance_margin) * 4; If multiplier = 2, buying_power = max(equity - initial_margin,0) * 2; If multiplier = 1, buying_power = cash}}
#'  
#'  \item{\code{cash}}{ \code{(numeric)} cash balance}
#'  \item{\code{portfolio_value}}{ \code{(numeric)} Total value of cash + holding positions}
#'  \item{\code{trading_blocked}}{ \code{(logical)} If true, the account is not allowed to place orders as a boolean.}
#'  \item{\code{transfers_blocked}}{ \code{(logical)} If true, the account is not allowed to request money transfers as a boolean.}
#'  \item{\code{account_blocked}}{ \code{(logical)} If true, the account activity by user is prohibited as a boolean.}
#'  \item{\code{created_at}}{ \code{(Datetime/POSIXct)} Timestamp this account was created at.}
#' }
#' @examples 
#' account()
#' # With default arguments equivalent to :
#' account(live = FALSE)
#' # For access to live accounts, you must submit as live = TRUE
#' account(live = TRUE)
#' @importFrom httr GET
#' @importFrom purrr map_if
#' @importFrom lubridate as_datetime
#' @export
account <-
  function(live = as.logical(Sys.getenv("APCA-LIVE", FALSE))) {
    
  #Set URL & Headers
  
  headers = get_headers(live)
  #Send Request
  out = httr::GET(url = get_url("account", live = live), headers)
  out = response_text_clean(out)
  suppressWarnings(
    out <- purrr::map_if(out, .p = ~!is.na(as.numeric(.x)) && !is.logical(.x), .f = as.numeric)
  )
  suppressMessages(out$created_at <- lubridate::as_datetime(out$created_at, tz = Sys.timezone()))
  return(out)
}
#----------------------------------------------------------------------------------------------

#' @family Accounts
#' @title get_account
#' @rdname account
#' @description \code{get_account} is deprecated please use \code{\link[AlpacaforR]{account}} instead.
#' @examples get_account()
#' @export

get_account <- account






# account_config ----
# Wed Apr 22 20:25:56 2020
#' @family Account
#' @title Get Account Configurations
#'
#' @description The account configuration API serves an important role in setting the way you want. See [Account Configurations](https://alpaca.markets/docs/api-documentation/api-v2/account-configuration/) for details.
#' @param dtbp_check \code{(character)} both, entry, or exit. Controls Day Trading Margin Call (DTMC) checks. Default NULL (no change). Set to "default" to set all configuration options back to defaults. See Value section for defaults.
#' @param no_shorting \code{(logical)} If true, account becomes long-only mode. Default NULL (no change).
#' @param pdt_check \code{(character)} Not documented.
#' @param suspend_trade \code{(logical)} If true, new orders are blocked. Default NULL (no change).
#' @param trade_confirm_email \code{(character)} all or none. If none, emails for order fills are not sent. Default NULL (no change).
#' @inheritParams account
#' @return AccountConfigurations \code{(list)} [AccountConfigurations](https://alpaca.markets/docs/api-documentation/api-v2/account-configuration/#accountconfigurations-entity)  object with the following parameters (defaults shown):
#' \itemize{
#'  \item{`dtbp_check = "entry"`}
#'  \item{`no_shorting = FALSE`}
#'  \item{`pdt_check = "entry"`}
#'  \item{`suspend_trade = FALSE`}
#'  \item{`trade_confirm_email = "all"`}
#' }
#' @examples 
#' account_config(live = FALSE)
#' # Which is similar to:
#' account_config()
#' # For access to live accounts, you must submit as live = TRUE
#' account_config(live = TRUE)
#' account_config(dtbp_check = "both", no_shorting = TRUE)
#' account_config("default")
#' @importFrom httr GET PATCH
#' @importFrom purrr compact
#' @export
account_config <-
  function(dtbp_check = NULL,
           no_shorting = NULL,
           pdt_check = NULL,
           suspend_trade = NULL,
           trade_confirm_email = NULL,
           live = as.logical(Sys.getenv("APCA-LIVE", FALSE))) {
    
  #Create body with order details, most common is a named list 
  .def <- ifelse(length(dtbp_check == "default") == 0, F, tolower(substr(dtbp_check,0,1)) == "d")
  if (.def) {
    bodyl <- list(dtbp_check = "entry", trade_confirm_email = "all", pdt_check = "entry", suspend_trade = FALSE, no_shorting = FALSE)
  } else {
    bodyl <- purrr::compact(list(dtbp_check = dtbp_check, no_shorting = no_shorting, suspend_trade = suspend_trade, trade_confirm_email = trade_confirm_email))
  }
  
  #Set URL & Headers
  headers = get_headers(live)
  if (length(bodyl) > 0) {
    # if configuration options are set
    account_config = httr::PATCH(url = get_url(c("account", "configurations"), live = live), body = bodyl, encode = "json", headers)
  } else {
    # if no options are set, just return the account config
    account_config = httr::GET(url = get_url(c("account", "configurations"), live = live), headers)
  }
  account_config = response_text_clean(account_config)
  return(account_config)
}
#----------------------------------------------------------------------------------------------
#NEW for V2
#account_config(live = TRUE)
#' @title get_config
#' @rdname account_config
#' @description \code{get_config} is deprecated, please use \code{\link[AlpacaforR]{account_config}} instead.
#' @examples get_config()
#' @export
get_config <- account_config







#' @rdname account_config
#' @md
#' @title Send Account Configurations function (Deprecated)
#' 
#' @description \code{set_config} is deprecated. Please use \code{\link[AlpacaforR]{account_config}} instead.
#' See [Account Configurations](https://alpaca.markets/docs/api-documentation/api-v2/account-configuration/) for details.
#' @export
set_config <- account_config





# account_activities ----
# Wed Apr 22 20:25:28 2020
#' @family Account
#' @title Get Account Activity
#'
#' @description The account activities API provides access to a historical record of transaction activities that have impacted your account. Trade execution activities and non-trade activities, such as dividend payments, are both reported through this endpoint. See [Account Activities](https://alpaca.markets/docs/api-documentation/api-v2/account-activities/) for details.
#' @param activity_type \code{(character)} The activity type you want to view entries for. A list of valid activity types can be found in Details.
#' @param date \code{(character/Date)} The date in YYYY-MM-DD format for which you want to see activities. 
#' @param until \code{(character/Date)} The response will contain only activities submitted before this date in YYYY-MM-DD format. (Cannot be used with date.)
#' @param after \code{(character/Date)} The response will contain only activities submitted after this date in YYYY-MM-DD format. (Cannot be used with date.)
#' @param direction \code{(character)} asc or desc, default is desc.
#' @param page_size \code{(integer/numeric)} The maximum number of entries to return in the response.
#' @param page_token \code{(character)} The ID of the end of your current page of results to indicate where the next page should start.
#' @inheritParams account
#' @return \code{TradeActivity} \code{(tibble)} [TradeActivity](https://alpaca.markets/docs/api-documentation/api-v2/account-activities/#tradeactivity-entity) Object or  a [NonTradeActivity](https://alpaca.markets/docs/api-documentation/api-v2/account-activities/#nontradeactivity-entity) Object. See Details.
#' \itemize{
#'  \item{\code{id}}{ \code{(character)} An id for the activity. Always in "
#'  ::" format. Can be sent as `page_token` in requests to facilitate the paging of results.}
#'  \item{\code{activity_type}}{ \code{(character)} Activity type to filter for. IE "FILL". Non case-sensitive.}
#'  \item{\code{transaction_time}}{ \code{(POSIXct)} The time at which the execution occurred.}
#'  \item{\code{type}}{ \code{(character)} `"fill"` or `"partial_fill"`}
#'  \item{\code{price}}{ \code{(numeric)} The per-share price that the trade was executed at.}
#'  \item{\code{qty}}{ \code{(integer)} The number of shares involved in the trade execution.}
#'  \item{\code{side}}{ \code{(character)} `"buy"` or `"sell"`}
#'  \item{\code{symbol}}{ \code{(character)} The symbol of the security being traded.}
#'  \item{\code{leaves_qty}}{ \code{(integer)} For partially_filled orders, the quantity of shares that are left to be filled.}
#'  \item{\code{order_id}}{ \code{(character)} The id for the order that filled.}
#'  \item{\code{cum_qty}}{ \code{(integer)} The cumulative quantity of shares involved in the execution.}
#' }
#' @return \code{NonTradeActivity} \code{(tibble)} A [NonTradeActivity Entity](https://alpaca.markets/docs/api-documentation/api-v2/account-activities#nontradeactivity-entity):
#' \itemize{
#'   \item{`activity_type`}{ `(character)` See Details}
#'  \item{\code{id}}{ \code{(character)} An id for the activity. Always in "
#'  ::" format. Can be sent as `page_token` in requests to facilitate the paging of results.}
#'   \item{\code{date}}{ \code{(POSIXct)} The date on which the activity occurred or on which the transaction associated with the activity settled.}
#'   \item{\code{net_amount}}{ \code{(numeric)} The net amount of money (positive or negative) associated with the activity.}
#'  \item{\code{symbol}}{ \code{(character)} The symbol of the security involved with the activity. Not present for all activity types.}
#'  \item{\code{qty}}{ \code{(integer)} For dividend activities, the number of shares that contributed to the payment. Not present for other activity types.}
#'   \item{\code{per_share_amount}}{ \code{(numeric)} For dividend activities, the average amount paid per share. Not present for other activity types.}
#' }
#' @details 
#' Activity Types:
#' \itemize{
#'   \item{\code{"FILL"}}{ Order fills (both partial and full fills)}
#'   \item{\code{"TRANS"}}{ Cash transactions (both CSD and CSR)}
#'   \item{\code{"MISC"}}{ Miscellaneous or rarely used activity types (All types except those in TRANS, DIV, or   FILL)}
#'   \item{\code{"ACATC"}}{ ACATS IN/OUT (Cash)}
#'   \item{\code{"ACATS"}}{ ACATS IN/OUT (Securities)}
#'   \item{\code{"CSD"}}{ Cash disbursement(+)}
#'   \item{\code{"CSR"}}{ Cash receipt(-)}
#'   \item{\code{"DIV"}}{ Dividends}
#'   \item{\code{"DIVCGL"}}{ Dividend (capital gain long term)}
#'   \item{\code{"DIVCGS"}}{ Dividend (capital gain short term)}
#'   \item{\code{"DIVFEE"}}{ Dividend fee}
#'   \item{\code{"DIVFT"}}{ Dividend adjusted (Foreign Tax Withheld)}
#'   \item{\code{"DIVNRA"}}{ Dividend adjusted (NRA Withheld)}
#'   \item{\code{"DIVROC"}}{ Dividend return of capital}
#'   \item{\code{"DIVTW"}}{ Dividend adjusted (Tefra Withheld)}
#'   \item{\code{"DIVTXEX"}}{ Dividend (tax exempt)}
#'   \item{\code{"INT"}}{ Interest (credit/margin)}
#'   \item{\code{"INTNRA"}}{ Interest adjusted (NRA Withheld)}
#'   \item{\code{"INTTW"}}{ Interest adjusted (Tefra Withheld)}
#'   \item{\code{"JNL"}}{ Journal entry}
#'   \item{\code{"JNLC"}}{ Journal entry (cash)}
#'   \item{\code{"JNLS"}}{ Journal entry (stock)}
#'   \item{\code{"MA"}}{ Merger/Acquisition}
#'   \item{\code{"NC"}}{ Name change}
#'   \item{\code{"OPASN"}}{ Option assignment}
#'   \item{\code{"OPEXP"}}{ Option expiration}
#'   \item{\code{"OPXRC"}}{ Option exercise}
#'   \item{\code{"PTC"}}{ Pass Thru Charge}
#'   \item{\code{"PTR"}}{ Pass Thru Rebate}
#'   \item{\code{"REORG"}}{ Reorg CA}
#'   \item{\code{"SC"}}{ Symbol change}
#'   \item{\code{"SSO"}}{ Stock spinoff}
#'   \item{\code{"SSP"}}{ Stock split}
#' }
#' @examples 
#' account_activities(activity_type = "FILL")
#' @importFrom httr GET
#' @importFrom purrr compact map map_lgl
#' @importFrom rlang abort warn
#' @export
account_activities <-
  function(activity_type = NULL,
           date = NULL,
           until = NULL,
           after = NULL,
           direction = "desc",
           page_size = 50,
           page_token = NULL,
           live = as.logical(Sys.getenv("APCA-LIVE", FALSE))) {
    
  .dt <- purrr::compact(list(date = date, after = after, until = until))
  if (length(.dt) > 0) {
    .dt <- purrr::map(.dt, try_date)
    .na <- purrr::map_lgl(.dt, is.na)
    if (any(.na)) {
      rlang::abort(paste0("Check ", names(.na)[.na], " argument"))
    }
    
  }
  if (!is.null(activity_type)) activity_type <- toupper(activity_type)
  #Set URL & Headers
  headers = get_headers(live)
  .url <- get_url(
    c("account",
      "activities",
      activity_type),
    list(
      date = date,
      until = until,
      after = after,
      direction = direction,
      page_size = page_size,
      page_token = page_token
    ),
    live = live
  )
  
  #Send Request
  aa = httr::GET(url = .url, headers)
  out <- aa_transform(aa)
  return(out)
}


#' @title Account Portfolio History
#' @family Account
#' @description The [portfolio history endpoint](https://alpaca.markets/docs/api-documentation/api-v2/portfolio-history/) returns the timeseries data for equity and profit loss information of the account in a requested timespan (optional) for a given timeframe.
#' @details All \code{(Date/POSIXlt)} will parse correctly if in `YYYY-MM-DD` \href{https://www.iso.org/iso-8601-date-and-time-format.html}{RFC 3339} format or `(Datetime/POSIXct)`, `YYYY-MM-DD HH:MM` \href{https://www.iso.org/iso-8601-date-and-time-format.html}{ISO8601} format. Other formats will often work, but are not guaranteed to parse correctly.
#' @param period `(character/Duration/Period)` *Optional* The period of time with which the data will extend backwards from `date_end` in `number + unit` format. Such as `"1D"`, where `unit` can be `"D"` for day, `"W"` for week, `"M"` for month and `"A"/"Y"` for year. Defaults to `"1M"`. Accepts any number for multiplier. Non case-sensitive. Also accepts \code{\link[lubridate]{period}} objects.
#' @param timeframe `(character)` *Optional* The timeframe of the returned data in `"MT"` format where `M` is a multiplier and `T` is the timeframe from one of the following (non case-sensitive):
#' #' \itemize{
#'  \item{`'m'`/`'min'`/`'minute'`}{ (`multiplier` can be `1`/`5`/`15`)}
#'  \item{`'h'`/`'hour'`}{ `multiplier` will be `1`.}
#'  \item{`'d'`/`'day'`}{ (`multiplier` will be `1`)}
#' } 
#'If omitted, "`1Min`" for less than 7 days period, "`15Min`" for less than 30 days, or otherwise `"1D"`.
#' @param date_end \code{(Date/POSIXlt/Datetime(POSIXct)/character)} *Optional* See Details for formatting guidelines. The end date of the returned data. Defaults to the current market date (rolls over at the market open if `extended_hours = FALSE`, otherwise at 7am ET). *Note* that when `period = 1D` and `date_end` is supplied, the API returns no data.
#' @param extended_hours `(logical)` *Optional* If true, include extended hours in the result. This is effective only for timeframes less than 1D. Default `FALSE`
#' @return A [PortfolioHistory](https://alpaca.markets/docs/api-documentation/api-v2/portfolio-history/#portfoliohistory-entity) `(tibble)` Object with R amenable data types. *Note* that base value & timeframe can be accessed by calling `attr(out, "info")` on the returned tibble.
#' \itemize{
#'   \item{\code{timestamp}}{ \code{(Date/POSIXlt/Datetime(POSIXct)/character)} time of each data element, with the first element being the beginning of the time window.}
#'  \item{\code{equity}}{ \code{(numeric)} equity value of the account in dollar amount as of the end of each time window}
#'  \item{\code{profit_loss}}{ \code{(numeric)} profit/loss in dollar from the base value}
#'  \item{\code{profit_loss_pct}}{ \code{(numeric)} profit/loss in percentage from the base value}
#'  \item{\code{base_value}}{ \code{(numeric)} basis in dollar of the profit loss calculation}
#'  \item{\code{timeframe}}{ \code{(character)} time window size of each data element}
#' }
#' @examples 
#' # Get the previous year's returns for the paper account
#' account_portfolio("1y")
#' # Get portfolio history when the COVID-19 pandemic overtook the US
#' account_portfolio("3m", date_end = "2020-05-20")
#' @inheritParams account
#' @importFrom stringr str_extract str_detect regex
#' @importFrom lubridate `.__T__-:base` `.__T__+:base` as_date duration weeks days
#' @importFrom httr GET
#' @importFrom purrr iwalk compact
#' @export

account_portfolio <-
  function(period = NULL,
           timeframe = NULL,
           date_end = NULL,
           extended_hours = FALSE,
           live = as.logical(Sys.getenv("APCA-LIVE", FALSE))) {
    
  # Fix and detect args
  # check classes ----
  # Mon May 18 11:01:09 2020
  .vn <- list(period = c("character", "Period", "NULL"), timeframe = c("character", "NULL"), date_end = c("character", "Date", "Datetime", "POSIXct", "POSIXlt", "NULL"), extended_hours = "logical", live = "logical")
  .e <- environment()
  purrr::iwalk(.vn, ~{
    if (!inherits(get0(.y, inherits = F, envir = .e), .x)) rlang::abort(paste0(.y," must be one of ", paste0(.x, collapse = ", ")))
  })
  
  #  period ----
  # Mon May 18 10:53:29 2020
  if (is.null(period)) {
    .period <- "M"
    .lp <- "months"
    .pmultiplier <- 1
    message(paste0("`period` set to 1 Month"))
  } else if (isFALSE(inherits(period, c("Period", "Duration")))) {
    .period <- stringr::str_extract(period, "[A-Za-z]+$")
    .period <- substr(tolower(.period), 0, 1)
    if (.period == "d") {
      .period = "D"
      .lp <- "days"
    } else if (grepl("a|y", .period)) {
      .period = "A"
      .lp <- "years"
    } else if (.period == "w") {
      .period <- "W"
      .lp <- "weeks"
    } else if (.period == "m") {
      .period <- "M"
      .lp <- "months"
    } else {
      rlang::abort("`period` must have a unit of (D)ays, (W)eeks, (M)onths, or (Y)e(A)rs")
    }
    .pmultiplier <- as.numeric(stringr::str_extract(period, "^\\d+"))
  } else if (isTRUE(inherits(period, c("Period", "Duration")))) {
    if (isTRUE(inherits(period, c("Period")))) {
       .period <- toupper(stringr::str_extract(period, "[A-Za-z]"))
       .pmultiplier <- as.numeric(stringr::str_extract(period, "[0-9]"))
       .period <- ifelse(.period == "Y", "A", .period)
       .lp <- c(D = "days", A = "years", Y = "years", W = "weeks", M = "months")[.period]
    }
     
  }
  .lp <- lubridate::duration(.pmultiplier, units = .lp)
  .p_num <- as.numeric(factor(.period, levels = c("D", "W", "M", "A")))
  #  timeframe ----
  # Mon May 18 10:53:35 2020
  # if timeframe is blank
  
  if (!is.null(timeframe)) {
    if (stringr::str_detect(timeframe, "^\\d+")) {
      # Account for old argument style to timeframe
      .multiplier <- as.numeric(stringr::str_extract(timeframe, "^\\d+"))
      
    } 
    .timeframe <- substr(tolower(stringr::str_extract(timeframe, "[A-Za-z]+$")), 0, 1)
  } else {
    .timeframe <- ""
    .multiplier <- 1
  }
  
  
  # set minimum timeframes if null
  if (.lp <= lubridate::weeks(1) && !.timeframe %in% c("h","d") && .p_num < 2) {
    .timeframe <- "m"
    if (isTRUE(!.multiplier %in% c(1, 5, 15))) {
      message(paste0("multiplier can be 1,5, or 15 when `timeframe` is minutes. Multiplier set to 5."))
      .multiplier <- 5
    }
  } else if ((.lp > lubridate::weeks(1) && .lp <= lubridate::days(30)) && !.timeframe %in% c("h","d") && .p_num < 3) {
    .timeframe <- "m"
    if (isTRUE(!.multiplier %in% c(5, 15))) {
      message(paste0("multiplier can be 5 or 15 when `timeframe` is minutes and period or `date_end` to the present is > 7 days & < 30 days. Multiplier set to 5."))
      .multiplier <- 5
    }
  } else if (.lp > lubridate::days(30)) {
    .timeframe <- "d"
    if (isFALSE(stringr::str_detect(timeframe, stringr::regex(.timeframe, ignore_case = T)))) {
      message(paste0("`timeframe` must be day when `period` is greater than 30 days."))
    }
    
  }
  if (isTRUE(.multiplier != 1) && .timeframe == "d") {
    message(paste0("multiplier must be 1 when `timeframe` is day. Setting to 1."))
    .multiplier <- 1
  } 
  # Get the timeframe in human readable
  .timeframe <- list(m = c("Minutes", "Min"), h = c("Hours", "H"), d = c("Days", "D"))[[.timeframe]]
  
  #  date_end ----
  # Mon May 18 11:00:40 2020
  if (!is.null(date_end)) {
    `-` <- lubridate::`.__T__-:base`
    `+` <- lubridate::`.__T__+:base`
    .ac <- lubridate::as_date(account(live)$created_at)
    .date <- lubridate::as_date(try_date(date_end))
    if (.date <= .ac) {
      .date <- lubridate::as_date(.ac + .lp)
     rlang::warn(paste0("`date_end`: ",.date,", is before account creation date. `date_end` set to ", .ac, " + `period`:", .lp,"=", .date))
      
    }
  } else {
    .date <- NULL
  }
  

  # if either arg was null, message
  if (is.null(timeframe) || isFALSE(str_detect(timeframe, stringr::regex(.timeframe, ignore_case = T))) || isFALSE(str_detect(timeframe, stringr::regex(as.character(.multiplier))))) {
    message(paste0("Timeframe set to ", .multiplier," ", .timeframe[1]))
  }
  # Send request ----
  # Mon May 18 11:49:14 2020
  headers <- get_headers(live)
  .url <- get_url(c(
    "account",
    "portfolio",
    "history"
  ),
  list(
    period = paste0(.pmultiplier, .period),
    timeframe = paste0(.multiplier, .timeframe[2]),
    date_end = .date,
    extended_hours = extended_hours
  ),
  live = live)
  
  if (isTRUE(get0(".dbg", envir = .GlobalEnv, mode = "logical", inherits = F))) message(.url)
  .resp <- httr::GET(.url, headers)
  out <- port_transform(.resp)
  return(out)
}
