# account ----
# Wed Apr 22 20:26:32 2020
#' @family Account
#' @title Get Account Details
#'
#' @description The accounts API serves important information related to an account, including account status, funds available for trade, funds available for withdrawal, and various flags relevant to an account's ability to trade. See the [Account](https://alpaca.markets/docs/api-documentation/api-v2/account/) Endpoint in the API v2 Docs for full details.
#' @param live \code{(logical)} TRUE / FALSE if you are connecting to a live account. Default to FALSE, so it will use the paper url if nothing was provided.
#' @param v \code{(integer)} The API version number. Defaults to 2. There is only the V2 API available for all endpoints other than Bars as of 2020-04-18. This argument is present to accommodate future API versions.
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
#' account(live = FALSE, v = 2)
#' # For access to live accounts, you must submit as live = TRUE
#' account(live = TRUE)
#' @importFrom httr GET parse_url build_url
#' @importFrom purrr map_if
#' @importFrom lubridate as_datetime
#' @export
account <- function(live = FALSE, v = 2){
  #Set URL & Headers
  .url = httr::parse_url(get_url(live))
  headers = get_headers(live)
  .url$path <- list(version = paste0("v", v),
                    "account")
  .url <- httr::build_url(.url)
  #Send Request
  out = httr::GET(url = .url, headers)
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
account_config <- function(dtbp_check = NULL, no_shorting = NULL, pdt_check = NULL, suspend_trade = NULL, trade_confirm_email = NULL, live = FALSE, v = 2) {
  #Create body with order details, most common is a named list 
  .def <- ifelse(length(dtbp_check == "default") == 0, F, dtbp_check == "default")
  if (.def) {
    bodyl <- list(dtbp_check = "entry", trade_confirm_email = "all", pdt_check = "entry", suspend_trade = FALSE, no_shorting = FALSE)
  } else {
    bodyl <- purrr::compact(list(dtbp_check = dtbp_check, no_shorting = no_shorting, suspend_trade = suspend_trade, trade_confirm_email = trade_confirm_email))
  }
  
  #Set URL & Headers
  url = get_url(live)
  headers = get_headers(live)
  
  
  if (length(bodyl) > 0) {
    # if configuration options are set
    account_config = httr::PATCH(url = paste0(url,"/",paste0("v",v),"/account/configurations"), body = bodyl, encode = "json", headers)
  } else {
    # if no options are set, just return the account config
    account_config = httr::GET(url = paste0(url,"/",paste0("v",v),"/account/configurations"), headers)
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
#' @param direction asc or desc, default is desc.
#' @param page_size The maximum number of entries to return in the response.
#' @param page_token The ID of the end of your current page of results.
#' @inheritParams account
#' @return \code{TradeActivity} \code{(tibble)} [TradeActivity](https://alpaca.markets/docs/api-documentation/api-v2/account-activities/#tradeactivity-entity) Object or  a [NonTradeActivity](https://alpaca.markets/docs/api-documentation/api-v2/account-activities/#nontradeactivity-entity) Object. See Details.
#' \itemize{
#'  \item{\code{id}}{ \code{(character)} An id for the activity. Always in "
#'  ::" format. Can be sent as `page_token` in requests to facilitate the paging of results.}
#'  \item{\code{activity_type}}{ \code{(character)} "FILL"}
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
#' @importFrom httr GET parse_url build_url
#' @importFrom purrr compact map map_lgl
#' @importFrom tibble as_tibble
#' @importFrom dplyr mutate_at vars
#' @importFrom lubridate as_datetime as_date
#' @importFrom magrittr `%>%`
#' @importFrom rlang abort warn
#' @export
account_activities <- function(activity_type = NULL, date = NULL, until = NULL, after = NULL, direction = "desc", page_size = 50, page_token = NULL, live = FALSE, v = 2){
  .dt <- purrr::compact(list(date = date, after = after, until = until))
  if (length(.dt) > 0) {
    .dt <- purrr::map(.dt, try_date)
    .na <- purrr::map_lgl(.dt, is.na)
    if (any(.na)) {
      rlang::abort(paste0("Check ", names(.na)[.na], " argument"))
    }
    
  }
  #Set URL & Headers
  .url = httr::parse_url(get_url(live))
  headers = get_headers(live)
  .url$path <- purrr::compact(list(paste0("v",v),
                                   "account",
                                   "activities",
                                   activity_type))
  .url$query <- list(date = date,
                     until = until,
                     after = after,
                     direction = direction,
                     page_size = page_size,
                     page_token = page_token)
  .url <- httr::build_url(.url)
  #Send Request
  aa = httr::GET(url = .url, headers)
  out <- aa_transform(aa)
  return(out)
}
#----------------------------------------------------------------------------------------------
#NEW for V2
#account_activities(activity_type = "FILL")
