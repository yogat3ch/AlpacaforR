% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Account.R
\name{account}
\alias{account}
\alias{get_account}
\title{Get Account Details}
\usage{
account(live = as.logical(Sys.getenv("APCA-LIVE", FALSE)), v = 2)

get_account(live = as.logical(Sys.getenv("APCA-LIVE", FALSE)), v = 2)
}
\arguments{
\item{live}{\code{(logical)} TRUE / FALSE if you are connecting to a live account. Default to FALSE, so it will use the paper url if nothing was provided.}

\item{v}{\code{(integer)} The API version number. Defaults to 2. There is only the V2 API available for all endpoints other than Bars as of 2020-04-18. This argument is present to accommodate future API versions.}
}
\value{
Account \code{(list)} An \href{https://alpaca.markets/docs/api-documentation/api-v2/account/#account-entity}{Account} object list of length 26:
\itemize{
\item{\code{id}}{ \code{(character)} Account ID}
\item{\code{account_number}}{ \code{(character)} Account number}
\item{\code{status}}{ \code{(character)} See \href{https://alpaca.markets/docs/api-documentation/api-v2/account/#properties}{Account Endpoint Properties} for details}
\item{\code{currency}}{ \code{(character)} USD}
\item{\code{buying_power}}{ \code{(numeric)} Current available $ buying power; If multiplier = 4, this is your daytrade buying power which is calculated as \code{(last_equity - (last) maintenance_margin) * 4; If multiplier = 2, buying_power = max(equity - initial_margin,0) * 2; If multiplier = 1, buying_power = cash}}

\item{\code{cash}}{ \code{(numeric)} cash balance}
\item{\code{portfolio_value}}{ \code{(numeric)} Total value of cash + holding positions}
\item{\code{trading_blocked}}{ \code{(logical)} If true, the account is not allowed to place orders as a boolean.}
\item{\code{transfers_blocked}}{ \code{(logical)} If true, the account is not allowed to request money transfers as a boolean.}
\item{\code{account_blocked}}{ \code{(logical)} If true, the account activity by user is prohibited as a boolean.}
\item{\code{created_at}}{ \code{(Datetime/POSIXct)} Timestamp this account was created at.}
}
}
\description{
The accounts API serves important information related to an account, including account status, funds available for trade, funds available for withdrawal, and various flags relevant to an account's ability to trade. See the \href{https://alpaca.markets/docs/api-documentation/api-v2/account/}{Account} Endpoint in the API v2 Docs for full details.

\code{get_account} is deprecated please use \code{\link[AlpacaforR]{account}} instead.
}
\examples{
account()
# With default arguments equivalent to :
account(live = FALSE, v = 2)
# For access to live accounts, you must submit as live = TRUE
account(live = TRUE)
get_account()
}
\seealso{
Other Account: 
\code{\link{account_activities}()},
\code{\link{account_config}()},
\code{\link{account_portfolio}()}
}
\concept{Account}
\concept{Accounts}
