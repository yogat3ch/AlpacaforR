
# calendar ----
# Wed Apr 22 20:39:32 2020
#' @family Calendar
#' @title Get Market Calendar Times
#' 
#' @description The calendar API serves the full list of market days from 1970 to 2029. It can also be queried by specifying a start and/or end time to narrow down the results. In addition to the dates, the response also contains the specific open and close times for the market days, taking into account early closures. See [Calendar](https://alpaca.markets/docs/api-documentation/api-v2/calendar/) Endpoint for details.
#' @param from `(Date/Datetime/POSIXct/character)` in YYYY-MM-DD format. Starting date of request. Defaults to today. Calendar data goes back to 1970. If blank, defaults to today's date.
#' @param to `(Date/Datetime/POSIXct/character)` in YYYY-MM-DD format. Ending date of request. Default to today. Calendar data goes to 2029. If blank, defaults to today's date.
#' @return Calendar \code{tibble} A [Calendar](https://alpaca.markets/docs/api-documentation/api-v2/calendar/#calendar-entity) Object consisting of the following:
#' \itemize{
#'   \item{\code{date}}{ \code{(character)} vector of trading days in the time range}
#'   \item{\code{open}}{ \code{(character)} The time the market opens at on this date in hour:min format as a string.}
#'   \item{\code{close}}{ \code{(character)} The time the market closes at on this date in hour:min format as a string.}
#'   \item{\code{session_open}}{ \code{(character)} The start time of the session in hh:mm format}
#'   \item{\code{session_close}}{ \code{(character)} The closing time of the session in hh:mm format.}
#'   \item{\code{day}}{ \code{(Interval)} An interval of the market day \code{open \%--\% close}.}
#'   \item{\code{session}}{ \code{(Interval)} An interval of the market session \code{session_open \%--\% session_close}.}
#'   \item{\code{dow}}{ \code{(character)} Three letter abbreviation of the day of the week as an ordered factor with levels Sunday - Saturday. }
#' }
#'@seealso \link[lubridate]{Interval-class}
#' @examples 
#' # Get all dates:
#' calendar()
#' # Get specific date range:
#' calendar(from = "2019-01-01", to = "2019-04-01")
#' @export


calendar <- function(from = NULL, to = NULL){

  bounds <- list(from = from, to = to)
  #If dates were given, make sure they are in a string format. I add plus 1 to the "to" date, or it will always return one day before the day "to"
  # 2020-03-29T18:25:01 This appears to be fixed, removing the +1
  .null <- purrr::map_lgl(bounds, is.null)
  # Check for null values and warn if NULL
  if (any(.null)){
    message(paste0(paste0("`",names(.null)[.null],"`", collapse = ", "), " arg(s) is/are NULL, setting from/to to ", lubridate::today()))
    bounds <- purrr::map(bounds, ~{
      if (is.null(.x)) lubridate::today() else try_date(.x, timeframe = "day")
    })
  }
  # Check for weekend values and warn if weekend
  purrr::iwalk(bounds, ~{
    if (lubridate::wday(.x) %in% c(1,7)) {
      message(paste0(.y, " is a ",lubridate::wday(.x, label = TRUE, abbr = FALSE),", Calendar API will return data for the previous Friday or following Monday"))
    }
  })
  headers = get_headers()
  .url <- get_url("calendar",
                  list(start = as.character(bounds[[1]]),
                       end = as.character(bounds[[2]])))

  calendar <- cal_transform(httr::GET(url = .url, headers))
  
  return(calendar)
}


cal_transform <- function(resp, .tz = "America/New_York") {
  calendar =  response_text_clean(resp)
  calendar <- dplyr::mutate(calendar, date = lubridate::as_date(date)) %>%
    dplyr::mutate(
      dplyr::across(dplyr::starts_with("session"),
                    ~ paste0(
                      stringr::str_sub(.x, start = 1, end = 2),
                      ":",
                      stringr::str_sub(.x, start = 3, end = 4)
                    )
      )) %>%
    dplyr::mutate(
      day = lubridate::interval(
        start = lubridate::ymd_hm(paste(date, open), tz = .tz),
        end = lubridate::ymd_hm(paste(date, close), tz = .tz)
      ),
      session = lubridate::interval(
        start = lubridate::ymd_hm(paste(date, session_open), tz = .tz),
        end = lubridate::ymd_hm(paste(date, session_close), tz = .tz)
      ),
      dow = lubridate::wday(date, label = T)
    ) %>%
    dplyr::select(date, everything(), dow, day, session)
}
