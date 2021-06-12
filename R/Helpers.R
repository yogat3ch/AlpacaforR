#  Re-exports ----
# Sun Sep 20 11:15:14 2020

#' @title NULL Replacement
#' @name grapes-or-or-grapes
#' @inherit rlang::`%||%`
#' @importFrom rlang `%||%`
#' @export
`%||%` <- rlang::`%||%`

#' @title Vectorized NA replacement
#' @name grapes-or-grapes
#' @inherit rlang::`%|%`
#' @importFrom rlang `%|%`
#' @export
`%|%` <- rlang::`%|%`

#' @name grapes-at-grapes
#' @inherit rlang::`%@%`
#' @importFrom rlang `%@%`
#' @export
`%@%` <- rlang::`%@%`

#' @title Default value for zero length variable
#' @name grapes-or-z-or-grapes
#' @description This infix replaces zero length variables with a default value
#' @param x (lhs) If `x` is length zero,
#' @param y (rhs)  will return `y`; otherwise returns `x`
#' @examples 
#' numeric(0) %|z|% NA_real_
#' @export
`%|z|%` <- function(lhs, rhs) {
  if (rlang::is_empty(lhs)) rhs else lhs
}

get_cred <- function(cred) {
  out <- Sys.getenv(cred, unset = NA)
  if (is.na(out)) rlang::abort(message = paste0(cred," is unset. Please set your API credentials as environmental variables. See vignette('Getting Started', 'AlpacaforR') for details."))
  out
}

#' @title Get the Alpaca API default live setting
#' @description `live` is a logical argument to many package functions that indicates whether to use the live account (`TRUE`) or paper account (`FALSE`). The default is set by environmental variable, see \link[AlpacaforR]{firstrun} for details on setting up the default live setting.
#' @return \code{(logical)} Alpaca API live setting
#' @export
get_live <- function() {
  live <- as.logical(get_cred("APCA-LIVE"))
}

#' @title Get an API key from Environmental Variables
#' @description `APCA-[LIVE/PAPER]-KEY` or `POLYGON-KEY`s are character vectors set in the Renviron file, see \link[AlpacaforR]{firstrun} for details on setting up these credentials.
#' @param live \code{(logical)} Alpaca live setting
#' @param api \code{(character)} `"a"/"p"` for Alpaca/Polygon respectively
#' @return \code{(character)} the requested key
#' @export
get_key <- function(live = get_live(), api = "a") {
  if (grepl("^a", api, ignore.case = TRUE))
    out <- get_cred(paste0("APCA-", ifelse(live, "LIVE", "PAPER"), "-KEY"))
  else
    out <- get_cred("POLYGON-KEY")
  out
}

#' @title Get an API secret from Environmental Variables
#' @description `APCA-[LIVE/PAPER]-SECRET` are character vectors set in the Renviron file, see \link[AlpacaforR]{firstrun} for details on setting up these credentials.
#' @param live \code{(logical)} Alpaca live setting
#' @return \code{(character)} the requested secret
#' @export
get_secret <- function(live = get_live()) {
  get_cred(paste0("APCA-", ifelse(live, "LIVE", "PAPER"), "-SECRET"))
}

# tax_form <- function(year, cost_basis = c("fifo", "lifo", "hc", "lc")[1]) {
#   tx <- account_activities("FILL", live = TRUE)
#   slider::slide(tx, ~{
#     if (.x$side == "sell") {
#        # Find an appropriate buy order to link it to
#        # Calculate cost basis
#        # Calcuate gain/loss
#        
#     }
#   })
#   # Output in 1099-B format
#   # Add gain/loss calculation options
#   # https://www.investopedia.com/terms/c/costbasis.asp
#   
# }


#' @title time_index
#' @description  Get the time index for a data.frame or xts
#' @param x \code{(tsibble/list with tibble)}
#' @param type 
#' \itemize{
#'   \item{\code{"character"/"c"}}{ (Default) \code{(character)} vector of the index}
#'   \item{\code{"value"/"v"}}{ \code{(Date/Datetime)} vector of values in the time index}
#'   \item{\code{"language"/"symbol"/"s"/"l"}}{ \code{(symbol)} for the time index}
#' }

#' @return \code{(character/numeric)} Either the name of the index, or the index itself based on `name` argument
#' @export

time_index <- function(x, type = "character"){
  
  if (inherits(x, "list")) x <- get_tibble(x)
  
  .type <- match_letters(type,
                         v = "value",
                         c = "character",
                         l = "language",
                         s = "symbol")
  
  
  if (tsibble::is_tsibble(x)) {
    out <- as.character(tsibble::index(x))
  } else {
    if (!inherits(x, "character")) .nm <- colnames(x)
    else
      .nm <- x
    out <- stats::na.exclude(stringr::str_extract(.nm, stringr::regex("^time$|^date$", ignore_case = TRUE)))
  }
  
  
  if (.type == "value") {
    out <- x[[out]]
  } else if (.type %in% c("language", "symbol")) {
    out <- rlang::sym(out)
  }
  return(out)
}


#' @title time_interval
#' @description Retrieve the time interval. If a `tsibble`, values are retrieved with `tsibble::interval`. If another object, the time index is detected and the Mode of the intervals is used.
#' @param x \code{(data.frame/tsibble)} input data
#' @return \code{(list)} with the following:
#' \itemize{
#'  \item{multiplier}{ \code{(numeric)} of the multiple of the interval period}
#'  \item{timeframe}{ \code{(character)} describing the interval}
#' }
#' @export

time_interval <- function(x) {
  
  if (tsibble::is_tsibble(x)) {
    i <- tsibble::interval(x)
    idx <- purrr::map_lgl(i, ~ .x > 0)
    out <- list(
      multiplier = unclass(i)[idx][[1]],
      timeframe = names(unclass(i)[idx])
    )
  } else {
    out <- if (!is.atomic(x)) x[[time_index(x)]] else x
    out <- list(
      multiplier = as.numeric(DescTools::Mode(purrr::map_dbl(1:30, ~{
        abs(difftime(out[.x], out[.x + 1]))
      })))
      ,  timeframe = gsub("s$", "", as.character(DescTools::Mode(purrr::map_chr(1:30, ~{
        units(difftime(out[.x], out[.x + 1]))
      }))))
    )
  }
  out
}


valid_date <- function(x, .out) {
  if (!inherits(.out, c("numeric"))) {
    .yout <- lubridate::year(.out)
  } else {
    .yout <- .out
  }
  tests <- purrr::map(list(
    isna = is.na(.out),
    ispast = .yout < 1792,
    isfuture = .yout > lubridate::year(Sys.Date()) + 50), ~.x & !is.na(x) & x != 0)
  
  if (purrr::some(tests, any)) {
    idx <- unique(do.call(c, purrr::map_if(tests, is.logical, which)))
    stop(
      paste(x[idx],"was parsed to", .out[idx], ". Is this expected?")
      , call. = FALSE)
  }
}

date_try.character <- function(x, tz) {
  .orders <- c("Ymd", "mdY", "dmY", "ymd", "mdy", "dmy")
  if (grepl("T", x)) {
    # if a character and datetime
    .out <- lubridate::parse_date_time(x, orders = c("YmdT", "YmdTz"), tz = tz)
  } else if (grepl("\\:", x)) {
    .out <- lubridate::parse_date_time(x, orders = paste(.orders,"R"), tz = tz)
  } else {
    # if a date
    .out <- lubridate::parse_date_time(x, orders = .orders, tz = tz)
  }
  return(.out)
}

date_try.default <- function(x, tz) {
  if (!is.null(tz)) .out <- lubridate::with_tz(x, tzone = tz) else x
} 

date_try.integer <- date_try.double <- function(x, tz) {
  if (all(dplyr::between(x, 1792, 3000))) {
    # Handle years
    .out <- lubridate::years(x - 1970) + lubridate::origin
  } else if (all(x < 100000)) {
    # Handle Dates
    .out <- lubridate::as_date(x, origin = lubridate::origin)
  } else {
    # Handle datetimes
    .out <- lubridate::as_datetime(signif(x / 10 ^ ceiling(log10(x)), 10) * 10 ^ 10, origin = lubridate::origin, tz = tz)
  }
  return(.out)
}

date_try <- function(x, tz = Sys.timezone()) {
  if (all(is.na(x))) return(x)
  UseMethod("date_try")
}

# try_date ----
# Sat Apr 18 11:55:17 2020
#' @title Reliable Date/Datetime conversion for most time representations from the \href{https://polygon.io/}{Polygon.io} & \href{https://alpaca.markets/}{Alpaca.markets} APIs
#' @description Attempts to coerce input to a valid \code{\link[lubridate]{Date} or \link[lubridate]{POSIXct}} object.
#' @param .x \code{(numeric/character/Date/Datetime)} input vector
#' @param timeframe \code{(character)} The timeframe of the `.x` vector. See \link[AlpacaforR]{market_data}.
#' @param tz \code{(character)} Timezone. See \link[base]{OlsonNames} for valid strings. \strong{Default NULL}
#' @export

try_date <- function(.x, timeframe = "day", tz = NULL) {
  timeframe <-
    match_letters(
      timeframe,
      mi = "minute",
      ho = "hour",
      da = "day",
      we = "week",
      Mo = "month",
      qu = "quarter",
      ye = "year",
      n = 2
    )
  .out <- withCallingHandlers(date_try(.x, tz), warning = rlang::cnd_muffle, message = rlang::cnd_muffle) 
  if (!timeframe %in% c("minute", "hour")) { 
    .fn <- switch(as.character(timeframe),
                  day = lubridate::as_date,
                  week = tsibble::yearweek,
                  month = tsibble::yearmonth,
                  quarter = tsibble::yearquarter,
                  year = identity
    )
    .out <- suppressWarnings(rlang::exec(.fn, x = .out))
  }
  
  valid_date(.x, .out)
  return(.out)
}

#' @title Is object a `try-error`?
#' @description detects `try-error`
#' @param x input object
#' @return \code{(logical)}
#' @export

is_error <- function(x) inherits(x, "try-error")

#' @title Match the first `n` letters to supplied arguments
#' @description Case insensitive matching of argument to possibilities provided in ellipsis.
#' @param x \code{(character)} to match on
#' @param ... \code{(character)} vectors to match against
#' @param n \code{(numeric)} how many characters of `x` to use in matching. Set to `NULL` to use all
#' @param multiple \code{(logical)} are multiple matches allowed? If `FALSE` (Default) only the first match is returned.
#' @inheritParams base::grep
#' @param capitalize \code{(logical)} whether to capitalize the result
#' @return \code{(character)} vector of matches
#' @export

match_letters <- function(x, ..., n = 1, multiple = FALSE, ignore.case = FALSE, capitalize = FALSE) {
  if (!is.character(x)) return(x)
  if (!is.null(n))
    x <- substr(x, 0, n)
  if (is.null(x)) {
    out <- x
  } else {
    out <- tryCatch(grep(ifelse(length(x) > 1, paste0("^",x, collapse = "|"), paste0("^" ,x)), unlist(rlang::dots_list(...), use.names = FALSE), perl = TRUE, value = TRUE, ignore.case = ignore.case),
                    error = function(e) {
                      message(paste0(e))
                    })
    if (!multiple)
      out <- out[1]
    
    if (capitalize && !is.null(out))
      out <- purrr::map_chr(out, ~purrr::when(nchar(.x) == 1,. ~ toupper(.x), ~ gsub("^(\\w)(\\w+)","\\U\\1\\L\\2", .x, perl = TRUE)))
  }
  out
}
