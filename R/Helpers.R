
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
  if (all(x < 100000)) {
    .out <- lubridate::as_date(x, origin = lubridate::origin)
  } else {
    .out <- lubridate::as_datetime(signif(x / 10 ^ ceiling(log10(x)), 10) * 10 ^ 10, origin = lubridate::origin, tz = tz)
  }
  return(.out)
}

date_try <- function(x, tz) {
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
  .out <- withCallingHandlers(date_try(.x, tz), warning = rlang::cnd_muffle, message = rlang::cnd_muffle) 
  if (!timeframe %in% c("minute", "hour")) { 
    .fn <- switch(as.character(timeframe),
                  day = lubridate::as_date,
                  week = tsibble::yearweek,
                  month = tsibble::yearmonth,
                  quarter = tsibble::yearquarter,
                  year = lubridate::year
    )
    .out <- suppressWarnings(rlang::exec(.fn, x = .out))
  }
  
  valid_date(.x, .out)
  return(.out)
}
