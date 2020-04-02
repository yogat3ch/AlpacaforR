# Internals:  Sun Jan 12 10:20:31 2020 ----

# response_text_clean Clean data from Server Response function  ----
# Sun Mar 29 16:02:00 2020
#' 
#' Clean the response text (usually in unreadable json) and convert to a readable format using fromJSON. 
#' @param dat The response from our server GET request usually in a json format.
#' @keywords internal
#' @return The response in a readable format as a list.
response_text_clean <- function(dat){
  
  dat = httr::content(dat, as = "text", encoding = "UTF-8")
  dat = jsonlite::fromJSON(dat)
  return(dat)
}
# get_url for Server Request ----
# Sun Mar 29 16:02:51 2020
#' get_url
#' 
#' Get the correct URL for the Server Request that is sent to interact with the API. If the user is on a paper account, then the paper account URL will be returned. 
#' @keywords internal
#' @return The correct URL according to account type (live or paper) that will be sent in the API request.
get_url <- function(live=FALSE){
  
  url <- ifelse(live, 
                "https://api.alpaca.markets",
                "https://paper-api.alpaca.markets")
  
  return(url)
}
# UPDATED for V2 ----
# get_url_poly Get Polygon URL for Server Request function  ----
# Sun Mar 29 16:04:24 2020
#' 
#' Get Polygon's URL for the Server Request that is sent to interact with the API.
#' @keywords internal
#' @return The correct URL for Polygon's API.
get_url_poly <- function(){
  url = "https://api.polygon.io" 
  return(url)
}
# get_headers Get Headers for Server Request function  ----
# Sun Mar 29 16:05:32 2020  
#'
#' @keywords internal
#' @return The correct headers that will be sent in the API request.
#' @param live TRUE / FALSE if you are connecting to a live account. Default to NULL, so it will use the key variables set by the user for their respective paper account. Set live = TRUE to find your live key credentials.
get_headers <- function(live=FALSE){
  
  ifelse(live, 
         headers <- httr::add_headers('APCA-API-KEY-ID' = Sys.getenv("APCA-LIVE-API-KEY-ID"), 
                                      'APCA-API-SECRET-KEY' = Sys.getenv("APCA-LIVE-API-SECRET-KEY")),
         
         headers <- httr::add_headers('APCA-API-KEY-ID' = Sys.getenv("APCA-PAPER-API-KEY-ID"), 
                                      'APCA-API-SECRET-KEY' = Sys.getenv("APCA-PAPER-API-SECRET-KEY"))
  )
  return(headers)
}

# Helper functions for get_bars ----
# Sat Mar 28 09:40:23 2020
# int_math  ----
# Sun Mar 29 16:06:55 2020
#' int_math 
#' 
#' Function for working with intersections, setdiff, and raw difference of intervals
#' @param int1 `(interval)` object. 
#' @param int2 `(interval)` object.
#' @param math `(character)` indicating what set function to perform. See details for options.
#' @details \itemize{
#' \item{If `"setdiff"` the function mirrors \link[base]{setdiff}. IE if it is assumed that \eqn{int1 \supseteq int2}, then the function will return \eqn{int1 \nsupseteq int2}.}
#' \item{ if `"difference"`, no assumptions are made about the nature of the sets and the function returns \eqn{int1 \cup int2 - int1 \cap  int2}}
#' \item{ if `"intersection"`, the intersection is returned. \eqn{int1 \cap int 2}}
#' }
#' @return `(list or interval)` If `math = "setdiff"` or `"difference"` a list of interval objects with the leading and lagging intervals (if they exist).  If `math = "intersection"` an interval object of the intersection.
#' @keywords internal
#' @import lubridate purrr

int_math <- function(int1, int2, math = "setdiff") {
  
  .ints <- purrr::map(list(int1 = int1, int2 = int2), lubridate::int_standardize)
  if (!lubridate::int_overlaps(int1, int2)) {
    stop("Intervals do not overlap")
  }
  math <- c(i = "intersect", s = "setdiff", d = "difference")[substr(math, 1, 1)]
  # Which has the earliest start date
  if (math == "intersect") {
    # order the starts
    .ord <- order(do.call(c, purrr::map(.ints, lubridate::int_start)))
    # get the latest start
    .begin <- lubridate::int_start(.ints[.ord][[2]])
    # order the ends
    .ord <- order(do.call(c, purrr::map(.ints, lubridate::int_end)))
    # get the earliest end
    .end <- lubridate::int_end(.ints[.ord][[1]])
    .out <- lubridate::interval(.begin, .end) 
  } else if (math == "setdiff" || math == "difference") {
    # If the start times of either interval are not identical
    if (!do.call(identical, setNames(purrr::map(.ints, lubridate::int_start), c("x","y")))) {
      # Get the leading difference
      .ord <- order(do.call(c, purrr::map(.ints, lubridate::int_start)))
      if (math == "setdiff") {
        # if what is supposed to be the superset interval is contained within the subset interval warn
        if (lubridate::`%within%`(.ints[[1]], .ints[[2]])) {
          # Let the user know so they can switch the argument
          message(paste0("int1 is contained within int2. Raw difference between intervals will be returned."))
        } else {
          .lead <- lubridate::interval(lubridate::int_start(.ints[[1]]), lubridate::int_start(.ints[[2]]))
          # If the subset int2 has the earlier start return NULL
          if (sign(lubridate::as.duration(.lead)) == -1) {
            .lead <- NULL
          }
        }
      } else {
        .lead <- lubridate::interval(lubridate::int_start(.ints[.ord][[1]]), lubridate::int_start(.ints[.ord][[2]]))
      }
    } else {
      .lead <- NULL
    }
    # If the end times of either interval are not identical
    if (!do.call(identical, setNames(purrr::map(.ints, lubridate::int_end), c("x","y")))) {
      # Get the lagging difference
      .ord <- order(do.call(c, purrr::map(.ints, lubridate::int_end)))
      if (math == "setdiff") {
        # if the superset interval (int1) is indeed the superset
        if (!lubridate::`%within%`(.ints[[1]], .ints[[2]])) {
          .lag <- lubridate::interval(lubridate::int_end(.ints[[2]]), lubridate::int_end(.ints[[1]]))
          # If the subset int2 has the later end return NULL
          if (sign(lubridate::as.duration(.lag)) == -1) {
            .lag <- NULL
          }
        }
      } else {
        .lag <- lubridate::interval(lubridate::int_end(.ints[.ord][[1]]), lubridate::int_end(.ints[.ord][[2]]))
      }
    } else {
      .lag <- NULL
    }
    .out <- purrr::compact(list(lead_int = .lead, lag_int = .lag)) 
  }
  
  return(.out)
}

# bars_bounds ----
# Sun Mar 29 16:07:18 2020
#'bars_bounds
#'
#'Internal function for fixing input date boundaries. The function is intended for use in an environment where its parameters already exist as it will use the variables from it's parent environment. If non-required params need to be specified explicitly, **all params must be named**.
#'@param from See \link[AlpacaforR]{get_bars}
#'@param to See \link[AlpacaforR]{get_bars}
#'@param after See \link[AlpacaforR]{get_bars}
#'@param until See \link[AlpacaforR]{get_bars}
#'@param v See \link[AlpacaforR]{get_bars}
#'@return `(list)` A list with two Date/Datetime items that are boundaries for the date range necessary to call the API with to retrieve the appropriate data from the specified API. Items will have names corresponding to the non-NULL `from/to/after/until` arguments in the calling environment or supplied to the function.
#'@keywords internal
#'@importFrom purrr imap map compact
#'@import lubridate

bars_bounds <- function(..., pf = parent.frame()) {
  #trickery to get the variables from the calling environment
  .vars <- list(...)
  .n <- try(names(.vars))
  if (class(.n) == "try-error" || is.null(.n)) {
    .date_vars <- purrr::map(c(from = "from", to = "to", after = "after", until = "until"), get0, envir = pf)
    rm(pf)
  } else if (all(c("from", "to") %in% .n) || all(c("after", "until") %in% .n)) {
    .date_vars <- .vars[c(from = "from", to = "to", after = "after", until = "until")]
    rm(pf)
  }
  
  
  # Remove NULL arguments
  .bounds <- purrr::compact(.date_vars)
  
  # Coerce to floor/ceiling dates/datetimes or fail with NA
  .bounds <- purrr::imap(.bounds, ~{
    if (is.character(.x) && stringr::str_detect(.x, ":")) {
      # if a character and date/time
      .out <- lubridate::ymd_hm(.x, tz = Sys.timezone())
    } else if (is.character(.x)) {
      .out <- lubridate::ymd(.x, tz = Sys.timezone())
    } else if (any(lubridate::is.Date(.x) ||
                   lubridate::is.POSIXct(.x) ||
                   lubridate::is.POSIXlt(.x))) {
      .out <- .x
    } else {
      # If the format is incorrect return NA
      return(NA)
    }
    .q <- F
    # Floors or ceilings
    if (which(.tf_order %in% timeframe) < 3) {
      .unit <- paste(1, "day")
    } else if (as.character(timeframe) == "quarter") {
      .q <- T
      .unit <- paste(3 * multiplier, "months")
      timeframe <- "month"
      multiplier <- 3 * multiplier
    } else {
      .unit <- paste(multiplier, timeframe)
    }
    
    if (.y == "from" || .y == "after") {
      .out <- lubridate::floor_date(.out, .unit)
      if (.q) {
        .out <- .out - lubridate::days(1)
      }
    } else {
      .out <- lubridate::ceiling_date(.out, .unit, change_on_boundary = T) + lubridate::duration(multiplier, timeframe)
    }
    # For requests to the v2 API where aggregates are yearly, the floor date needs to be 12/31
    if (timeframe == "year") {
      .out <- .out - lubridate::days(1)
    }
    .out
  }) 
  # API version specific bounds
  if (v == 1) {
    .bounds <- purrr::imap(.bounds, ~{
      if (stringr::str_detect(.x, ":")) {
        .dt <- format(lubridate::as_datetime(.x, tz = Sys.timezone()), "%Y-%m-%dT%H:%M:%S%z")
        # If it's a datetime already, just change to iso8601
        
      } else if (.y == "from" || .y == "after"){
        # If it's the start/after - use the beginning of the trading day
        .dt <- format(lubridate::as_datetime(paste0(.x, " 07:00"), format = "%Y-%m-%d %H:%M", tz = Sys.timezone()), "%Y-%m-%dT%H:%M:%S%z")
        
      } else {
        # If its the end/until - use the end of the trading day
        .dt <- format(lubridate::as_datetime(paste0(.x, " 19:00"), format = "%Y-%m-%d %H:%M", tz = Sys.timezone()), "%Y-%m-%dT%H:%M:%S%z")
      }
      paste0(stringr::str_sub(.dt, 1, -3),":", stringr::str_sub(.dt, -2, nchar(.dt)))
    })
  } else if (v == 2) {
    .bounds <- purrr::map(.bounds, lubridate::as_date)
  }
  browser(expr = length(.bounds) == 0)
  return(.bounds)
}

# bars_url ----
# Sun Mar 29 16:07:34 2020
#'bars_url
#'
#'Internal function for generating query urls. The function is intended for use in an environment where its parameters already exist. If non-required params need to be specified explicitly, **all params must be named**.
#'@param ticker See \link[AlpacaforR]{get_bars}
#'@param .bounds Output from \[AlpacaforR]{bars_bounds}. If the output is assigned to `.bounds` in the calling environment this object will be used.
#'@param limit *v1 only* `See \link[AlpacaforR]{get_bars}
#'@param multiplier See \link[AlpacaforR]{get_bars}
#'@param timeframe See \link[AlpacaforR]{get_bars}
#'@param v See \link[AlpacaforR]{get_bars}
#'@return `(character)` URL to be called with \link[AlpacaforR]{bars_get}
#'@keywords internal
#'@importFrom httr parse_url build_url
#'@importFrom purrr map_chr
#'@importFrom lubridate as_date

bars_url <- function(...) {
  list2env(list(...), environment())
  # Format ticker:  Thu Mar 26 08:48:03 2020 ----
  if (v == 1) {
    .ticker = ifelse(length(ticker) > 1, paste0(trimws(ticker), collapse = ","), ticker)
  } else if (v == 2) {
    .ticker = ticker
  }
  #Limit :  Thu Mar 26 08:50:30 2020 ----
  # If limit is null or full = T OR if limit > 1000 then set at 1000 for v1
  if (v == 1) {
    if (is.null(limit) || isTRUE(limit > 1000)) {
      if (isTRUE(limit > 1000)) message("Limit cannot exceed 1000, coercing to 1000.")
      limit <- 1000
    }  
  }
  if (v == 1) {
    url = httr::parse_url("https://data.alpaca.markets") #Pricing data uses unique URL, see market data API documentation to learn more
    timeframe <- ifelse(timeframe == "minute", "Min", "day")
    timeframe <- ifelse(timeframe == "Min", paste0(multiplier, timeframe), timeframe)
    #full = F Make API requests:  Tue Mar 17 21:37:35 2020 ----
    url$path <- list("v1", "bars", timeframe)
    # NULL Values are automatically dropped, so only the set boundaries will remain
    
    url$query <- list(symbols = .ticker,
                      limit = limit,
                      start = .bounds$from,
                      end = .bounds$to,
                      after = .bounds$after,
                      until = .bounds$until
    )
    # Build the url
    url <- URLdecode(httr::build_url(url))
  } else if (v == 2) {
    url <- purrr::map_chr(setNames(.ticker, .ticker), ~{
      url = httr::parse_url(get_url_poly())
      url$path <- list(
        v = "v2",
        ep = "aggs",
        "ticker",
        ticker = .x,
        "range",
        multiplier = multiplier,
        timeframe = timeframe,
        from = as.character(lubridate::as_date(.bounds[[1]])),
        to = as.character(lubridate::as_date(.bounds[[2]]))
      )
      url$query <- list(unadjusted = unadjusted,
                        apiKey = Sys.getenv("APCA-LIVE-API-KEY-ID"))
      url <- httr::build_url(url)
    })
  }
  return(url)
}

# bars_get ----
# Sun Mar 29 16:07:49 2020
#' bars_get
#' 
#' Internal function for retrieving data from Alpaca API and outputting data.frame with query attribute. 
#' @param url `(character)` the url rendered by \link[AlpacaforR]{bars_url}
#' @return `(list)` See \link[AlpacaforR]{get_bars} as the output is the same.
#' @keywords internal
#' @importFrom magrittr "%>%"
#' @importFrom purrr iwalk
#' @import dplyr httr lubridate
bars_get <- function(url, ...) {
  list2env(list(...), environment())
  if (v == 1) {
    headers = get_headers()
    message(paste0("Retrieving\n", url))
    agg_quote = httr::GET(url = url, headers)
    .query <- list()
    .query$status_code <- agg_quote$status_code
    .query$url <- url
    .query$ts <- lubridate::with_tz(lubridate::parse_date_time(agg_quote[["headers"]][["date"]], "a, d b Y T"), tz = Sys.timezone())
    if (agg_quote$status_code != 200) {
      message(paste(url, "\nReturned status code", agg_quote$status_code, "- Returning NULL"))
      return(NULL)
    }
    agg_quote = response_text_clean(agg_quote)
    purrr::iwalk(agg_quote, ~{
      if (nrow(.x) == 0) {
        message(paste(url, "\nWarning:", .y ,"returned no results"))
      }  
    })
    
    bars <- bars_transform(agg_quote)
    # add query metadata to match v2 api
    bars <- purrr::map(bars, ~{
      attr(.x, "query") <- .query  
      .x
    })
    
    
    
  } else if (v == 2) {
    # get for v2 API
    bars <- purrr::imap(url, ~{
      message(paste0("Retrieving ", .y, ":\n", url))
      #Send Request
      agg_quote = httr::GET(url = url)
      # Save the query meta-date for appending to the output df
      .query <- list()
      .query$status_code <- agg_quote$status_code
      .query$url <- url
      .query$ts <- lubridate::with_tz(lubridate::parse_date_time(agg_quote[["headers"]][["date"]], "a, d b Y T"), tz = Sys.timezone())  
      if (agg_quote$status_code != 200) {
        message(paste("Ticker", .x, "returned status code", agg_quote$status_code, "- Returning NULL"))
        return(NULL)
      }
      agg_quote = response_text_clean(agg_quote)
      if (length(agg_quote$results) == 0) {
        message(paste("Ticker", .x, "returned no data", "- Returning response metadata only"))
        return(agg_quote[1:5])
      }
      # Get the query meta-info returned by the API
      query <- append(agg_quote[1:5], .query)
      agg_quote$results$t = agg_quote$results$t/1000
      
      agg_quote <- bars_transform(agg_quote["results"])$results
      attr(agg_quote, "query") <- query
      return(agg_quote)
    })
  }
  return(bars)
}

# bars_expected ----
#' bars_expected
#' 
#' An internal function for use when param `full = T` in \link[AlpacaforR]{get_bars} that creates a vector of expected Date/Datetimes that are expected from a given call to either v1 or v2 API. The function is intended for use in an environment where its parameters already exist. If non-required params need to be specified explicitly, **all params must be named**.
#' @param bars `(list)` (Required) Output from \link[AlpacaforR]{bars_get}
#' @param v See \link[AlpacaforR]{get_bars}
#' @param .bounds See \link[AlpacaforR]{bars_bounds}
#' @param multiplier See \link[AlpacaforR]{get_bars}
#' @param timeframe  See \link[AlpacaforR]{get_bars}
#' @return `(Date/Datetime)` A vector of expected date/datetimes based on the `.bounds`, `timeframe` and `multiplier`
bars_expected <- function(bars, ...) {
  list2env(list(...), environment())
  #Calendar expansion for segmenting queries and data completeness check:  Thu Mar 26 08:50:42 2020 ----
  #Get the trading days in between the sequence
  .cal <- get_calendar(.bounds[[1]], .bounds[[2]]) %>%
    dplyr::mutate_at(dplyr::vars(date), lubridate::ymd) %>% 
    dplyr::mutate(
      day = lubridate::interval(
        start = lubridate::ymd_hm(paste(date, open), tz = Sys.timezone()),
        end = lubridate::ymd_hm(paste(date, close), tz = Sys.timezone())
      ),
      session = lubridate::interval(
        start = lubridate::ymd_hm(paste(date, session_open), tz = Sys.timezone()),
        end = lubridate::ymd_hm(paste(date, session_close), tz = Sys.timezone())
      ),
      dow = lubridate::wday(lubridate::as_date(date), label = T)
    ) %>%
    dplyr::select(date, dow, day, session, everything()) %>%
    dplyr::filter(date >= .bounds[[1]] & date <= lubridate::now())
  
  # Set the parameters for seq and lubridate durations
  if(.tf_num == 6) {
    .by <- paste(3 * multiplier, "months")
    .timeframe <- "months"
    .multiplier <- multiplier * 3
  } else {
    .by <- paste0(multiplier," ", timeframe, "s")
    .multiplier <- multiplier
    .timeframe <- timeframe
  }
  
  .expected <- purrr::imap(bars, ~{
    .cutoff <- attr(.x, "query")$ts
    # if there is data 
    if(nrow(.x) > 0 && .tf_num > 3) {
      #start the sequence at one interval prior to the starting date in the data, and seq to the upper bound
      .expected <- c(.x$time[1] - lubridate::duration(.multiplier, .timeframe),
                     seq(from = as.POSIXct(.x$time[1], tz = Sys.timezone()), to = as.POSIXct(.bounds[[2]], tz = Sys.timezone()), by = .by)
      )
      .expected <- subset(.expected, subset = .expected >= .bounds[[1]] & .expected <= .cutoff)
    } else {
      if (.tf_num < 3) {
        # interval length .by
        .by <- paste0(.multiplier, " ", ifelse(.timeframe == "minute", substr(as.character(.timeframe), 1, 3), as.character(.timeframe)),"s")
        # units for floor/ceiling
        .units <- paste0(
          ifelse(.timeframe == "minute", 30, 1), # 30 min baseline if tf = mins
          " ",
          ifelse(
            .timeframe == "minute",
            substr(as.character(.timeframe), 1, 3), # 1 hour base if tf = hours
            as.character(.timeframe)
          ),
          "s"
        )
        .expected <- purrr::map(.cal$day, ~{
          .out <- seq(from = lubridate::ceiling_date(lubridate::int_start(.x), .units), to = lubridate::ceiling_date(lubridate::int_end(.x), .units), by = .by)
          
          
          .out[.out >= .bounds[[1]] & .out <= .cutoff]
        }) %>% do.call(c, .)
      } else if (.tf_num == 3) {
        # dat aggregates:
        # 1. computed at close, thus we exclude today
        # 2. start at floor_date Monday 
        .from <- lubridate::as_date(lubridate::floor_date(.bounds[[1]], unit = paste0(.multiplier, " ", .timeframe, "s"), week_start = 1))
        .expected <- seq(.from, lubridate::today(), by = paste0(.multiplier, " ", .timeframe, "s"))
        # dates that are trading days (match the .cal) and less than today
        .expected <- .expected[.expected %in% .cal$date & .expected < lubridate::today()]
      } else if (.tf_num == 4) {
        # if the interval is by week
        if (.bounds[[2]] > lubridate::now()) {
          .to <- lubridate::floor_date(lubridate::today(), .timeframe)
        } else {
          .to <- lubridate::as_date(.bounds[[2]])
        }
        if (.multiplier > 1) {
          .from <- .bounds[[1]] + lubridate::duration(.multiplier - 1, .timeframe)
        } else {
          .from <- .bounds[[1]]
        }
        .expected <- seq(from = lubridate::as_date(.from), to = .to, by = paste(.multiplier, .timeframe))
        
      } else if (.tf_num == 5) {
        if (.bounds[[2]] > lubridate::now()) {
          .to <- lubridate::floor_date(lubridate::today(), .timeframe)
        } else {
          .to <- lubridate::as_date(.bounds[[2]])
        }
        
        .from <- .bounds[[1]] + lubridate::duration(.multiplier, .timeframe)
        .expected <- seq(from = lubridate::as_date(.from), to = .to, by = paste(.multiplier, .timeframe))
      } else if (.tf_num == 6) {
        .by <- paste(3 * .multiplier, "months")
        .from <- .bounds[[1]]
        .to <- .bounds[[2]]
        .expected <- seq(from = lubridate::as_date(.from), to = .to, by = paste(.multiplier, .timeframe))
      } else if (.tf_num == 7) {
        .from <- .bounds[[1]]
        .to <- .bounds[[2]]
        .expected <- seq(from = lubridate::as_date(.from), to = .to, by = paste(.multiplier, .timeframe))
      }
    }
    if (.tf_num > 2) {
      .expected <- lubridate::as_date(.expected)
    }
    browser(expr = length(.expected) == 0)
    
    return(.expected)
  })
  
  return(.expected)
}


# Tue Mar 31 09:43:59 2020
# bars_missing ----
# Sun Mar 29 16:09:40 2020
#' bars_complete
#' 
#' Internal function for comparing actual data to expected data and returning missing data. The function is intended for use in an environment where its parameters already exist. If non-required params need to be specified explicitly, **all params must be named**. 
#' @param bars Output from \link[AlpacaforR]{bars_get}
#' @param v See \link[AlpacaforR]{get_bars}
#' @param .bounds Output from \link[AlpacaforR]{bars_bounds}
#' @return `(list)` If there are no missing values, NULL is returned. Otherwise, a list with two items corresponding to the data for each symbol returned:
#' \itemize{
#'  \item{`missing` vector of missing expected Date/Datetimes}
#'  \item{`gaps` `(data.frame)` with three columns:
#'  \itemize{
#'   \item{`gap_times`: a vector of Date/Datetimes with the gaps in the data that can be used for subsequent API query bounds.}
#'   \item{`gap_inds`: the indices of the gaps in the data (if the missing data is prior to or after the actual data, this will be NA).}
#'   \item{`f`: a factor to `split` the gaps into query boundaries}
#'   \item{`position`: a factor indicating whether the row corresponds to  `leading` missing data, or the `begin`ing or `end` of a gap in the data.}
#'  }
#'  } 
#' }
#' @keywords internal
#' @importFrom purrr map2
#' @import lubridate

bars_missing <- function(bars, ...) {
  list2env(list(...), environment())
  
  .expected <- bars_expected(bars, v = v, .bounds = .bounds, timeframe = timeframe, multiplier = multiplier, .tf_num = .tf_num)
  
  .out <- purrr::map2(bars, .expected, ~{
    .expected <- .y
    if (.tf_num < 3) {
      message(paste0("timeframe: ", .tf_num))
      .cutoff <- attr(.x, "query")$ts - lubridate::duration(multiplier, timeframe)
      .actual <- subset(.x$time, subset = lubridate::hm(format(.x$time, "%H:%M")) >= lubridate::hm("09:30") & lubridate::hm(format(.x$time, "%H:%M")) <= lubridate::hm("16:00")) %>% 
        # account for the time at which the query was done for m/h requests
        subset(subset = . < .cutoff)
      # Get the missing dates
      .missing_dates <- try(lubridate::as_datetime(setdiff(.expected, .actual), origin = lubridate::origin))
      browser(expr = class(.missing_dates) == "try-error")
    } else {
      .actual <- try(lubridate::as_date(.x$time)  )
      browser(expr = class(.actual) == "try-error")
      # get the missing days
      .missing_dates <- try(lubridate::as_date(setdiff(.expected, .actual)))
      browser(expr = class(.missing_dates) == "try-error")
    }
    
    # Get the missing dates
    
    if (length(.missing_dates) == 0) return(NULL)
    #TODO this overcomplicates things, reduce to missing_times df
    .gap_times <- c()
    .gap_inds <- c()
    
    if (any(.missing_dates < min(.actual))) {
      .md <- .missing_dates[.missing_dates < min(.actual)]
      # return the lower bound and the min
      .gap_inds <- append(.gap_inds, c(NA, 1))
      if (.tf_num < 3) {
        .gap_times <- append(.gap_times, c(.md - lubridate::days(1), .md))
      } else {
        .gap_times <- append(.gap_times, c(.md, .md + lubridate::days(1)))
      }
      
    }
    
    # Get the gaps indices
    .inds <- which(diff(.actual) > max(diff(.expected)))
    .inds <- sort(c(.inds, .inds + 1) %>% subset(subset = . < length(.actual)))
    
    # append them to .gap_times
    .gap_times <- append(.gap_times, unique(.actual[.inds]))
    .gap_inds <- append(.gap_inds, .inds[!.inds %in% .gap_inds])
    
    if (any(.missing_dates > max(.actual))) {
      # if the missing are greater than the max
      .md <- .missing_dates[.missing_dates > max(.actual)]
      # return the last index and NA 
      .gap_inds <- append(.gap_inds, c(length(.actual), NA))
      # return the max and upper bound for the times
      browser(expr = class(.actual) != class(.bounds[[2]]))
      if (.tf_num < 3) {
        .gap_times <- append(.gap_times, c(.md - lubridate::days(1), .md))
      } else {
        .gap_times <- append(.gap_times, c(.md, .md + lubridate::days(1)))
      }
      
    }
    
    if (length(.gap_times) == 0) return(list(missing = missing_dates, gaps = NA))
    if (anyNA(.gap_inds)) {
      # If there are leading or lagging missing times/inds, remove those and add them to their own df
      .na <- which(is.na(.gap_inds))
      .position <- c()
      if (any(.na == 1)) .position[1] <- "leading"
      if (any(.na > 1)) .position[length(.position) + 1] <- "end"
      
      .df <- try(data.frame(gap_times = .gap_times[.na], gap_inds = .gap_inds[.na], f = NA, position = .position))
      .gap_inds <- .gap_inds[- c(.na)]
      .gap_times <- .gap_times[- c(.na)]
      if (length(.gap_inds) == 0) {
        return(.df)
      }
    }
    
    
    .gaps <- try(data.frame(gap_times = .gap_times, gap_inds = .gap_inds, f = ifelse(length(.gap_times) / 2 > 1, rep(seq(1, length(.gap_times) / 2), each = 2), 1), position =  rep_len(c("begin","end"), length.out = length(.gap_times))))
    
    if (exists(".na")) {
      # rebind those leading/lagging missing times/inds to the df
      if (any(.na == 1)) {
        .gaps <- rbind.data.frame(.df[1,], .gaps)
      }
      if(any(.na > 1)) {
        .gaps <- rbind.data.frame(.gaps, .df[nrow(.df),])
      }
    }
    # Use the 1 day endpoint for retrieving missing dates in all cases where timeframe is <= days
    .gaps$timeframe <- ifelse(.tf_num < 3, timeframe, "day")
    .gaps$multiplier <- ifelse(.tf_num < 3, multiplier, 1)
    # Get the gaps
    browser(expr = class(.gaps) == "try-error")
    
    # Get the actual excluded timestamps ----
    # Sun Mar 29 16:45:52 2020
    return(list(missing = .missing_dates, gaps = .gaps))
  })
  return(.out)
}

# bars_complete ----
# Wed Apr 01 17:24:47 2020
#' bars_complete
#' 
#' This function uses the output from bars_missing and fills the missing data

bars_complete <- function(bars, .missing, ...) {
  list2env(list(...), envir = environment())
  purrr::map2(.missing, bars, ~{
    # If there are no missing data, return the data
    if (is.null(.x) && is.null(.x[[1]])) return(.y)
    # if days or hours
    if (v == 1) {
      
    } else if (v == 2) {
      if (.tf_num < 3) {
        # case when the missing data leads the returned
        if (is.na(.x$gaps$f[1])) {
          # create the boundaries
          .bounds <- bars_bounds(from = .x$gaps$gap_times[1], to = .x$gaps$gap_times[2])
          .url <- bars_url(.bounds = .bounds)
          .bars <- bars_get(.url)
          .missing <- bars_missing(bars)
          length(.missing) #TODO if there is still missing data continue. This will need to be nested inside of a while loop or something of that nature to continue requerying until the data is filled.
          # Remove from the df
          .missing <- .x$gaps[- c(1:2),]
        }
      }
    }
    
    
  })
  
  
}



# bars_transform ----
# Sun Mar 29 16:09:30 2020
#' bars_transform
#' 
#' Internal function for transforming data from Alpaca API to a human-readable TTR/quantmod compatible format
#' @keywords internal
#' @importFrom magrittr "%>%" 
#' @import dplyr
bars_transform <- function(bars) {
  bars = purrr::imap(bars, ~{
    if (!any("data.frame" %in% class(.x))) {
      message(paste0("The symbol ", .y, " returned no data.")) 
      return(NULL)
    } 
    nms <- c(time = "t", open = "o", high = "h", low = "l", close = "c", volume = "v")
    out <- dplyr::mutate_at(.x, dplyr::vars("t"), ~as.POSIXct(.,origin = "1970-01-01")) %>% dplyr::mutate_at(dplyr::vars(o,h,c,l,v),~as.numeric(.)) %>%
      dplyr::rename((!!nms))
  })  
}


# UPDATED for V2 ----
