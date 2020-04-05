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
  .n <- try(names(.vars), silent = T)
  .env <- try(list2env(list(...), environment()), silent = T)
  .tf_num <- get0(".tf_num", envir = pf, ifnotfound = get0(".tf_num", sys.frame(-1)))
  if (class(.n) == "try-error" || is.null(.n) || class(.env) == "try-error") {
    .date_vars <- purrr::map(c(from = "from", to = "to", after = "after", until = "until"), get0, envir = pf)
    
  } else if (all(c("from", "to") %in% .n) || all(c("after", "until") %in% .n)) {
    .date_vars <- .vars[c(from = "from", to = "to", after = "after", until = "until")]
  }
  rm(pf)
  
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
    
    # Floors or ceilings
    if (.tf_num < 3) {
      # Case less than day, to include the day requested, the boundary must be the previous day
      .unit <- paste(1, "day")
    } else if (.tf_num == 4 && multiplier > 1) {
      # lubridate does not support multi-weeks to floor_date
      # case where timeframe is week and multiplier is more than 1, just use 1 week and we will subract multiplier - 1, weeks
      .w <- T
      .unit <- paste(1, timeframe)
    } else if (.tf_num == 6) {
      # case where timeframe is quarter
      .q <- T
      .unit <- paste(multiplier, timeframe)
      # quarter isn't an acceptable duration in lubridate so we improvise with 3 months 
      timeframe <- "month"
      multiplier <- 3 * multiplier
    } else {
      .unit <- paste(multiplier, timeframe)
    }
    
    if (.y == "from" || .y == "after") {
      .out <- lubridate::floor_date(.out, .unit)
      if (exists(".q")) {
        # to include the initial day of the quarter, the end of the previous quarter must be the lower boundary
        .out <- .out - lubridate::days(1)
      } else if (exists(".w")) {
        # lubridate does not support multi-weeks to floor_date, so we just subtract multiplier - 1 weeks
        .out <- .out - lubridate::duration(multiplier - 1, "weeks")
      }
    } else {
      # To include the ending boundary, we need to add a full cycle of multiplier * timeframe
      .out <- lubridate::ceiling_date(.out, .unit, change_on_boundary = F) + lubridate::duration(multiplier, timeframe)
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

bars_url <- function(..., pf = parent.frame()) {
  .vars <- list(...)
  .env <- try(list2env(.vars, environment()), silent = T)
  .n <- try(names(.vars), silent = T)
  .vn <- c(ticker = "ticker", .bounds = ".bounds", multiplier = "multiplier", timeframe = "timeframe", v = "v", unadjusted = "unadjusted")
  if (class(.env) == "try-error") {
    list2env(purrr::map(.vn, get0, envir = pf), environment())
  }
  rm(pf)
  # if ( !all(.vn %in% ls(all.names = T)) || any(purrr::map_lgl(.vn, ~is.null(get0(.x)))) ) {
  #   browser()
  #   stop("bars_url is missing necessary variables")
  # }
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
bars_expected <- function(bars, ..., pf = parent.frame()) {
  .vars <- list(...)
  .env <- try(list2env(.vars, environment()), silent = T)
  .n <- try(names(.vars), silent = T)
  .vn <- c(.bounds = ".bounds", multiplier = "multiplier", timeframe = "timeframe", v = "v")
  .tf_num <- get0(".tf_num", envir = pf, ifnotfound = get0(".tf_num", sys.frame(-1)))
  if (class(.env) == "try-error") {
    list2env(purrr::map(.vn, get0, envir = pf), environment())
  }
  .vn <- append(.vn, c(.tf_num = ".tf_num"))
  browser(expr = !exists(".tf_num"))
  rm(pf)
  # if ( !all(.vn %in% ls(all.names = T)) || any(purrr::map_lgl(.vn, ~is.null(get0(.x)))) ) {
  #   browser()
  #   stop("bars_expected is missing necessary variables")
  # }
  # 
  
  
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
    dplyr::filter(date >= lubridate::as_date(.bounds[[1]]) & date <= lubridate::now())
  
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
    .cutoff <- rlang::`%||%`(attr(.x, "query")$ts, attr(.x, "query")[[1]]$ts)
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
#' bars_missing
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
#' @importFrom purrr map2_dfr map_dfr flatten_int
#' @importFrom dplyr bind_rows mutate_at
#' @importFrom lubridate:
#' @importFrom tibble tibble
#' @importFrom magrittr "%>%"
#' @import lubridate isoweek with_tz

bars_missing <- function(bars, ..., pf = parent.frame()) {
  .vars <- try(list2env(list(...), environment()), silent = T)
  .vn <- c(.bounds = ".bounds", multiplier = "multiplier", timeframe = "timeframe", v = "v")
  .tf_num <- get0(".tf_num", envir = pf, ifnotfound = get0(".tf_num", sys.frame(-1)))
  if (class(.vars) == "try-error") {
    list2env(purrr::map(.vn, get0, envir = pf), environment())
  }
  .vn <- append(.vn, c(.tf_num = ".tf_num"))
  # if ( !all(.vn %in% ls(all.names = T)) || any(purrr::map_lgl(.vn, ~is.null(get0(.x)))) ) {
  #   browser()
  #   stop("bars_missing is missing necessary variables")
  # }
  rm(pf)
  .expected <- bars_expected(bars, v = v, .bounds = .bounds, timeframe = timeframe, multiplier = multiplier)

  .out <- purrr::map2(bars, .expected, ~{
    browser()
    .ticker <- rlang::`%||%`(attr(.x, "query")$ticker, attr(.x, "query")[[1]]$ticker)
    .expected <- .y
    if (.tf_num < 3) {
      message(paste0("timeframe: ", .tf_num))
      .cutoff <- rlang::`%||%`(attr(.x, "query")$ts, attr(.x, "query")[[1]]$ts) - lubridate::duration(multiplier, timeframe)
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
    .any <- length(.missing_dates) > 0
  
    if (length(.missing_dates) == 0) return(NULL)
    .missing_dates <- lubridate::with_tz(.missing_dates, Sys.timezone())
    # Use the 1 day endpoint for retrieving missing dates in all cases where timeframe is <= days
    .multiplier <- ifelse(.tf_num < 3, multiplier, 1)
    .timeframe <- ifelse(.tf_num < 3, timeframe, "day")
    # If there are leading missing dates (the most common case)
    if (any(.missing_dates < min(.actual))) {
      # get the leading missing dates
      .md <- .missing_dates[.missing_dates < min(.actual)]
      # remove the leading missing dates
      .missing_dates <- .missing_dates[!.missing_dates < min(.actual)]
      # return the day endpoint bounds that will retrieve the missing data
      if (.tf_num < 4) {
        # add one day previous
        .url_md <- c(.md[1] - lubridate::days(1), .md)
        # split up by weeks
        .leading <- split(.url_md, lubridate::isoweek(.url_md)) %>% 
          # map over the weeks
          purrr::map_dfr(~{
            .gap_times <- range(.x)
            .url <- bars_url(
              .bounds = list(from = .gap_times[1], to = .gap_times[2]),
              multiplier = ifelse(.tf_num < 3, multiplier, 1),
              timeframe = ifelse(.tf_num < 3, timeframe, "day"),
              ticker = .ticker,
              v = v,
              unadjusted = unadjusted
            )
            # need to subset out any additional dates
            .m <- .x[.x %in% .md]
            # if the added day on the beginning adds an additional week, return NULL
            if (length(.m) == 0) return(NULL)
            .leading <- tibble::tibble(missing = list(.m), url = .url)
          })  
         
        
      } else {
        .gap_times <- c(.md, .md + lubridate::days(1))
        if (length(.gap_times) > 2) {
          
          .gap_times <- split(.gap_times, f = rep(seq(1:{length(.gap_times) / 2}), each = 2))
          .leading <- purrr::map2_dfr(.gap_times, .md, ~{
            .bounds <- bars_bounds(from = .x[[1]], to = .x[[2]], multiplier = .multiplier, timeframe = .timeframe)
            # create an interval within those bounds
            .int <- lubridate::interval(.x[[1]], .x[[2]])
            # Determine which missing_dates are within that interval
            .md <- .missing_dates[lubridate::`%within%`(.missing_dates, .int)]
            .url <- bars_url(
              .bounds = .bounds,
              multiplier = .multiplier,
              timeframe = .timeframe,
              ticker = .ticker,
              v = v,
              unadjusted = unadjusted
            )
            .df <- tibble::tibble(missing = list(.y), url = .url)
          })  
        } else {
          .url <- bars_url(
            .bounds = list(from = .gap_times[1], to = .gap_times[2]),
            multiplier = ifelse(.tf_num < 3, multiplier, 1),
            timeframe = ifelse(.tf_num < 3, timeframe, "day"),
            ticker = .ticker,
            v = v,
            unadjusted = unadjusted
          )
          .leading <- tibble::tibble(missing = list(.md), url = .url)
        }
      }
      
      # if there are no more missing dates, return the dataframe
      if (length(.missing_dates) == 0) .out <- .leading
    }
    # If there are lagging missing dates (the 2nd most common case)
    if (any(.missing_dates > max(.actual))) {
      # retrieve missing > the max
      .md <- .missing_dates[.missing_dates > max(.actual)]
      # remove the lagging missing dates
      .missing_dates <- .missing_dates[!.missing_dates > max(.actual)]
      if (.tf_num < 4) {
        # add one day previous
        .url_md <- c(.md[1] - lubridate::days(1), .md)
        # split up by weeks
        .lagging <- split(.url_md, lubridate::isoweek(.url_md)) %>% 
          # map over the weeks
          purrr::map_dfr(~{
            .gap_times <- range(.x)
            .url <- bars_url(
              .bounds = list(from = .gap_times[1], to = .gap_times[2]),
              multiplier = ifelse(.tf_num < 3, multiplier, 1),
              timeframe = ifelse(.tf_num < 3, timeframe, "day"),
              ticker = .ticker,
              v = v,
              unadjusted = unadjusted
            )
            # need to subset out any additional dates
            .m <- .x[.x %in% .md]
            # if the added day on the beginning adds an additional week, return NULL
            if (length(.m) == 0) return(NULL)
            .lagging <- tibble::tibble(missing = list(.m), url = .url)
          })  
      } else {
        .gap_times <- c(.md, .md + lubridate::days(1))
        if (length(.gap_times) > 2) {
          .gap_times <- split(.gap_times, f = rep(seq(1:{length(.gap_times) / 2}), each = 2))
          .lagging <- purrr::map2_dfr(.gap_times, .md, ~{
            .bounds <- bars_bounds(from = .x[[1]], to = .x[[2]], multiplier = .multiplier, timeframe = .timeframe)
            # create an interval within those bounds
            .int <- lubridate::interval(.x[[1]], .x[[2]])
            # Determine which missing_dates are within that interval
            .md <- .missing_dates[lubridate::`%within%`(.missing_dates, .int)]
            .url <- bars_url(
              .bounds = .bounds,
              multiplier = .multiplier,
              timeframe = .timeframe,# Use the 1 day endpoint for retrieving missing dates in all cases where timeframe is <= days
              ticker = .ticker,
              v = v,
              unadjusted = unadjusted
            )
            .df <- tibble::tibble(missing = list(.y), url = .url)
          })  
        } else {
          .url <- bars_url(
            .bounds = list(from = .gap_times[1], to = .gap_times[2]),
            multiplier = .multiplier,
            timeframe = .timeframe,
            ticker = .ticker,
            v = v,
            unadjusted = unadjusted
          )
          .lagging <- tibble::tibble(missing = list(.md), url = .url)
        }
      }
      # if there are no more missing dates, return the dataframe
      if (exists(".leading") && length(.missing_dates) == 0) {
         # if there were leading, bind that to the lagging
        .out <- dplyr::bind_rows(.leading, .lagging)
        # otherwise just return lagging
      } else if (length(.missing_dates) == 0) .out <- .lagging
      
      
    }
    # If there are missing dates randomly interspersed
    if (length(.missing_dates) > 0) {
      if (.tf_num < 4) {
        # add one day previous
        .url_md <- c(.missing_dates[1] - lubridate::days(1), .missing_dates)
        # split up by weeks
        .mid <- split(.url_md, lubridate::isoweek(.url_md)) %>% 
          # map over the weeks
          purrr::map_dfr(~{
            .gap_times <- range(.x)
            .url <- bars_url(
              .bounds = list(from = .gap_times[1], to = .gap_times[2]),
              multiplier = ifelse(.tf_num < 3, multiplier, 1),
              timeframe = ifelse(.tf_num < 3, timeframe, "day"),
              ticker = .ticker,
              v = v,
              unadjusted = unadjusted
            )
            # need to subset out any additional dates
            .m <- .x[.x %in% .missing_dates]
            # if the added day on the beginning adds an additional week, return NULL
            if (length(.m) == 0) return(NULL)
            .mid <- tibble::tibble(missing = list(.m), url = .url)
          })  
      } else {
        .gap_times <- c(.missing_dates, .missing_dates + lubridate::days(1))
        if (length(.gap_times) > 2) {
          .gap_times <- split(.gap_times, f = rep(seq(1:{length(.gap_times) / 2}), each = 2))
          .mid <- purrr::map2_dfr(.gap_times, .md, ~{
            .bounds <- bars_bounds(from = .x[[1]], to = .x[[2]], multiplier = .multiplier, timeframe = .timeframe)
            # create an interval within those bounds
            .int <- lubridate::interval(.x[[1]], .x[[2]])
            # Determine which missing_dates are within that interval
            .md <- .missing_dates[lubridate::`%within%`(.missing_dates, .int)]
            .url <- bars_url(
              .bounds = .bounds,
              multiplier = .multiplier,
              timeframe = .timeframe,# Use the 1 day endpoint for retrieving missing dates in all cases where timeframe is <= days
              ticker = .ticker,
              v = v,
              unadjusted = unadjusted
            )
            .df <- tibble::tibble(missing = list(.y), url = .url)
          })  
        } else {
          .url <- bars_url(
            .bounds = list(from = .gap_times[1], to = .gap_times[2]),
            multiplier = .multiplier,
            timeframe = .timeframe,
            ticker = .ticker,
            v = v,
            unadjusted = unadjusted
          )
          .mid <- tibble::tibble(missing = list(.missing_dates), url = .url)
        }
      }
      if (exists(".leading") && exists(".lagging")) {
        # rebind those leading/lagging missing times/inds to the df
        .out <- dplyr::bind_rows(.leading, .mid, .lagging)
      } else if (exists(".leading")) {
        .out <- dplyr::bind_rows(.leading, .mid)
      } else if (exists(".lagging")) {
        .out <- dplyr::bind_rows(.mid, .lagging)
      } else {
        .out <- .mid
      }
    }
    
    browser(expr = length(.out[[2]]) == 0 && .any)
    return(.out)
  })
  if (length(.out) == 0) .out <- NULL # If empty just return NULL
  return(.out)
}

# bars_complete ----
# Wed Apr 01 17:24:47 2020
#' bars_complete
#' 
#' This function uses the output from bars_missing and fills the missing data
#' @importFrom magrittr "%>%"
#' @importFrom dplyr arrange distinct

bars_complete <- function(bars, .missing, ..., pf = parent.frame()) {
  # .vars <- try(list2env(list(...), environment()), silent = T)
  # .vn <- c(.bounds = ".bounds", multiplier = "multiplier", timeframe = "timeframe", v = "v")
  # .tf_num <- get0(".tf_num", envir = pf, ifnotfound = get0(".tf_num", sys.frame(-1)))
  # if (class(.vars) == "try-error") {
  #   list2env(purrr::map(.vn, get0, envir = pf), environment())
  # }
  # .vn <- append(.vn, c(.tf_num = ".tf_num"))
  # if ( !all(.vn %in% ls(all.names = T)) || any(purrr::map_lgl(.vn, ~is.null(get0(.x)))) ) {
  #   browser()
  #   stop("bars_complete is missing necessary variables")
  # }
  rm(pf)
 
  .newbars <- purrr::map2(.missing, bars, ~{
    # If there are no missing data, return the data
    if (is.null(.x)) return(.y)
    # if days or hours
    if (v == 1) {
      
    } else if (v == 2) {
      
      .ext <- environment()
      # assign df of missing to .m
      .ext$.m <- .x
      # grab the initial query
      .ext$.query <- list(attr(.y, "query"))
      # assign .y to two new objects for comparison
      .ext$.out <- .ext$.new <- .y
      .ext$.md <- do.call(c, .x$missing)
      while (!all(.ext$.md %in% .ext$.new$time)) {
        # get the new data
        .ext$.nd <- purrr::pmap_dfr(.x, ~{
          #browser()
          # Since this will only be a call for a single symbol, we can get the first index as it will be the new data
          .dat <- bars_get(..2)
          .m <- .miss <- bars_missing(.dat, .bounds = range(..1))
          .count <- 1
          if (!is.null(.m[[1]])){
            .t <- try(nrow(.miss[[1]]) > 0)
            browser(expr = class(.t) == "try-error")
          while(nrow(.miss[[1]]) > 0 && identical(.miss, .m)) {
            .nd <- bars_get(.miss[[1]][["url"]][.count])
            .m <- bars_missing(.nd, .bounds = range(.miss[[1]][["missing"]][[.count]]))
            
            .nt <- !.nd[[1]]$time %in% .dat[[1]]$time
            browser(expr = stringr::str_detect(.miss[[1]][["url"]][.count], "2020-03-25"))
            if (any(.nt)) {
              # if new times were retrieved
              # bind those new rows to the data
              .dat[[1]] <- dplyr::bind_rows(.nd[[1]][.nt,], .dat[[1]])
              # assign the query
              assign(".query", append(.ext$.query, list(attr(.nd[[1]], "query"))), envir = .ext)
            } else if(.count == nrow(.miss[[1]])) {
              # if no new times were retrieved, they're probably not going to be
              break
            }
            .count <- .count + 1
            .miss <- .m
            .t <- try(nrow(.miss[[1]]) > 0 && identical(.miss, .m))
            browser(expr = class(.t) == "try-error")
          }}
          assign(".query", append(.ext$.query, list(attr(.dat[[1]], "query"))), envir = .ext)
          # Add only the bars in missing dates
          .dat[[1]][.dat[[1]]$time %in% ..1,]
          # while there are missing values loop
          
        })
        # bind together the results from the new calls with the existing data
        .ext$.new <- dplyr::bind_rows(
          .ext$.out,
          .ext$.nd
        ) %>% 
          # and remove duplicates (there shouldnt be any but JIC)
          dplyr::distinct(time, .keep_all = T) %>% 
          dplyr::arrange(time)
        
        .fill_dates <- .ext$.md[!.ext$md %in% .ext$new$time]
        if (length(.fill_dates) > 0) {
          .fill_dates <- data.frame(time = .fill_dates) %>% 
          dplyr::mutate(volume = NA, vw = NA, open = NA, close = NA, high = NA, low = NA, n = NA)
          .ext$.new <- dplyr::bind_rows(.ext$.new, .fill_dates) %>% dplyr::arrange(time)  
        }
        
        
        if (identical(.ext$.new, .ext$.out)) break # for IPO where there might consistently be missing bars but no new bars will ever return because they don't exist
        # check for further missing
        
        # assign .new to .out for comparison
        .ext$.out <- .ext$.new
      }
    }
    # add the cumulative queries to the object
    attr(.out, "query") <- .ext$.query
    return(.out)
    })
  


  
  return(.newbars)
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
