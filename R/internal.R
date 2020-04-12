# Internals:  Sun Jan 12 10:20:31 2020 ----

# response_text_clean Clean data from Server Response function  ----
# Sun Mar 29 16:02:00 2020
#' 
#' Clean the response text (usually in unreadable json) and convert to a readable format using fromJSON. 
#' @param dat The response from our server GET request usually in a json format.
#' @keywords internal
#' @return The response in a readable format as a list.
#' @importFrom httr content
#' @importFrom jsonlite fromJSON
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
#' @importFrom httr add_headers
#' @importFrom purrr iwalk
#' @importFrom stringr str_replace
#' @importFrom rlang abort
get_headers <- function(live=FALSE){
  
  ifelse(live, 
         .headers <- httr::add_headers('APCA-API-KEY-ID' = Sys.getenv("APCA-LIVE-API-KEY-ID"), 
                                      'APCA-API-SECRET-KEY' = Sys.getenv("APCA-LIVE-API-SECRET-KEY")),
         
         .headers <- httr::add_headers('APCA-API-KEY-ID' = Sys.getenv("APCA-PAPER-API-KEY-ID"), 
                                      'APCA-API-SECRET-KEY' = Sys.getenv("APCA-PAPER-API-SECRET-KEY"))
  )
  purrr::iwalk(.headers$header, ~{
    if (nchar(.x) == 0) {
      .ev <- stringr::str_replace(.y, "(?<=APCA)", ifelse(live, "-LIVE", "-PAPER"))
      rlang::abort(message = paste0(.ev," is unset. Please set your API credentials as environmental variables. See vignette('installation', 'AlpacaforR') for details.")) #TODO if package name change, change this value
    }
  })
  return(.headers)
}

# Helper functions for get_bars ----
# Sat Mar 28 09:40:23 2020

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
#'@importFrom purrr imap map compact pmap
#'@importFrom rlang env_bind current_env env_get "!!!"
#' @importFrom stringr str_extract
#'@import lubridate

bars_bounds <- function(...) {
  #trickery to get the variables from the calling environment
  rlang::env_bind(rlang::current_env(), ...)
  .vn = c(.tf_num = ".tf_num", from = "from", to = "to", after = "after", until = "until", v = "v")
  if (!all(.vn %in% ls(all.names = T))) {
    .cev <- rlang::caller_env()
    rlang::env_bind(rlang::current_env(), !!!purrr::map(.vn[!.vn %in% ls(all.names = T)], ~rlang::env_get(env = .cev, nm = .x, default = NULL, inherit = T)))
    .tf_num <- .tf_num %||% which(.tf_order %in% timeframe)
  }
   if (all(c("from", "to") %in% ls(all.names = T)) || all(c("after", "until") %in% ls(all.names = T))) {
      .date_vars <- list(from = from, to = to, after = after, until = until)
   }
  if (length(purrr::compact(.date_vars)) < 2) {
    stop(paste0(stringr::str_extract(as.character(match.call()), "^\\w+")[1], " is missing necessary variables"))
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
      .out <- NA
    }
    # Warn
    if (is.na(.out)) rlang::warn(paste(.y,"could not be parsed. Returning NA"), class = "warning")
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
      # Create the quarter bounds that the polygon API uses
      .qs <- c(1, seq(3, 12, 3)) %>% {
        list(start = .[-5], end = dplyr::lead(.)[-5])
      } %>% {
        purrr::pmap(.l = ., .f = ~ {
          if (.x == 1) {
            .start <- lubridate::make_date(year = lubridate::year(.out), month = .x)
          } else {
            .start <- lubridate::make_date(year = lubridate::year(.out), month = .x, day = 30)
          }
          if (.y > 9) {
            .end <- lubridate::make_date(lubridate::year(.out), month = .y, day = 31)
          } else {
            .end <- lubridate::make_date(lubridate::year(.out), month = .y, day = 30)
          }
          lubridate::interval(
            start = .start,
            end = .end
          )
        })
      }
      # determine where the input date lies
      .q <- which(purrr::map_lgl(.qs, ~lubridate::`%within%`(a = .out, b = .x)))
      .unit <- paste(multiplier, timeframe)
      # Because duration doesn't accept quarter
      multiplier <- 3 * multiplier
      timeframe <- "months"
    } else {
      .unit <- paste(multiplier, timeframe)
    }
    
    if (.y == "from" || .y == "after") {
      .out <- lubridate::floor_date(.out, .unit)
      if (exists(".q")) {
        # to include the initial day of the quarter, the end of the previous quarter must be the lower boundary
        if (.q == 1) {
          .out <- lubridate::make_date(lubridate::year(.out), month = 12, day = 30)
        } else {
          .out <- lubridate::int_start(.qs[[.q]])
        }
        
      } else if (exists(".w")) {
        # lubridate does not support multi-weeks to floor_date, so we just subtract multiplier - 1 weeks
        .out <- .out - lubridate::duration(multiplier - 1, "weeks")
      }
    } else {
      # To include the ending boundary, we need to add a full cycle of multiplier * timeframe
      .out <- lubridate::ceiling_date(.out, .unit, change_on_boundary = F) + lubridate::duration(multiplier, timeframe)
      if (exists(".q")) {
        # Convert to the appropriate quarter boundary
        .out <- lubridate::int_end(.qs[[.q]])
      }
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
    .bounds <- purrr::map(.bounds, ~lubridate::force_tz(lubridate::as_date(.x), Sys.timezone()))
  }
  browser(expr = any(purrr::map_lgl(
    .bounds,
    ~ get0("dbg", .GlobalEnv, ifnotfound = F) &&
      (length(.x) == 0 ||
         lubridate::year(.x) > {
           lubridate::year(lubridate::today()) + 1
         } || is.na(.x))
  )))
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
#'@importFrom rlang env_bind current_env env_get "!!!"
#'@importFrom purrr map_chr
#'@importFrom lubridate as_date
#' @importFrom stringr str_extract

bars_url <- function(...) {
  .vn <- c(ticker = "ticker", .bounds = ".bounds", multiplier = "multiplier", timeframe = "timeframe", v = "v", unadjusted = "unadjusted")
  rlang::env_bind(rlang::current_env(), ...)
  if (!all(.vn %in% ls(all.names = T))) {
    .cev <- rlang::caller_env()
    rlang::env_bind(rlang::current_env(), !!!purrr::map(.vn[!.vn %in% ls(all.names = T)], ~rlang::env_get(env = .cev, nm = .x, default = NULL, inherit = T)))
  }
  if (!all(.vn %in% ls(all.names = T))) {
    stop(paste0(stringr::str_extract(as.character(match.call()), "^\\w+")[1], " is missing necessary variables"))
  }
  timeframe <- as.character(timeframe)
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
#' @importFrom rlang env_bind current_env 
#' @importFrom purrr iwalk imap map
#' @importFrom stringr str_extract
#' @import dplyr httr lubridate
bars_get <- function(url, ...) {
  rlang::env_bind(rlang::current_env(), ...)
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
    if (is.null(names(url))) {
      names(url) <- stringr::str_extract(url, "(?<=ticker\\/)\\w+")
    }
    bars <- purrr::imap(url, ~{
      .url <- .x
      .murl <- stringr::str_replace(.x, "(?<=\\=)[:alnum:]+$", "[REDACTED]")
      message(paste0("Retrieving ", .y, ":\n", .murl))
      #Send Request
      agg_quote = httr::GET(url = .url)
      # Save the query meta-date for appending to the output df
      .query <- list()
      .query$status_code <- agg_quote$status_code
      .query$url <- .url
      .query$ts <- lubridate::with_tz(lubridate::parse_date_time(agg_quote[["headers"]][["date"]], "a, d b Y T"), tz = Sys.timezone())  
      if (agg_quote$status_code != 200) {
        message(paste("Call", .murl, "returned status code", agg_quote$status_code, "- Returning metadata only"))
        return(append(agg_quote[1:5], values = .query[2:3]))
      }
      agg_quote = response_text_clean(agg_quote)
      if (length(agg_quote$results) == 0) {
        message(paste("Call", .murl, "returned no data", "- Returning response metadata only"))
        return(append(agg_quote[1:5], values = .query[2:3]))
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
#' @param .tf_num Numeric rank of timeframe, calculated automatically if timeframe is passed.
#' @return `(Date/Datetime)` A vector of expected date/datetimes based on the `.bounds`, `timeframe` and `multiplier`
#'@importFrom rlang env_bind current_env env_get "!!!" "%||%" "%|%"
#' @importFrom stringr str_extract
#' @importFrom magrittr "%>%" extract2
#'@import lubridate dplyr
bars_expected <- function(bars, ...) {
  rlang::env_bind(rlang::current_env(), ...)
  .vn <- c(.bounds = ".bounds", multiplier = "multiplier", timeframe = "timeframe", v = "v", .tf_num = ".tf_num")
  if (!all(.vn %in% ls(all.names = T))) {
    .cev <- rlang::caller_env()
    rlang::env_bind(rlang::current_env(), !!!purrr::map(.vn[!.vn %in% ls(all.names = T)], ~rlang::env_get(env = .cev, nm = .x, default = NULL, inherit = T)))
    .tf_num <- .tf_num %||% which(.tf_order %in% timeframe)
  }
  if (!all(.vn %in% ls(all.names = T))) {
    stop(paste0(stringr::str_extract(as.character(match.call()), "^\\w+")[1], " is missing necessary variables"))
  }
  
  
  #Calendar expansion for segmenting queries and data completeness check:  Thu Mar 26 08:50:42 2020 ----
  #Get the trading days in between the sequence
  if (.tf_num < 4) {
    .cal <- get_calendar(.bounds[[1]], .bounds[[2]]) %>%
      dplyr::filter(date >= lubridate::as_date(.bounds[[1]]) & date <= lubridate::now())
  }
  
  
  # Set the parameters for seq and lubridate durations
 
    .by <- paste0(multiplier," ", timeframe, "s")
    .multiplier <- multiplier
    .timeframe <- timeframe

  .expected <- purrr::imap(bars, ~{
    if (rlang::`%||%`(.x$resultsCount, 1) == 0) return(NULL)
    .cutoff <- attr(.x, "query")$ts %||% tryCatch (attr(.x, "query")[[1]][["ts"]], error = function (e) NULL) %||% lubridate::now()
    # if there is data
    if (nrow(.x) > 0) {
      .bgn <- .x$time[1]
      if (.tf_num < 4) {
        .cal <- dplyr::filter(.cal, date >= lubridate::as_date(.bgn))
      }
    } else {
      .bgn <- .bounds[[1]]
    }
     
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
      }) %>% do.call(c, .)
    } else if (.tf_num == 3) {
      # dat aggregates:
      # 1. computed at close, thus we exclude today
      # 2. start at floor_date Monday 
      .from <- lubridate::force_tz(lubridate::as_date(lubridate::floor_date(.bgn, unit = paste0(.multiplier, " ", .timeframe, "s"), week_start = 1)), Sys.timezone())
      
      .expected <- try({seq(.from, .bounds[[2]], by = paste0(.multiplier, " ", .timeframe, "s"))})
      # dates that are trading days (match the .cal) and less than today
      .expected <- .expected[.expected %in% .cal$date & .expected < lubridate::today()]
    } else if (.tf_num == 4) {
     
      .expected <- seq(from = .bgn, to = .bounds[[2]], by = paste(.multiplier, .timeframe))
      
    } else if (.tf_num == 5) {
      # Change the day of the month of each expected value to match that of the starting actual value.
      .day <- lubridate::day(.x$time[1])
      .expected <- purrr::map(seq(from = .bgn, to = .bounds[[2]], by = paste(.multiplier, .timeframe)), ~lubridate::force_tz(lubridate::make_date(lubridate::year(.x), month = lubridate::month(.x), day = .day), Sys.timezone())) %>% purrr::reduce(c)
      .exp <- data.frame(expected = .expected) %>% 
        dplyr::mutate(y = lubridate::year(expected),
                      m = lubridate::month(expected))
      .act <- data.frame(actual = .x$time) %>% 
        dplyr::mutate(y = lubridate::year(actual),
                      m = lubridate::month(actual))
      .expected <- left_join(.exp, .act, by = c("y", "m")) %>% 
        dplyr::mutate(d = lubridate::day(actual)) %>% 
        dplyr::mutate_if(anyNA, ~ {. %|% .[length(.) - 1] %|% .[1]}) %>% 
        dplyr::mutate_at(dplyr::vars(expected), ~lubridate::make_date(y, m, d)) %>% 
        magrittr::extract2("expected")
      
    } else if (.tf_num == 6) {
      
      .expected <- try({seq(.bgn, .bounds[[2]], by = as.character(.timeframe))})
      
    } else if (.tf_num == 7) {
      
      .expected <- seq(from = .bgn, to = .bounds[[2]], by = paste(.multiplier, .timeframe))
    }
    
    if (.tf_num > 2) {
      .expected <- lubridate::force_tz(lubridate::as_date(.expected), tz = Sys.timezone())
    }
    .expected <- subset(.expected, subset = .expected >= .bounds[[1]] & .expected <= .cutoff)
    browser(expr = length(.expected) == 0 || class(.expected) == "try-error")
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
#' @importFrom rlang env_bind current_env env_get "!!!" "%||%"
#' @importFrom stringr str_extract
#' @import lubridate

bars_missing <- function(bars, ..., .tf_reduce = F) {
  .vn = c(.bounds = ".bounds", v = "v", timeframe = "timeframe", multiplier = "multiplier", .tf_num = ".tf_num")
  #ticker must be named if df is passed without query object (as in bars_complete)
  rlang::env_bind(rlang::current_env(), ...)
  if (!all(.vn %in% ls(all.names = T))) {
    .cev <- rlang::caller_env()
    rlang::env_bind(rlang::current_env(), !!!purrr::map(.vn[!.vn %in% ls(all.names = T)], ~rlang::env_get(env = .cev, nm = .x, default = NULL, inherit = T)))
    .tf_num <- .tf_num %||% which(.tf_order %in% timeframe)
  }
  if (!all(.vn %in% ls(all.names = T))) {
    stop(paste0(stringr::str_extract(as.character(match.call()), "^\\w+")[1], " is missing the following variables: ", paste0(.vn[!.vn %in% ls(all.names = T)], collapse = "\n")))
  }
  # If data.frame is input, output just the tibble (not a list)
  if(is.data.frame(bars)) {
    bars <- list(bars)
    bars_df <- T
  } else {
    bars_df <- F
  } 
      
  .expected <- bars_expected(bars, v = v, .bounds = .bounds, timeframe = timeframe, multiplier = multiplier)
  
  .out <- purrr::map2(bars, .expected, ~{
    if (rlang::`%||%`(.x$resultsCount, 1) == 0) return(NULL) # if no data was returned
    .ticker <- attr(.x, "query")$ticker %||% attr(.x, "query")[[1]] %||% ticker
    .expected <- .y
    if (.tf_num < 3) {
      .cutoff <- (attr(.x, "query")$ts %||% attr(.x, "query")[[1]]$ts %||% lubridate::now()) - lubridate::duration(multiplier, timeframe)
      .actual <- subset(.x$time, subset = lubridate::hm(format(.x$time, "%H:%M")) >= lubridate::hm("09:30") & lubridate::hm(format(.x$time, "%H:%M")) <= lubridate::hm("16:00")) %>% 
        # account for the time at which the query was done for m/h requests
        subset(subset = . < .cutoff)
      # Get the missing dates
      .missing_dates <- try(lubridate::as_datetime(setdiff(.expected, .actual), origin = lubridate::origin))
      browser(expr = class(.missing_dates) == "try-error")
    } else {
      .actual <- .x$time
      browser(expr = class(.actual) == "try-error")
      # get the missing days
      .missing_dates <- try(lubridate::as_datetime(setdiff(.expected, .actual), tz = Sys.timezone()))
      browser(expr = class(.missing_dates) == "try-error")
    }
    if (length(.missing_dates) == 0) return(NULL)
    if (.tf_num < 3) {
      .missing_dates <- lubridate::with_tz(.missing_dates, Sys.timezone())
    } else {
      .missing_dates <- lubridate::force_tz(.missing_dates, Sys.timezone())
    }
    browser(expr = !any(.actual %in% .expected))
    .any <- length(.missing_dates) > 0
  
  
    # Use the 1 day endpoint for retrieving missing dates in all cases where timeframe is <= days
    
    
    if (.tf_reduce) {
      # Do not go lower than one
      .tf_num <- ifelse(.tf_num > 1, .tf_num - 1, .tf_num)
      .timeframe <- as.character(.tf_order[.tf_num])
      # if there is a multiplier when timeframe is min, just reduce it to 1
      .multiplier <- ifelse((.tf_num == 1 && multiplier > 1) || (.tf_num > 2), 1, multiplier)
    } else {
      .timeframe <- as.character(timeframe)
      .multiplier <- multiplier
    }
    if (.timeframe == "quarter") {
      .tf_dur <- lubridate::duration(3, ifelse(.timeframe == "quarter", "months", .timeframe))  
    } else {
      .tf_dur <- lubridate::duration(multiplier, timeframe)
    }
    
    
    # If there are leading missing dates (the most common case)
    if (any(.missing_dates < min(.actual))) {
      # get the leading missing dates
      .md <- .missing_dates[.missing_dates < min(.actual)]
      # remove the leading missing dates
      .missing_dates <- .missing_dates[!.missing_dates < min(.actual)]
      # return the day endpoint bounds that will retrieve the missing data
      if (.tf_num < 4 && !.tf_reduce) {
        # add one day previous and one day ahead
        .url_md <- c(.md[1] - .tf_dur, .md, .md[length(.md)] + .tf_dur)
        # split up by weeks
        .leading <- split(.url_md, lubridate::isoweek(.url_md)) %>% 
          # map over the weeks
          purrr::map_dfr(~{
            .gap_times <- range(.x)
            .url <- bars_url(
              .bounds = list(from = .gap_times[1], to = .gap_times[2]),
              multiplier = .multiplier,
              timeframe = .timeframe,
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
        .b <- bars_bounds(from = .md[1] - .tf_dur, to = .actual[1], timeframe = .timeframe, multiplier = .multiplier)
        .url <- bars_url(
          .bounds = .b,
          multiplier = .multiplier,
          timeframe = .timeframe,
          ticker = .ticker,
          v = v,
          unadjusted = unadjusted
        )
        .leading <- tibble::tibble(missing = list(.md), url = .url)
      
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
      if (.tf_num < 4 && !.tf_reduce) {
        # add one period previous and one period ahead
        .url_md <- c(.md[1] - .tf_dur, .md, .md[length(.md)] + .tf_dur)
        # split up by weeks
        .lagging <- split(.url_md, lubridate::isoweek(.url_md)) %>% 
          # map over the weeks
          purrr::map_dfr(~{
            .gap_times <- range(.x)
            .url <- bars_url(
              .bounds = list(from = .gap_times[1], to = .gap_times[2]),
              multiplier = .multiplier,
              timeframe = .timeframe,
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
        .b <- bars_bounds(from = .actual[length(.actual)], to = .md[length(.md)] + .tf_dur, timeframe = .timeframe, multiplier = .multiplier)
        .url <- bars_url(
          .bounds = .b,
          multiplier = .multiplier,
          timeframe = .timeframe,
          ticker = .ticker,
          v = v,
          unadjusted = unadjusted
        )
        .lagging <- tibble::tibble(missing = list(.md), url = .url)
      
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
      if (.tf_num < 4 && !.tf_reduce) {
        # add one day previous
        .url_md <- c(.missing_dates[1] - .tf_dur, .missing_dates)
        # split up by weeks
        .mid <- split(.url_md, lubridate::isoweek(.url_md)) %>% 
          # map over the weeks
          purrr::map_dfr(~{
            .gap_times <- range(.x)
            .b <- bars_bounds(from = .gap_times[1], to = .gap_times[2])
            .url <- bars_url(
              .bounds = .b,
              multiplier = .multiplier,
              timeframe = .timeframe,
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
        # If any gaps exist in the missing data greater than 2 *  the duration of multiplier * timeframe, then split the missing dates at those indices
        .gap_times <- .missing_dates[which(diff(.missing_dates) > (2 * .tf_dur)) %>% {do.call(c, list(rbind(., . + 1)))}]
        if (length(.gap_times) == 0) .gap_times <- range(.missing_dates)
        if (length(.gap_times) > 2) {
          .gap_times <- split(.gap_times, f = rep(seq(1:{length(.gap_times) / 2}), each = 2))
          .mid <- purrr::map2_dfr(.gap_times, .md, ~{
            .b <- bars_bounds(from = .x[[1]], to = .x[[2]], multiplier = .multiplier, timeframe = .timeframe)
            # create an interval within those bounds
            .int <- lubridate::interval(.x[[1]], .x[[2]])
            # Determine which missing_dates are within that interval
            .md <- .missing_dates[lubridate::`%within%`(.missing_dates, .int)]
            .url <- bars_url(
              .bounds = .b,
              multiplier = .multiplier,
              timeframe = .timeframe,# Use the 1 day endpoint for retrieving missing dates in all cases where timeframe is <= days
              ticker = .ticker,
              v = v,
              unadjusted = unadjusted
            )
            .df <- tibble::tibble(missing = list(.y), url = .url)
          })  
        } else {
          .b <- bars_bounds(from = .gap_times[1], to = .gap_times[2])
          .url <- bars_url(
            .bounds = .b,
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
    
    browser(expr = (length(.out[[2]]) == 0 && .any))
    if(any(class(.actual) != class(do.call(c, .out$missing)))) {
      if (lubridate::is.Date(.actual)) {
        .out$missing <- purrr::map(.out$missing, ~lubridate::force_tz(lubridate::as_date(.x), Sys.timezone()))
      } else {
        browser()
      }
    }
    return(.out)
  })
  if (length(.out) == 0) {
    .out <- NULL # If empty just return NULL
  } else if (bars_df) {
    # If a single data.frame is fed in, return just the missing tibble
    .out <- .out[[1]]
  }
  return(.out)
}

# bars_complete ----
# Wed Apr 01 17:24:47 2020
#' bars_complete
#' 
#' This function uses the output from bars_missing and fills the missing data
#' @importFrom magrittr "%>%"
#' @importFrom purrr map2 map pmap_dfr
#' @importFrom stringr str_extract str_extract_all
#' @importFrom dplyr arrange distinct
#' @importFrom lubridate ymd
#' @importFrom rlang env_bind current_env env_get "!!!" "%|%" "%||%"
bars_complete <- function(bars, .missing, ...) {
  
  .vn <- c(v = "v", timeframe = "timeframe", multiplier = "multiplier", limit = "limit", unadjusted = "unadjusted", full = "full", .tf_num = ".tf_num")
  rlang::env_bind(rlang::current_env(), ...)
  if (!all(.vn %in% ls(all.names = T))) {
    .cev <- rlang::caller_env()
    rlang::env_bind(rlang::current_env(), !!!purrr::map(.vn[!.vn %in% ls(all.names = T)], ~rlang::env_get(env = .cev, nm = .x, default = NULL, inherit = T)))
    .tf_num <- .tf_num %||% which(.tf_order %in% timeframe)
  }
  if (!all(.vn %in% ls(all.names = T))) {
    stop(paste0(stringr::str_extract(as.character(match.call()), "^\\w+")[1], " is missing the following variables: ", paste0(.vn[!.vn %in% ls(all.names = T)], collapse = "\n")))
  }
 if (is.data.frame(bars)) bars <- list(bars)
  .newbars <- purrr::map2(.missing, bars, ~{
    # ticker is passed in as the name of object
    ticker <- attr(.y, "query")$ticker %||% attr(.y, "query")[[1]]$ticker
    # If there are no missing data, return the data
    if (is.null(.x)) return(.y)
    # if days or hours
    if (v == 1) {
      
    } else if (v == 2) {
      
      ext <- new.env()
      # assign df of missing to .m
      .m_tib <- .x
      # grab the initial query
      ext$.query <- list(attr(.y, "query"))
      # assign .y to two new objects for comparison
      ext$.out <- .y
      .md <- do.call(c, .x$missing)
      while (length(.md) > 0) {
        # get the new data
        browser(expr = is.null(.m_tib))
        .new <- purrr::pmap_dfr(.m_tib, ~{
          # Since this will only be a call for a single symbol, we can get the first index as it will be the new data
          .m_tib_missing <- ..1
          #.b <- stringr::str_extract_all(..2, "\\d{4}-\\d{2}-\\d{2}")[[1]]
          .new <- bars_get(..2)[[1]]
          if (!is.data.frame(.new)) {
            return(NULL) 
          }
          if (exists(".tf_reduce", envir = ext)) {
            # create an ordered vector of all dates in the final intended data
            .d <- sort(c(ext$.out$time, .md))
            # we need intervals that will contain times of the reduced timeframe data from which we can compute aggregates similar to how Polygon computes aggregates to create data for "unreturnable" dates
            .d_int <- purrr::map(.m_tib_missing, ~{
              # for each missing data find the interval between the closest prior date and the missing_date
              
              if (.tf_num %in% 4:6) {
                .b <- bars_bounds(from = .x, to = .x, multiplier = multiplier, timeframe = timeframe)
                .out <- lubridate::interval(start = .x, end = .b$to)
              } else {
                .out <- lubridate::interval(start = .d[which.max(which(.d < .x))], end = .x)
              }
              return(.out)
            }) %>% do.call(c, .)
              # Compute the aggregates for the times in the interval
            .new <- purrr::map2_dfr(.d_int, .m_tib_missing, ~{
                .nd <- .new[lubridate::`%within%`(.new$time, .x), ] 
                # If no data is returned even with reduced timeframe return NA
                
                .v <- try(sum(.nd$volume, na.rm = T), silent = T)
                .v <- ifelse(is.infinite(.v), NaN, .v)
                .h <- try(max(.nd$high, na.rm = T), silent = T)
                .h <- ifelse(is.infinite(.h), NaN, .h)
                .l <- try(min(.nd$low, na.rm = T), silent = T)
                .l <- ifelse(is.infinite(.l), NaN, .l)
                .c <- ifelse(length(.nd$close[length(.nd$close)]) == 0, NaN, .nd$close[length(.nd$close)])
                .n <- nrow(.) %||% NaN
                
                #construct dataframe from arguments
                .out <- data.frame(
                  time = .y,
                  volume = .v,
                  # mean approximation
                  open = .nd$open[1],
                  # the first value of open
                  high = .h,
                  # the max of high
                  low = .l,
                  # the min of low
                  close = .c,
                  # The last of close
                  n = .n # the number of datapoints used in the agg
                )
                .out
              })
          }
        
          if (any(.m_tib_missing %in% .new$time)) {
          # if the call returned any new data, append the query
          assign(".query", append(ext$.query, list(attr(.new, "query"))), envir = ext)
          }
          .new
        })
        
        # determine if there are still missing dates
        # tryCatch to suppress warning if no data returned
        .still_md <- .md[!.md %in% tryCatch(.new$time, warning = function(w) NULL, error = function(e) NULL)]
        if (length(.still_md) < length(.md)) {
          # if we found some of the missing dates then bind the found dates
          ext$.out <- bind_rows(
            ext$.out,
            .new[.new$time %in% .md,]  
          )
          # and make the still missing, the .md object
          if (length(.still_md) > 0) {
            .m_tib <- bars_missing(ext$.out, .bounds = .bounds, .tf_reduce = ext$.tf_reduce %||% F, ticker = ticker)
            # ticker must be passed here
            if (is.null(.m_tib)) break
          }
          .md <- .still_md
        } else if (identical(.still_md, .md)) {
          # otherwise, if we turned up nothing, reduce the timeframe for the next call
          ext$.tf_reduce <- T
          # re-populate the .m_tib object with the remaining missing data
          .m_tib <- bars_missing(ext$.out, .bounds = .bounds, .tf_reduce = ext$.tf_reduce, ticker = ticker)
          # and here
          # add 1 to the count, or if it doesn't exist yes, make it 1
          .count <- 1 + (get0(".count") %||% 0)
          if (.count == 2) break # for IPO where there might consistently be missing bars but no new bars will ever return because they don't exist
        }
      }
    }
    # add the cumulative queries to the object
    attr(ext$.out, "query") <- ext$.query
    return(dplyr::arrange(ext$.out, time))
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
    out <- dplyr::mutate_at(.x, dplyr::vars("t"), ~as.POSIXct(.,origin = "1970-01-01") %>%  lubridate::force_tz(Sys.timezone()))  %>% dplyr::mutate_at(dplyr::vars(o,h,c,l,v),~as.numeric(.)) %>%
      dplyr::rename((!!nms))
  })  
}


# UPDATED for V2 ----
