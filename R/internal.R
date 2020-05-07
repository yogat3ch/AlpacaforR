# Helper functions for market_data ----
# Sat Mar 28 09:40:23 2020

# try_date ----
# Sat Apr 18 11:55:17 2020
#' @title String to Date/Datetime conversion for market_data
#' @keywords internal
#' 
#' @description Attempts to coerce string to a valid date object in stock exchange time zone, or checks to see if it is already. If it fails to coerce to date, returns NA.
#' @importFrom stringr str_detect
#' @importFrom lubridate ymd_hm parse_date_time is.Date is.POSIXct is.POSIXlt force_tz
try_date <- function(.x) {
  suppressMessages({
  if (is.character(.x) && stringr::str_detect(.x, ":")) {
    # if a character and date/time
    .out <- tryCatch({.out <- lubridate::ymd_hm(.x, tz = "America/New_York")}, warning = function(cond) {.out <- lubridate::as_datetime(.x, tz = "America/New_York")})
  } else if (is.character(.x)) {
    .out <- lubridate::parse_date_time(.x, orders = c("ymd", "mdy", "dmy"), tz = "America/New_York")
  } else if (any(lubridate::is.Date(.x) ||
                 lubridate::is.POSIXct(.x) ||
                 lubridate::is.POSIXlt(.x))) {
    .out <- lubridate::force_tz(.x, "America/New_York")
  } else {
    # If the format is incorrect return NA
    .out <- NA
  }
  })
  return(.out)
}

is_inf <- function(.x) {
  if (is.null(.x)) return(F)
  # check for infinite
  out <- tryCatch(is.infinite(.x), error = function(e) F)
  # if more than on object returned, then it's obviously not a single infinite
  out <- ifelse(length(out) > 1, F, out)
  !out
}

#' @title fetch variables 
#' @description Fetches variables from environment of containing function for use in internal function
#' @keywords internal
#' @param .vn named vector of variable names ie for a variable `d` `.vn = c(d = "d")`
#' @param e The list made from ellipsis if parameters are input as ellipsis argument
#' @param cenv The caller environment, stored automatically
#' @param penv The parent of the caller environment, also stored automatically
#' @param sf The system frames for searching if not found in previous two environments.
#' @importFrom rlang `!!!` env_bind env_get_list caller_env
#' @importFrom stringr str_extract
#' @importFrom purrr keep map2_lgl walk

fetch_vars <- function(.vn, e = list(...), cenv = rlang::caller_env(), penv = parent.env(rlang::caller_env()), sf = rev(sys.frames())) {
  `!!!` <- rlang::`!!!`
  try(list2env(e, env = cenv), silent = T)
  # remove the variables already existing
  .vn <- .vn[!names(.vn) %in% ls(all.names = T, cenv)]
  # add the parent environment
  if (!identical(globalenv(), penv)) {
    parent.env(cenv) <- penv
    .inherit <- T
  } else {
    .inherit <- F
  }
  if (!all(names(.vn) %in% ls(all.names = T, envir = cenv))) {
    .vars <- purrr::keep(rlang::env_get_list(env = cenv, names(.vn), default = Inf, inherit = .inherit), is_inf)
    .vars <- .vars[purrr::map2_lgl(.vars, .vn[names(.vars)], ~inherits(.x, .y))]
    rlang::env_bind(cenv, !!!.vars)
    .missed <- .vn[!names(.vn) %in% ls(all.names = T, env = cenv)]
    if (length(.missed) > 0) {
      .vars <- purrr::walk(sf, ~{
        if (identical(globalenv(), .x)) return(NULL)
        .inherit <- ifelse(identical(globalenv(), parent.env(.x)), F, T)
        .v <- purrr::keep(rlang::env_get_list(env = .x, names(.missed), default = Inf, inherit = .inherit), is_inf)
        .v <- .v[purrr::map2_lgl(.v, .vn[names(.v)], ~inherits(.x, .y))]
        rlang::env_bind(cenv, !!!.v)
      })
    }
  }
  # if (!all(.vn %in% ls(all.names = T, env = cenv))) {
  #   browser()
  #   stop(paste0(stringr::str_extract(as.character(match.call()), "^\\w+")[1], " is missing necessary variables"))
  # }
  
}



#' @title convert timeframe to numeric `.tf_num`
#' @description If `.tf_num` is not found in environment, create it from `timeframe`. Throw errors if arguments are unacceptable
#' @inheritParams market_data
#' @param cenv Calling environment in which to assign variables
#' @param ... additional arguments
#' @return .tf_num & timeframe in calling environment. 
#' @keywords internal
#' @importFrom purrr map_chr
#' @importFrom rlang abort warn caller_env env_bind

tf_num <- function(timeframe, ..., cenv = rlang::caller_env()) {
  
  .e <- list(...)
  .vn <- list(multiplier = c("integer", "numeric"), v = c("integer", "numeric"))
  fetch_vars(.vn, e = .e)
  #quick detection of timespan abbreviations:  Thu Mar 26 08:34:00 2020 ----
  .tf_opts <- list(m = c("m","min","minute"), h = c("h", "hour"), d = c("d", "day"), w = c("w", "week"), M = c("M", "mo", "month"), q = c("q", "quarter"), y = c("y", "year"))
  # Create ordered factor or timeframe options
  .tf_order <- purrr::map_chr(.tf_opts, tail, 1) %>% {factor(., levels = .)}
  
  if (v == 1) {
    # Get the timeframe
    timeframe <- tail(.tf_opts[c(1,3)][[grep(stringr::regex(timeframe, ignore_case = T), .tf_opts[c(1,3)], ignore.case = T)]], 1)
    
    # Check args
    if (timeframe == "minute" && !any(multiplier %in% c(1,5,15))) {
      rlang::abort("The v1 API only accepts multipliers of 1,5,15 for the minute timeframe")
    } else if (timeframe == "day" && multiplier != 1) {
      rlang::warn("The v1 API only accepts 1 as a `multiplier` for the day timeframe. One day bars will be returned.", class = "warning")
      multiplier <- 1
    }
    
  } else if (v == 2){
    timeframe <- tail(.tf_opts[[grep(timeframe, .tf_opts, ignore.case = F)[1]]], 1)
  }
  # Get the timeframe as a numeric
  .tf_num <- which(.tf_order %in% timeframe)
  rlang::env_bind(cenv, .tf_num = .tf_num, timeframe = timeframe)
}
#' @title Create API amenable boundaries based on user input for market_data

#' @description Internal function for fixing input date boundaries. The function is intended for use in an environment where its parameters already exist as it will use the variables from it's parent environment. See notes on specifying arguments in the *Details* section for \code{\link[AlpacaforR]{bars_complete}}.
#'@param from See \code{\link[AlpacaforR]{market_data}}
#'@param to See \code{\link[AlpacaforR]{market_data}}
#'@param after See \code{\link[AlpacaforR]{market_data}}
#'@param until See \code{\link[AlpacaforR]{market_data}}
#'@param v See \code{\link[AlpacaforR]{market_data}}
#'@return \code{(list)} A list with two Date/Datetime items that are boundaries for the date range necessary to call the API with to retrieve the appropriate data from the specified API. Items will have names corresponding to the non-NULL \code{from/to/after/until} arguments in the calling environment or supplied to the function.
#'@keywords internal
#' @importFrom rlang is_named env_bind current_env caller_env env_get warn "!!!" "%||%"
#' @importFrom purrr map compact imap pmap map_lgl
#' @importFrom stringr str_extract str_detect
#' @importFrom lubridate ymd_hm ymd is.Date is.POSIXct is.POSIXlt force_tz wday make_date year interval `%within%` floor_date int_start duration ceiling_date int_end days hour as_datetime as_date year today
#' @importFrom dplyr lead
#' @importFrom magrittr `%>%`
bars_bounds <- function(...) {
  `%>%` <- magrittr::`%>%`
  `%||%` <- rlang::`%||%`
  `!!!` <- rlang::`!!!`
  
  #trickery to get the variables from the calling environment
  .vn = list(.tf_num = c("integer", "numeric"), from = c("character","POSIXct", "Datetime", "Date"), to = c("character","POSIXct", "Datetime", "Date"), after = c("character","POSIXct", "Datetime", "Date"), until = c("character","POSIXct", "Datetime", "Date"), v = c("integer", "numeric"), timeframe = c("factor", "character"), multiplier = c("integer", "numeric"), .tf_order = "factor")
  .e <- list(...)
  fetch_vars(.vn, e = .e)
  if (!".tf_num" %in% ls(all.names = T)) tf_num(timeframe)
   if (all(c("from", "to") %in% ls(all.names = T)) || all(c("after", "until") %in% ls(all.names = T))) {
      .date_vars <- list(from = from, to = to, after = after, until = until)
   }
  if (length(purrr::compact(.date_vars)) < 1) {
    stop(paste0(stringr::str_extract(as.character(match.call()), "^\\w+")[1], " is missing necessary variables"))
  }
  # Remove NULL arguments
  .bounds <- purrr::compact(.date_vars)
  # Set defaults if non specified
  if (!any(c("from", "after") %in% names(.bounds))) {
    .bounds$from <- Sys.Date() - 7
  } else if (!any(c("to", "until") %in% names(.bounds))) {
    .bounds$to <- Sys.Date()
  }
  # Coerce to floor/ceiling dates/datetimes or fail with NA
  .bounds <- purrr::imap(.bounds, ~{
    .out <- try_date(.x)
    # Warn
    if (is.na(.out) || lubridate::year(.out) < 1792 || lubridate::year(.out) > lubridate::year(lubridate::today()) + 50) rlang::warn(paste(paste0("`",.y,"`"),"was parsed to", .out, "is this correct?"), class = "warning")
    # Floors or ceilings
    if (.tf_num < 3) {
      # Case less than day, to include the day requested, the boundary must be the previous day
      .unit <- ifelse(v == 2, paste(1, "day"), paste(multiplier, timeframe))
      .wd <- lubridate::wday(.out, label = T, abbr = F)
      if (.wd %in% c("Sunday", "Saturday")) {
        rlang::warn(message = paste0(.y, " is a ", .wd, ". Timeframes less than one week will return no data for weekend days."))
      }
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
        
      } else if (exists(".w", inherits = F)) {
        # lubridate does not support multi-weeks to floor_date, so we just subtract multiplier - 1 weeks
        .out <- .out - lubridate::duration(multiplier - 1, "weeks")
      }
    } else {
      # To include the ending boundary, we need to add a full cycle of multiplier * timeframe
      .out <- lubridate::ceiling_date(.out, .unit, change_on_boundary = F) + lubridate::duration(multiplier, timeframe)
      if (exists(".q", inherits = F)) {
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
    if (timeframe == "minute" && (lubridate::interval(.bounds[[1]], .bounds[[2]]) / (60 * 60 * 24)) < 1) {
      if (!any(lubridate::hour(seq.POSIXt(from = .bounds[[1]], to = .bounds[[2]], by = paste0(multiplier, " ",ifelse(timeframe == "minute","min",timeframe),"s"))) %in% 9:16)) {
        rlang::warn("Date range supplied does not include typical market hours, the V1 API is unlikely to return data")
      }
    }
  
    .bounds <- purrr::imap(.bounds, ~{
      if ((.y == "from" || .y == "after") && lubridate::is.Date(.x)){
        # If it's the start/after - use the beginning of the trading day
        .dt <- lubridate::as_datetime(paste0(.x, " 07:00"), format = "%Y-%m-%d %H:%M", tz = "America/New_York")
        
      } else if (lubridate::is.Date(.x)) {
        # If its the end/until - use the end of the trading day
        .dt <- lubridate::as_datetime(paste0(.x, " 19:00"), format = "%Y-%m-%d %H:%M", tz = "America/New_York")
      } else {
        .dt <- .x
      }
      .dt
    })
  } else if (v == 2) {
    .bounds <- purrr::map(.bounds, ~lubridate::force_tz(lubridate::as_date(.x), "America/New_York"))
    if (all(names(.bounds) %in% c("from", "to"))) {
      .bounds <- setNames(.bounds, c("from", "to"))
    }
  }
  #browser(expr = any(purrr::map_lgl(.bounds, ~ get0("dbg", .GlobalEnv, ifnotfound = F) && (length(.x) == 0 || lubridate::year(.x) > {lubridate::year(lubridate::today()) + 1} || is.na(.x)))))
  return(.bounds)
}

# bars_url ----
# Sun Mar 29 16:07:34 2020
#' @title Create URLs for market_data endpoints
#'
#'
#' @description Internal function for generating query urls. The function is intended for use in an environment where its parameters already exist. See notes on specifying arguments in the *Details* section for \code{\link[AlpacaforR]{bars_complete}}.
#' @param ticker See \code{\link[AlpacaforR]{market_data}}
#' @param .bounds Output from \[AlpacaforR]{bars_bounds}. If the output is assigned  to \code{.bounds} in the calling environment this object will be used.
#' @param limit *v1 only* `See \code{\link[AlpacaforR]{market_data}}, NULL unless explicitly specified.
#' @param multiplier See \code{\link[AlpacaforR]{market_data}}
#' @param timeframe See \code{\link[AlpacaforR]{market_data}}
#' @param v See \code{\link[AlpacaforR]{market_data}}
#' @return \code{(character)} URL to be called with \code{\link[AlpacaforR]{bars_get}}
#' @keywords internal
#' @importFrom rlang is_named env_bind current_env caller_env env_get "!!!" is_named
#' @importFrom purrr map_chr map
#' @importFrom lubridate as_date
#' @importFrom stringr str_extract str_sub
#' @importFrom httr parse_url build_url
bars_url <- function(..., limit = NULL) {
  `%||%` <- rlang::`%||%`
  `!!!` <- rlang::`!!!`
  .vn <- list(ticker = c("character"), .bounds = "list", multiplier = c("numeric", "integer"), timeframe = c("factor", "character"), v = c("numeric", "integer"), unadjusted = "logical")
  
  .e <- list(...)
  fetch_vars(.vn, e = .e)
  if (!".tf_num" %in% ls(all.names = T)) tf_num(timeframe)
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
      if (isTRUE(limit > 1000)) message("`limit` cannot exceed 1000, coercing to 1000.")
      limit <- 1000
    }  
  }
  if (v == 1) {
    url = httr::parse_url("https://data.alpaca.markets") #Pricing data uses unique URL, see market data API documentation to learn more
    timeframe <- ifelse(timeframe == "minute", "Min", "day")
    timeframe <- ifelse(timeframe == "Min", paste0(multiplier, timeframe), timeframe)
    #full = F Make API requests:  Tue Mar 17 21:37:35 2020 ----
    url$path <- list("v1", "bars", as.character(timeframe))
    # Coerce to appropriately formatted character strings
    .bounds <- purrr::map(.bounds, ~{
      .x <- format(.x, "%Y-%m-%dT%H:%M:%S%z")
      paste0(stringr::str_sub(.x, 1, -3),":", stringr::str_sub(.x, -2, nchar(.x)))
    })
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
        timeframe = as.character(timeframe),
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
#' @title Retrieve market data from the respective API
#'
#' @description Internal function for retrieving data from Alpaca API and outputting data.frame with query attribute. See notes on specifying arguments in the *Details* section for \code{\link[AlpacaforR]{bars_complete}}.
#' @param url \code{(character)} the url rendered by \code{\link[AlpacaforR]{bars_url}}
#' @return \code{(list)} See \code{\link[AlpacaforR]{market_data}} as the output is the same.
#' @keywords internal
#' @importFrom magrittr "%>%"
#' @importFrom rlang is_named env_bind current_env caller_env env_get warn "%||%"
#' @importFrom purrr iwalk imap map
#' @importFrom httr GET
#' @importFrom lubridate with_tz parse_date_time
#' @importFrom stringr str_extract str_replace
bars_get <- function(url, ...) {
  `%||%` <- rlang::`%||%`
  `!!!` <- rlang::`!!!`
  `%>%` <- magrittr::`%>%`
  .vn <- list(url = c("character","list"), v = c("integer", "numeric"), .tf_order = c("factor"), timeframe = c("factor", "character"))
  .e <- list(...)
  fetch_vars(.vn, e = .e)
  if (!".tf_num" %in% ls(all.names = T)) tf_num(timeframe)
  if (v == 1) {
    headers = get_headers()
    message(paste0("Retrieving\n", url))
    agg_quote = httr::GET(url = url, headers)
    .query <- list()
    .query$status_code <- agg_quote$status_code
    .query$url <- url
    .query$ts <- lubridate::with_tz(lubridate::parse_date_time(agg_quote[["headers"]][["date"]], "a, d b Y T"), tz = "America/New_York")
    if (agg_quote$status_code != 200) {
      message(paste(url, "\nReturned status code", agg_quote$status_code, "- Returning NULL"))
      return(NULL)
    }
    agg_quote = response_text_clean(agg_quote)
    
    .e <- rlang::current_env()
    purrr::iwalk(agg_quote, ~{
      .warn <- tryCatch({is.null(nrow(.x)) || nrow(.x) == 0},  error = function(cond) {assign("err", cond, .e);T})
      if (.warn) {
        rlang::warn(paste0(.y, " returned no data. Returning metadata only"))
      }  
    })
    
    bars <- bars_transform(agg_quote)
    # add query metadata to match v2 api
    bars <- purrr::imap(bars, ~{
      .query$ticker <- .y
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
      .query$ts <- lubridate::with_tz(lubridate::parse_date_time(agg_quote[["headers"]][["date"]], "a, d b Y T"), tz = "America/New_York")  
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
#' @title Return a Date/Datetime vector that the user expects
#'
#' @description An internal function for use when param `full = T` in \code{\link[AlpacaforR]{market_data}} that creates a vector of expected Date/Datetimes that are expected from a given call to either v1 or v2 API. The function is intended for use in an environment where its parameters already exist. See notes on specifying arguments in the *Details* section for \code{\link[AlpacaforR]{bars_complete}}.
#' @param bars \code{(list)} (Required) Output from \code{\link[AlpacaforR]{bars_get}}
#' @param v See \code{\link[AlpacaforR]{market_data}}
#' @param .bounds See \code{\link[AlpacaforR]{bars_bounds}}
#' @param multiplier See \code{\link[AlpacaforR]{market_data}}
#' @param timeframe  See \code{\link[AlpacaforR]{market_data}}
#' @param .tf_num Numeric rank of timeframe, calculated automatically if `timeframe` is passed or available in the parent env.
#' @return \code{(Date/Datetime)} A vector of expected date/datetimes based on the \code{.bounds}, \code{timeframe} and \code{multiplier}
#' @importFrom rlang env_bind current_env caller_env env_get "!!!" "%||%" "%|%" is_named
#' @importFrom stringr str_extract
#' @importFrom magrittr "%>%" extract2
#' @importFrom lubridate as_date now ceiling_date int_start int_end force_tz floor_date today day make_date year month
#' @importFrom purrr map imap reduce
#' @importFrom dplyr filter mutate mutate_if mutate_at vars
#' @keywords internal
bars_expected <- function(bars, ...) {
  `%>%` <- magrittr::`%>%`
  `%||%` <- rlang::`%||%`
  `!!!` <- rlang::`!!!`
  `%|%` <- rlang::`%|%`
  .vn <- list(.bounds = "list", multiplier = c("numeric", "integer"), timeframe = c("factor", "character"), v = c("numeric", "integer"), .tf_num = c("integer", "numeric"), .tf_order = c("factor"))
  .e <- list(...)
  fetch_vars(.vn, e = .e)
  if (!".tf_num" %in% ls(all.names = T)) tf_num(timeframe)
  
  #Calendar expansion for segmenting queries and data completeness check:  Thu Mar 26 08:50:42 2020 ----
  #Get the trading days in between the sequence
  if (.tf_num < 4) {
    .cal <- calendar(.bounds[[1]], .bounds[[2]]) %>%
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
      .from <- lubridate::force_tz(lubridate::as_date(lubridate::floor_date(.bgn, unit = paste0(.multiplier, " ", .timeframe, "s"), week_start = 1)), "America/New_York")
      
      .expected <- try({seq(.from, .bounds[[2]], by = paste0(.multiplier, " ", .timeframe, "s"))})
      # dates that are trading days (match the .cal) and less than today
      .expected <- .expected[.expected %in% .cal$date & .expected < lubridate::today()]
    } else if (.tf_num == 4) {
      
      .expected <- seq(from = .bgn, to = .bounds[[2]], by = paste(.multiplier, .timeframe))
      
    } else if (.tf_num == 5) {
      # Change the day of the month of each expected value to match that of the starting actual value.
      .day <- lubridate::day(.x$time[1])
      .expected <- purrr::map(seq(from = .bgn, to = .bounds[[2]], by = paste(.multiplier, .timeframe)), ~lubridate::force_tz(lubridate::make_date(lubridate::year(.x), month = lubridate::month(.x), day = .day), "America/New_York")) %>% purrr::reduce(c)
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
      .expected <- lubridate::force_tz(lubridate::as_date(.expected), tz = "America/New_York")
    }
    .expected <- subset(.expected, subset = .expected >= .bounds[[1]] & .expected <= .cutoff)
    #browser(expr = length(.expected) == 0 || class(.expected) == "try-error")
    return(.expected)
  })
  
  return(.expected)
}

# bars_missing ----
# Sun Mar 29 16:09:40 2020
#' @title Determine missing values in bars objects
#'
#' @description Internal function for comparing actual data to expected data and returning missing data. The function is intended for use in an environment where its parameters already exist.  See notes on specifying arguments in the *Details* section for \code{\link[AlpacaforR]{bars_complete}}
#' @param bars Output from \code{\link[AlpacaforR]{bars_get}}
#' @param v See \code{\link[AlpacaforR]{market_data}}
#' @param .bounds Output from \code{\link[AlpacaforR]{bars_bounds}}
#' @return \code{(list)} If there are no missing values, NULL is returned. Otherwise, a list with two items corresponding to the data for each symbol returned:
#' \itemize{
#'  \item{\code{missing} vector of missing expected Date/Datetimes}
#'  \item{\code{gaps}}{ \code{(data.frame)} with three columns:
#'  \itemize{
#'   \item{\code{gap_times}}{ a vector of Date/Datetimes with the gaps in the data that can be used for subsequent API query bounds.}
#'   \item{\code{gap_inds}}{ the indices of the gaps in the data (if the missing data is prior to or after the actual data, this will be NA).}
#'   \item{\code{f}}{ a factor to \code{split} the gaps into query boundaries}
#'   \item{\code{position}}{ a factor indicating whether the row corresponds to  \code{leading} missing data, or the \code{begin}ing or \code{end} of a gap in the data.}
#'  }
#'  } 
#' }
#' @keywords internal
#' @importFrom purrr map map2 map_dfr
#' @importFrom dplyr bind_rows
#' @importFrom tibble tibble
#' @importFrom magrittr "%>%"
#' @importFrom rlang env_bind current_env caller_env env_get "!!!" "%||%" is_named
#' @importFrom lubridate now duration hm as_datetime origin with_tz force_tz isoweek interval `%within%` is.Date as_date
#' @importFrom stringr str_extract

bars_missing <- function(bars, ..., .tf_reduce = F) {
  `%||%` <- rlang::`%||%`
  `!!!` <- rlang::`!!!`
  `%>%` <- magrittr::`%>%`
  .vn <- list(.bounds = "list", multiplier = c("numeric", "integer"), timeframe = c("factor", "character"), v = c("numeric", "integer"), unadjusted = "logical", .tf_num = c("integer", "numeric"), .tf_order = c("factor"))
  .e <- list(...)
  fetch_vars(.vn, e = .e)
  if (!".tf_num" %in% ls(all.names = T)) tf_num(timeframe)
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
      #browser(expr = class(.missing_dates) == "try-error")
    } else {
      .actual <- .x$time
      #browser(expr = class(.actual) == "try-error")
      # get the missing days
      .missing_dates <- try(lubridate::as_datetime(setdiff(.expected, .actual), tz = "America/New_York"))
      #browser(expr = class(.missing_dates) == "try-error")
    }
    if (length(.missing_dates) == 0) return(NULL)
    if (.tf_num < 3) {
      .missing_dates <- lubridate::with_tz(.missing_dates, "America/New_York")
    } else {
      .missing_dates <- lubridate::force_tz(.missing_dates, "America/New_York")
    }
    #browser(expr = !any(.actual %in% .expected))
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
      #browser(expr = get0("dbg", .GlobalEnv, ifnotfound = F) && !exists(".md"))
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
      #browser(expr = get0("dbg", .GlobalEnv, ifnotfound = F) && !exists(".md"))
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
          .mid <- purrr::map_dfr(.gap_times, ~{
            .b <- bars_bounds(from = .x[[1]], to = .x[[2]], multiplier = .multiplier, timeframe = .timeframe)
            # create an interval within those bounds
            .int <- lubridate::interval(.x[[1]], .x[[2]])
            # Determine which missing_dates are within that interval
            .md <- .missing_dates[lubridate::`%within%`(.missing_dates, .int)]
            # Remove any missing dates accounted for in this iteration from the pool
            .missing_dates <<- .missing_dates[!.missing_dates %in% .md]
            # if this iteration accounts for no missing dates, return NULL
            if (length(.md) == 0) return(NULL)
            .url <- bars_url(
              .bounds = .b,
              multiplier = .multiplier,
              timeframe = .timeframe,
              ticker = .ticker,
              v = v,
              unadjusted = unadjusted
            )
            .df <- tibble::tibble(missing = list(.md), url = .url)
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
    
    #browser(expr = (length(.out[[2]]) == 0 && .any))
    if(any(class(.actual) != class(do.call(c, .out$missing)))) {
      if (lubridate::is.Date(.actual)) {
        .out$missing <- purrr::map(.out$missing, ~lubridate::force_tz(lubridate::as_date(.x), "America/New_York"))
      } else {
        #browser()
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
#' @title Query the API as many times as necessary to fill a market_data query
#'
#' @description This function uses the output from bars_missing and fills the missing data. It's arguments are represented in the first object \code{.vn}. 
#'@details The arguments for all bars_\* functions  will be called from the environment from which the function is called if not specified explicitly. If params need to be specified explicitly, **all params must be named**. 
#' @param \code{bars} Returned by \code{bars_get}
#' @param \code{.missing} Returned by \code{bars_missing}
#' @importFrom magrittr "%>%"
#' @importFrom purrr map map2 pmap_dfr map2_dfr
#' @importFrom stringr str_extract str_extract_all
#' @importFrom dplyr arrange distinct
#' @importFrom lubridate ymd interval `%within%`
#' @importFrom rlang env_bind current_env caller_env env_get "!!!" "%|%" "%||%" is_named
#' @keywords internal
bars_complete <- function(bars, .missing, ...) {
  `%>%` <- magrittr::`%>%`
  `%||%` <- rlang::`%||%`
  `!!!` <- rlang::`!!!`
  .vn <- list(.bounds = "list", multiplier = c("numeric", "integer"), timeframe = c("factor", "character"), v = c("numeric", "integer"), unadjusted = "logical", .tf_num = c("integer", "numeric"), .tf_order = c("factor"), limit = c("integer", "numeric"), full = "logical")
  .e <- list(...)
  fetch_vars(.vn, e = .e)
  if (!".tf_num" %in% ls(all.names = T)) tf_num(timeframe)
 if (is.data.frame(bars)) bars <- list(bars)
  .newbars <- purrr::map2(.missing, bars, ~{
    # ticker is passed in as the name of object
    ticker <- attr(.y, "query")$ticker %||% attr(.y, "query")[[1]]$ticker
    # If there are no missing data, return the data
    if (is.null(.x)) return(.y)
    # if days or hours
    if (v == 2 || v == 1) {
      
      ext <- new.env()
      # assign df of missing to .m
      .m_tib <- .x
      # grab the initial query
      ext$.query <- list(attr(.y, "query"))
      # assign .y to two new objects for comparison
      ext$.out <- .y
      .md <- do.call(c, .x$missing)
      #browser(expr = get0("dbg", .GlobalEnv, ifnotfound = F) && !exists(".md"))
      while (length(.md) > 0) {
        # get the new data
        #browser(expr = is.null(.m_tib))
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
                # for weeks : quarter, the date returned is either the floor_date or representative of the period for which the aggregates are computed (or at least this appears to the case since using the previous period results in similar numbers to the previous bar data returned by polygon)
                .b <- bars_bounds(from = .x, to = .x, multiplier = multiplier, timeframe = timeframe)
                .out <- lubridate::interval(start = .x, end = .b$to)
              } else if (v == 2) {
                .out <- lubridate::interval(start = .d[which.max(which(.d < .x))], end = .x)
              } else {
                .out <- lubridate::interval(start = .x, end = .d[which.min(which(.d > .x))])
              }
              return(.out)
            }) %>% do.call(c, .)
              # Compute the aggregates for the times in the interval
            .new <- purrr::map2_dfr(.d_int, .m_tib_missing, ~{
                .nd <- .new[lubridate::`%within%`(.new$time, .x), ] 
                # If no data is returned even with reduced timeframe return NA
                suppressWarnings({
                .v <- try(sum(.nd$volume, na.rm = T), silent = T)
                .v <- ifelse(is.infinite(.v), NaN, .v)
                .h <- try(max(.nd$high, na.rm = T), silent = T)
                .h <- ifelse(is.infinite(.h), NaN, .h)
                .l <- try(min(.nd$low, na.rm = T), silent = T)
                .l <- ifelse(is.infinite(.l), NaN, .l)
                .c <- ifelse(length(.nd$close[length(.nd$close)]) == 0, NaN, .nd$close[length(.nd$close)])
                .n <- nrow(.nd) %||% NaN
                })
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
          if (v == 1) .new <- .new[- 7] # remove the n row for the v1 API
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
          .count <- 1 + (get0(".count", inherits = F) %||% 0)
          if (.count == 2) break # for IPO where there might consistently be missing bars but no new bars will ever return because they don't exist
        }
        #browser(expr = get0("dbg", .GlobalEnv, ifnotfound = F) && !exists(".md"))
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
#' @title Transform bars objects
#'
#' 
#' @description Internal function for transforming data from Alpaca API to a human-readable TTR/quantmod compatible format
#' @keywords internal
#' @importFrom purrr imap
#' @importFrom dplyr mutate_at vars rename
#' @importFrom lubridate force_tz
#' @importFrom magrittr `%>%`

bars_transform <- function(bars) {
  `%>%` <- magrittr::`%>%`
  bars = purrr::imap(bars, ~{
    if (!any("data.frame" %in% class(.x))) {
      message(paste0("The symbol ", .y, " returned no data.")) 
      return(NULL)
    } 
    nms <- c(time = "t", open = "o", high = "h", low = "l", close = "c", volume = "v")
    out <- dplyr::mutate_at(.x, dplyr::vars("t"), ~as.POSIXct(.,origin = "1970-01-01", tz = "America/New_York"))  %>% dplyr::mutate_at(dplyr::vars(o,h,c,l,v),~as.numeric(.)) %>%
      dplyr::rename((!!nms))
  })  
}


# get_headers Get Headers for Server Request function  ----
# Sun Mar 29 16:05:32 2020  
#' @title return headers for API calls
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

# get_url for Server Request ----
# Sun Mar 29 16:02:51 2020
#' @title Return the Alpaca URL
#' 
#' @description  Get the correct URL for the Server Request that is sent to interact with the API. If the user is on a paper account, then the paper account URL will be returned. 
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
#' @title Return the polygon URL
#' 
#' @description Get Polygon's URL for the Server Request that is sent to interact with the API.
#' @keywords internal
#' @return The correct URL for Polygon's API.
get_url_poly <- function(){
  url = "https://api.polygon.io" 
  return(url)
}

# Internals:  Sun Jan 12 10:20:31 2020 ----
# response_text_clean Clean data from Server Response function  ----
# Sun Mar 29 16:02:00 2020
#' @title Clean API responses
#' 
#' @description Clean the response text (usually in unreadable json) and convert to a readable format using fromJSON. 
#' @param dat The response from our server GET request usually in a json format.
#' @keywords internal
#' @return The response in a readable format as a list.
#' @importFrom httr content
#' @importFrom jsonlite fromJSON
response_text_clean <- function(dat){
  # If empty, return empty list
  if (length(dat$content) == 0) {
    out <- list()
  } else {
    dat = httr::content(dat, as = "text", encoding = "UTF-8")
    out <- jsonlite::fromJSON(dat)
  }
  
  return(out)
}

#' @title Check if value provided to a ticker_id argument is an ID or a ticker symbol
#' @keywords internal
#' @description for use in functions that accept `ticker_id`
#' @param ticker_id 
#' @return \code{logical} indicating whether the object is a 
#' @importFrom stringr str_count

is_id <- function(ticker_id) {
  out <- tryCatch({
    .out <- all(stringr::str_count(ticker_id, "-") ==  4, nchar(ticker_id) == 36)
    length(.out) > 0 && .out
  }, error = function(e) F)
  return(out)
}

#' @title transform positions objects
#' 
#' @description Cleans arrays of position objects, and position objects
#' @keywords internal
#' @importFrom stringr str_extract
#' @importFrom rlang warn
#' @importFrom purrr map_dfc
#' @importFrom tibble as_tibble
pos_transform <- function(pos) {
  if (class(pos) != "response") {
    .code <- pos$code
    .message <- pos$message
  } else if (class(pos) == "response") {
    .method <- pos$request$method
    .sym <- stringr::str_extract(pos$request$url, "\\w+$")
    .code <- pos$status_code
    #browser()
    .pos <- response_text_clean(pos)
    .message <- .pos$message
  }
  
  if(any(grepl(pattern = "^4", x = .code))) {
    rlang::warn(paste("Position was not modified.\n Message:", .message))
    return(.pos)
  } else if (.sym != "positions" && .sym %in% .pos$symbol && .method == "DELETE") {
    message(paste0(.sym, " closed successfully."))
  } else if (.method == "DELETE" && any(grepl("^2", .pos$body$code))) {
    message(paste0("All positions closed successfully.\nClosed Position(s): ", paste0(.pos$body$symbol[grepl("^2", .pos$body$code)], collapse = ", ")))
  }
  
  #Check if any pos exist before attempting to return
  if(length(.pos) == 0) {
    message("No positions are open at this time.")
    out <- .pos
  } else if(length(.pos) > 1 && !(.method == "DELETE" && .sym == "positions")) {
    browser()
    # coerce to numeric in .positions objects
    .pos[,c(5:6,8:ncol(.pos))] <- purrr::map_dfc(.pos[,c(5:6,8:ncol(.pos))], as.numeric)
    out <- tibble::as_tibble(.pos)
  } else {
    # if close_all
    out <- orders_transform(.pos$body)
    attr(out, "info") <- .pos[1:2]
  }
  return(out)
}

#' @title account activities transform
#' @description transform account activities
#' @param resp Reponse from account_activities endpoint
#' @keywords internal
#' @importFrom dplyr mutate_at vars `%>%`
#' @importFrom rlang warn
#' @importFrom stringr str_extract
#' @importFrom lubridate as_datetime
#' @importFrom tibble as_tibble

aa_transform <- function(resp) {
  `%>%` <- magrittr::`%>%`
  if (class(resp) != "response") {
    .code <- resp$code
    .message <- resp$message
  } else if (class(resp) == "response") {
    .method <- resp$request$method
    .sym <- stringr::str_extract(resp$request$url, "\\w+$")
    .code <- resp$status_code
    #browser()
    .resp <- response_text_clean(resp)
    .message <- .resp$message
  }
  
  if(any(grepl(pattern = "^4", x = .code))) {
    rlang::warn(paste("Activities not retrieved.\n Message:", .message))
    return(.resp)
  }
  
  #Check if any pos exist before attempting to return
  if(length(.resp) == 0) {
    message("No Activities available with specified criteria.")
    out <- .resp
  } else if(length(.resp) > 1) {
    .resp <- tibble::as_tibble(.resp)
    # coerce to numeric in positions objects
    suppressMessages({
      out <- .resp %>%
        dplyr::mutate_at(dplyr::vars(transaction_time), ~lubridate::as_datetime(., tz = Sys.timezone())) %>% 
        dplyr::mutate_at(dplyr::vars(price, qty, leaves_qty, cum_qty), as.numeric)
    })
    
  }
  return(out)
}

# Format orders to workable and readable format before returning
#' @title Convert money strings to numeric
#' 
#' @description remove $ from cash quantity strings and convert to numeric
#' @keywords internal 
#' @importFrom stringr str_replace_all
toNum <- function(x){
  as.numeric(stringr::str_replace_all(x, "\\$|\\,", ""))
}

#' @title Transform order objects
#' 
#' @description Replaces plain text quantities and dates with respective R objects
#' @param orders A dataframe returned from any orders_* endpoint
#' @return \code{(tibble)}  with respective R compliant objects (numeric, POSIXct/Datetime, character)
#' @keywords internal
#' @importFrom dplyr mutate_at mutate_if vars ends_with
#' @importFrom lubridate ymd_hms
#' @importFrom tibble as_tibble
#' @importFrom rlang `%||%` warn
#' @importFrom purrr map

orders_transform <- function(o) {
  if (class(o) == "response") {
    if (length(o$content) == 0 && grepl("^2", o$status_code)) {
      message(paste0("Order canceled successfully"))
    } else if (grepl("^5", o$status_code)) {
      rlang::warn("Failed to cancel order.")
    }
    .method <- o$request$method
    .code <- o$status_code
    .o <- response_text_clean(o)
    .message <- .o$message
  } else if (class(o) != "response") {
    .code <- 200
    .o <- o
  }
  
  if (grepl("^4", .code)) {
    rlang::warn(paste0("Code: ",.code,",\nMessage:", .message))
    return(.o)
  }
  if ((is.list(.o) && length(.o) > 0) || ("body" %in% names(.o) && .method == "DELETE")) {
    if (.method == "DELETE") {.o <- .o$body;.q <- .o[1:2]}
    .o <- tibble::as_tibble(purrr::map(.o, rlang::`%||%`, NA))
    suppressMessages({
      suppressWarnings({
        .o <- dplyr::mutate_at(.o, dplyr::vars(dplyr::ends_with("at")),list(~lubridate::ymd_hms(., tz = Sys.timezone())))
        out <- dplyr::mutate_if(.o, ~is.character(.) && !is.na(as.numeric(toNum(.))), list(toNum))  
      })})
  } else if (length(.o) == 0 && .method == "GET") {
    message(paste("No orders for the selected query/filter criteria.","\nCheck `ticker_id` or set status = 'all' to see all orders."))
    out <- .o
  } else if (.method == "DELETE") {
    # case when deleting single order
    out <- .o
  }
  if (exists(".q", inherits = F)) attr(out, "query") <- .q
  return(out)
}


#' @title order_check
#' @description smart detect: type, order_class, extended_hours. Fix names for take_profit, stop_loss if partialled. Throw errors/warnings for specific criteria
#' @param penv \code{environment} the parent environment, otherwise a named list of arguments from the parent environment
#' @param ... named arguments. Will automatically get arguments from enclosing environment. 
#' @return \code{(list)} returns list with appropriate arguments, to be merged with parent environment via `list2env`
#' @keywords internal
#' @importFrom rlang abort current_env env_bind env_get caller_env `!!!` `%||%`
#' @importFrom purrr imap_chr map imap
order_check <- function(penv = NULL, ...) {
  # get rlang fns while testing (so you don't have to manually load the package each time)
  `!!!` <- rlang::`!!!`
  `%||%` <- rlang::`%||%`
  # add the arguments to the environment ----
  # Thu Apr 30 17:29:18 2020
  .o <- try(list2env(as.list(penv), environment()))
  if (class(.o) == "try-error"){
    .vn <- list(ticker_id = "character", action = "character", type = "character", qty = c("numeric","integer"), side = "character", time_in_force = "logical", limit = c("numeric","integer"), stop = c("numeric","integer"), extended_hours = "logical", client_order_id = "character", order_class = "character", take_profit = "list", stop_loss = "list")
    .e <- list(...)
    fetch_vars(.vn, e = .e)
  }
  # ticker_id ----
  # Fri May 01 11:15:39 2020
  # Check if ticker is id
  .is_id <- is_id(ticker_id)
  if (action == "s" && isTRUE(.is_id) && is.null(order_class)) {
    #if ticker_id is ID, action is submit and qty is NULL, populate qty from previous order
    .oo <- orders(ticker_id)
    if (.oo$side == "buy") {
      side <- "sell";message("`side` set to 'sell'")
      if (is.null(qty)) qty <- .oo$qty;message(paste0("`qty` set to ",qty))
      ticker_id <- .oo$symbol;message(paste0("`ticker_id` set to ",.oo$symbol))
      if (isTRUE(client_order_id)) client_order_id <- .oo$id;message(paste0("`client_order_id` set to ", .oo$id))
    }
  } else if (!isTRUE(.is_id)) {
    #Convert ticker argument to upper if action is submit
    ticker_id <- toupper(ticker_id)
  }
  
  # if side is partialled or missing ----
  # Thu Apr 30 20:32:52 2020
  if (action == "s") {
    if (!is.null(side)) {
      side <- tolower(substr(side, 0, 1))
      side <- ifelse(side == "b", "buy", "sell")
    } else if (is.null(side)) {
      if ((order_class %||% "none") %in% c("bracket", "oto")) {
        side <- "buy"
        message("order_class: ", order_class," requires side = 'buy', `side` set to 'buy'.")
      } else if ((order_class %||% "none") == "oco") {
        side <- "sell"
        message("order_class: 'oco' requires side = 'sell', `side` set to 'sell'.")
      } else {
        rlang::abort("`side` is required for order submissions.")
      }
    }
    
    # if quantity is missing ----
    # Thu Apr 30 20:17:38 2020
    if (is.null(qty)) {
      rlang::abort("qty must be set.")
    }
    # fix names for take_profit and stop_loss
    if (!is.null(take_profit)) names(take_profit) <- "limit_price"
    if (!is.null(stop_loss)) {
      .n <- purrr::imap_chr(stop_loss, ~{
        if (grepl("^l", .y, ignore.case = T)) "limit_price" else "stop_price"
      })
      names(stop_loss) <- .n
    }
    # set type if partialled and order_class is NULL  ----
    # Thu Apr 30 20:20:16 2020
    
    if (!is.null(type) && is.null(order_class)){
      type <- tolower(type)
      if (grepl("s", type) && grepl("l", type)) {
        type <- "stop_limit"
      } else if (substr(type,1,1) == "s") {
        type <- "stop"
      } else if (substr(type,1,1) == "l") {
        type <- "limit"
      } else if (substr(type,1,1) == "m") {
        type <- "market"
      }
    } else if ((order_class %||% "none") == "bracket" && (type %||% "none") != "market") {
      message("order_class: 'bracket' requires type = 'market'. `type` set to 'market'.")
      type <- "market"
    } else if ((order_class %||% "none") == "oco" && (type %||% "none") != "limit") {
      
      message("order_class: 'oco' requires type = 'limit'. `type` set to 'limit'.")
      type <- "limit"
    } else if ((order_class %||% "none") == "oto" && (type %||% "none") != 'market') {
      message("order_class: 'oto' requires type = 'market'. `type` set to 'market'.")
      type <- "market"
    }
    
    
    if (is.null(order_class)) {
      # smart detect type in the absence of order_class
      if (is.null(type) && !is.null(limit) && is.null(stop)) {
        # if just limit is provided
        if (is.null(stop) && is.null(type)) {
          type <- "limit";message("`type` set to 'limit'")
        }
      } else if (is.null(type) && !is.null(stop) && is.null(limit)) { 
        # if just stop is provided
        if (is.null(limit) && is.null(type)) {
          type <- "stop";message("`type` set to 'stop'")
        }
      } else if (!is.null(stop) && !is.null(limit)) {
        if (is.null(type)) type <- "stop_limit";message("`type` set to 'stop_limit'")
      } else if (is.null(type)) {
        rlang::abort("`type` must be set.")
      }
      # throw errors if not detected or arguments dont match
      if (type == "limit" && is.null(limit)){ 
        rlang::abort(paste0("Please set limit price."))
      } else if (type == "stop" && is.null(stop)) {
        rlang::abort(paste0("Please set stop price."))
      } else if ((is.null(stop) || is.null(limit)) && type == "stop_limit") {
        rlang::abort(paste0(paste0(unlist(purrr::imap(list(stop = stop, limit = limit), ~{
          if (is.null(.x)) .y else NULL
        })), collapse = ", "), " must be set."))
      } 
    } else if (!is.null(order_class)) {
      # if order class is specified, set required arguments accordingly or throw errors
      # order_class Advanced orders ----
      # Thu Apr 30 15:05:26 2020  
      if ((is.null(take_profit) && is.null(stop_loss)) && order_class == "oto") {
        rlang::abort("`take_profit` or `stop_loss` must have at least one parameter set when order_class = 'oto'")
      } else if ((is.null(take_profit) || is.null(stop_loss)) && order_class %in% c('oco','bracket')) {
        rlang::abort("`take_profit` must be set, and `stop_loss` must have at least one parameter set when order_class = 'oco'/'bracket'")
      }
      # parameter parsing, error checking & warnings for advanced orders
      if (order_class == "bracket") {
        if (!time_in_force %in% c("day","gtc")) {
          rlang::abort("time_in_force must be 'day' or 'gtc' when `order_class = 'bracket'. See documentation for details.")
        }
      } 
    }
    if (isTRUE(extended_hours) && (type != "limit" || time_in_force != "day" || order_class %in% c("oco","oto", "bracket"))) rlang::abort(paste0("Extended hours only supports simple 'limit' orders and `time_in_force = 'day'`"))
  } 
  out <- list(ticker_id = ticker_id, action = action, type = type, qty = qty, side = side, time_in_force = time_in_force, limit = limit, stop = stop, extended_hours = extended_hours, client_order_id = client_order_id, order_class = order_class, take_profit = take_profit, stop_loss = stop_loss)
}



# wl_transform ----
# Sun May 03 08:55:01 2020
#' @title Transform watchlist objects
#'
#' @description Replaces timestamps with POSIXct in watchlist info
#' @param wl The watchlist object
#' @return \code{(tibble)} with respective R compliant objects (POSIXct)
#' @keywords internal
#' @importFrom dplyr mutate_at vars select everything ends_with
#' @importFrom magrittr `%>%`
#' @importFrom rlang abort `%||%` 
#' @importFrom tibble as_tibble tibble
#' @importFrom purrr map

wl_transform <- function(wl, action, wl_info = NULL) {
  `%||%` <- rlang::`%||%`
  `%>%` <- magrittr::`%>%`
  if (class(wl) == "response") {
    if (length(wl$content) == 0 && grepl("^2", wl$status_code)) {
      message(paste0("Watchlist deleted successfully"))
      return(tibble::tibble())
    }
    .method <- wl$request$method
    .code <- wl$status_code
    .wl <- response_text_clean(wl)
    .message <- .wl$message
  } else if (class(wl) != "response") {
    .code <- 200
    if (length(wl_info) == 0) {
      wl_info <- attr(wl, "info")
    }
    .wl <- wl
  }
  if(any(grepl(pattern = "^4", x = .code))) {
    rlang::abort(paste("Watchlist was not modified.\n Message:", .message))
    return(.wl)
  }
  # Transform the watchlist info object if it exists
  if (class(wl) == "response") {
    wl_info <- suppressMessages({
      suppressWarnings({
        dplyr::mutate_at(tibble::as_tibble(purrr::map(.wl[1:5], rlang::`%||%`, NA)), dplyr::vars(dplyr::ends_with("at")),list(~lubridate::ymd_hms(., tz = Sys.timezone()))) %>% 
          dplyr::select(name, updated_at, dplyr::everything())
      })
    })
  }
  
  if ("assets" %in% names(.wl)) {
    # If it's a specific watchlist, make the watchlist info an attribute
    out <- .wl$assets
    attr(out, "info") <- wl_info
  } else if (is.data.frame(wl) && length(wl_info) > 0) {
    out <- wl
    attr(out, "info") <- wl_info
  } else if (length(.wl)  == 5) {
    # if it's an array of watchlists
    out <- wl_info 
  } else if (action == "d") {
    # if empty, return empty tibble
    out <- tibble::tibble()
  }
  out
}

# wl_nm2id ----
# Sun May 03 08:54:50 2020
#' @title Look up watchlist id from name
#'
#' @description fetch the watchlist id corresponding to a watchlist name
#' @param nm \code{(character)} *required* the name of the watchlist
#' @inheritParams account
#' @return id \code{(character)} the id of the watchlist OR, if no id, the array of watchlists
#' @keywords internal
#' @importFrom httr GET build_url
#' @importFrom rlang warn caller_env env_bind env_get is_named `!!!`
#' @importFrom purrr map
wl_nm2id <- function(nm, ...) {
  `!!!` <- rlang::`!!!`
  # if watchlist_id is a name
  # get the list of watchlists
  .vn = list(v = c("integer", "numeric"), .url = c("character", "url"))
  if (!all(.vn %in% ls(all.names = T))){
    .e <- list(...)
    fetch_vars(.vn, e = .e)
  }

  headers <- get_headers()
  .url$path <- list(paste0("v", v), "watchlists")
  .url <- httr::build_url(.url)
  watchlist = httr::GET(url = .url, headers)
  watchlist = response_text_clean(watchlist)
  # get the id
  id <- watchlist$id[watchlist$name %in% nm]
  if (length(id) == 0) {
    rlang::warn(paste0("No watchlist by that name, did you mean ", agrep(nm, watchlist$name, ignore.case = T, value = T),"?"))
    id <- watchlist
  }
  return(id)
}

# .mode ----
# Sun May 03 08:54:39 2020
#'@title get the mode
#'@keywords internal
.mode <- function (v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# poly_transform ----
# Sun May 03 08:54:26 2020
#'
#' @title transform polygon.io reference endpoints
#' @keywords internal 
#' @description coerce data objects returned from the various polygon.io endpoints to R compliant objects
#' @param resp The response object from httr
#' @param ep The endpoint
#' @return \code{list/data.frame/tibble} Either a list or tibble depending on the endpoint
#' @importFrom rlang warn expr call2 `%||%` is_quosures `!!!` `!!` is_expression
#' @importFrom lubridate as_date as_datetime origin
#' @importFrom dplyr vars mutate_at rename
#' @importFrom purrr map_lgl modify_depth map_int map_if walk2
#' @importFrom tibble tibble as_tibble

poly_transform <- function(resp, ep) {
  `%||%` <- rlang::`%||%`
  `!!!` <- rlang::`!!!`
  `!!` <- rlang::`!!`
  .code <- resp$status_code
  .resp <- response_text_clean(resp)
  .message <- .resp$error
  # check for errors
  if(any(grepl(pattern = "^4", x = .code))) {
    rlang::warn(paste(.ep[[ep]]$nm, "endpoint error.\n Message:", .message))
    return(.resp)
  }
  
  if (ep == "t") {
    .o <- list(.tbl = .resp$tickers, .vars = c("updated"), .f = lubridate::as_date, .q = .resp[1:4])
  } else if (ep == "tt") {
    attr(.resp$results ,"query") <- .resp[1]
    return(.resp$results)
  } else if (ep == "td") {
    .o <- list(.tbl = .resp, .vars = c("updated", "listdate"), .f = lubridate::as_date)
  } else if (ep == "tn") {
    .o <- list(.tbl = .resp, .vars = c("timestamp"), .f = lubridate::as_datetime)
  } else if (ep %in% c("m", "l")) {
    .o <- list(.tbl = .resp$results, .q = .resp[1])
  } else if (ep %in% c("sd","ss")) {
    .o <- list(.tbl = .resp$results, .vars = dplyr::vars(dplyr::ends_with("Date")), .f = lubridate::as_date, .q = .resp[1:2])
  } else if (ep == "sf") {
    .o <- list(.tbl = .resp$results, .vars = c('calendarDate', 'reportPeriod', 'updated', 'dateKey'), .f = lubridate::as_date)
  } else if (ep == "ms") {
    suppressMessages({
      .resp$serverTime <- lubridate::as_datetime(.resp$serverTime, tz = "America/New_York")
    })
    return(.resp)
  } else if (ep == "mh") {
    .o <- list(.tbl = .resp, .vars = list(c("date"), c("open", "close")), .f = list(lubridate::as_date, lubridate::as_datetime))
  } else if (ep == "e") {
    .o <- list(.tbl = .resp)
  } else if (ep %in% c("ht", "hq")) {
    .o <- list(.tbl = dplyr::rename(.resp$results, time = 't'), .vars = "time", .f = rlang::expr(~lubridate::as_datetime(. / 1e9, tz = "America/New_York", origin = lubridate::origin)), .m = .resp$map)
  } else if (ep %in% c("lt", "lq")) {
    .resp$last$timestamp <- lubridate::as_datetime(.resp$last$timestamp / 1e3, origin = lubridate::origin, tz = Sys.timezone())
    .o <- list(.tbl = .resp$last, .q = .resp[purrr::map_lgl(.resp, ~!is.list(.x))])
  } else if (ep == "do") {
    .o <- list(.tbl = .resp[-1], .vars = "from", .f = rlang::expr(~lubridate::as_datetime(., tz = "America/New_York")), .q = .resp[1])
  } else if (ep == "cm") {
    return(tibble::tibble(CM = as.character(.resp)))
  } else if (ep == "sa") {
    .o <- list(.tbl = .resp$tickers[1:9], .vars = c("lastQuote.t", "lastTrade.t", "updated"), .f = rlang::expr(~lubridate::as_datetime(. / 1e9, tz = "America/New_York", origin = lubridate::origin)), .q = .resp[1:2])
  } else if (ep == "st") {
    .o <- list(.tbl = .resp$ticker[1:9], .vars = c("lastQuote.t", "lastTrade.t", "updated"), .f = rlang::expr(~lubridate::as_datetime(. / 1e9, tz = "America/New_York", origin = lubridate::origin)), .q = .resp[1])
  } else if (ep == "sg") {
    .o <- list(.tbl = .resp$tickers[1:9], .vars = c("lastQuote.t", "lastTrade.t", "updated"), .f = rlang::expr(~lubridate::as_datetime(. / 1e9, tz = "America/New_York", origin = lubridate::origin)), .q = .resp[1])
  } else if (ep %in% c("pc", "gd")) {
    .o <- list(.tbl = dplyr::rename(.resp$results, time = 't', volume = "v", open = "o", high = "h", low = "l", close = "c", ticker = "T"), .vars = "time", .f = rlang::expr(~lubridate::as_datetime(. / 1e9, tz = "America/New_York", origin = lubridate::origin)), .q = .resp[1:5])
  } else {
    
  }
  # Check for no result
  if (length(.o$.tbl) == 0) {
    rlang::warn(paste0("Query returned no results. If metadata exists it will be returned"))
    .o$.vars <- NULL
  } else {
    if (is.data.frame(.o$.tbl$day) || is.list(.o$.tbl$day)) {
      .t <- unlist(.o$.tbl[1:5], recursive = F)
      if (ep == "st") .t <- purrr::map_if(.t, ~length(.x) > 1 || is.null(.x), list)
      .o$.tbl <- dplyr::bind_cols(.t, .o$.tbl[6:9]) 
    }
  .o$.tbl <- purrr::modify_depth(.o$.tbl, .depth = -1, rlang::`%||%`, y = NA, .ragged = T)
  .m <- .mode(purrr::map_int(.o$.tbl, length))
  .o$.tbl <- purrr::map_if(.o$.tbl, ~length(.x) > .m, ~list(.x))
  .o$.tbl <- tibble::as_tibble(.o$.tbl, .rows = .m)
  }
  if (!is.null(.o$.vars)) {
    # if there are vars to be changed
    out <- list()
    # ensure the objects are properly nested
    if (!is.list(.o$.f)) .fn <- list(.o$.f) else .fn <- .o$.f
    if (!is.list(.o$.vars) && is.character(.o$.vars)) .v <- list(.o$.vars) else .v <- .o$.vars
    # map over the lists and apply the transformations
    if (is.list(.o$.vars) && !rlang::is_quosures(.o$.vars)) {
      purrr::walk2(.v, .fn, ~{
        if (is.character(.x)) .x <- dplyr::vars(!!(.x))
        if (length(out) == 0) .t <- .o$.tbl else .t <- out
        out <<- do.call(dplyr::mutate_at, args = list(.tbl = .t, .vars = .x, .f = .y))
      })
    } else if (is.function(.o$.f)) {
      out <- do.call(dplyr::mutate_at, args = .o[1:3])
    } else if(rlang::is_expression(.o$.f)) {
      out <- eval(rlang::call2(dplyr::mutate_at, !!!(list(.tbl = .o$.tbl, .vars = .o$.vars, .f = .o$.f))))
    }
    
  } else {
    out <- .o$.tbl
  }
  if (!is.null(.o$.q)) attr(out, "query") <- .o$.q
  if (!is.null(.o$.m)) attr(out, "map") <- .o$.m
  return(out)
}
#' @title Update Websocket message objects
#' @keywords internal
#' 
#' Used in ws_create to to instantiate and update websocket message objects
#' @importFrom dplyr bind_rows
#' @importFrom tibble tibble
#' @importFrom lubridate now
#' @importFrom stringr str_remove
ws_msg <- function(out, .o = NULL, msg, toConsole = T) {
  # Update the last message
  if (exists("lastmessage", out$env)) rm(list = "lastmessage", envir = out$env)
  assign("lastmessage", msg, out$env)
  if (toConsole) cat("Message: ", msg, "\n")
  if (exists("msgs", out$env)) {
    wsmsg <- get("msgs", out$env)
    wsmsg <- dplyr::bind_rows(wsmsg, tibble::tibble(Timestamp = lubridate::now(tz = Sys.timezone()), Message = stringr::str_remove(msg, "^\\d{4}\\-\\d{2}\\-\\d{2}\\s\\d{2}\\:\\d{2}\\:\\d{2}\\,\\s")))
    # if the object has reached 1/3rd of the allowable memory allocation
    if (object.size(wsmsg) / (memory.size(NA) * 1048567) > .33) {
      # half it's size by removing the first half
      wsmsg <- wsmsg[- c(1:(nrow(wsmsg) %/% 2)),]
    }
    assign("msgs", wsmsg, out$env)
  } else {
    assign("msgs", tibble::tibble(Timestamp = lubridate::now(tz = Sys.timezone()), Message = msg), out$env)
  }
  if (!is.null(.o)) {
    if (.o$ev %in% c("T", "Q", "A", "AM")) {
      if (!exists("bars", envir = out$env, inherits = F)) {
        bars <- list()
        bars[[paste0(.o$ev,".",.o$sym)]] <- tibble::as_tibble(.o)
        assign("bars", bars, out$env)
      } else {
        .bars <- get0("bars", out$env, inherits = F)
        .nm <- paste0(.o$ev,".",.o$sym)
        .bars[[.nm]] <- dplyr::bind_rows(.bars[[.nm]], tibble::as_tibble(.o))
        if (object.size(.bars) / (memory.size(NA) * 1048567) > .33) {
          # half it's size by removing the first half
          .bars <- purrr::map(.bars, ~{
            .x[- c(1:(nrow(.x) %/% 2)),]
          })
        }
        assign("bars", .bars, out$env)
      }
    }
  }
}

#' @keywords internal
#' @title ws_log
#' @description Performs logging of streaming bars data based on input options to ws_create
#' @param .o `(list)` The raw message content from the Websocket
#' @param out `(list)` The ws_create out object
#' @param log_bars `(logical)` The flag as to whether to log bars on the drive as CSV or not
#' @return bars `(list)` object in the out$env environment in the object returned from `ws_create` with the previously transmitted data as a `tibble` for each polygon subscription channel, each named according to the channel from which it came. Additionally, a CSV named by the Subscription channel if `logbars = T` in the local or specified directory with the same data.
#' @details The rows of the each of the bars objects are halved if it's size reaches .33 of the memory allocated to R. Prevents memory overflow and potential freezing. 
#' @importFrom rlang `!!!` is_named env_get env_bind current_env caller_env
#' @importFrom purrr map
ws_log <- function(..., penv = NULL) {
  if (!.log) return(NULL) # stop if no logging
  `!!!` <- rlang::`!!!`
  # add the arguments to the environment ----
  # Thu Apr 30 17:29:18 2020
  .e <- try(list2env(as.list(penv), environment()))
  .vn <- c(.o = ".o", .log = ".log", .msg = ".msg", out = "out", log_bars = "log_bars", log_msgs = "log_msgs", log_path = "log_path", logfile = "logfile")
  if (!all(.vn %in% ls(all.names = T))){
    .e <- list(...)
    fetch_vars(.vn, e = .e)
  }
  # If listening to a subscription chacnnel & logging bars
  if (.o$ev %in% c("T", "Q", "A", "AM") && log_bars) {
    # Create the name of the CSV log for Polygon channels
    .log_ev <- paste0(log_path, paste0(.o$ev,".",.o$sym,".csv"))
    
    # if the file doesnt exist, create it
    if (!file.exists(.log_ev)) {
      file.create(.log_ev)
      write(paste0(paste0(names(.o), collapse = ", "),"\n"), file = .log_ev, append = T)
    } 
    write(paste0(paste0(.o, collapse = ", "),"\n"), file = .log_ev, append = T)
  }
  
  if (log_msgs) write(.msg, file = logfile, append = T)
}
