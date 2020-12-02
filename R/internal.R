
# Helper functions for market_data ----
# Sat Mar 28 09:40:23 2020

#  Re-exports ----
# Sun Sep 20 11:15:14 2020
#' @keywords internal
dplyr::`%>%`

#' @keywords internal
rlang::`%||%`

#' @keywords internal
rlang::`!!!`

#' @keywords internal
rlang::`!!`

#' @keywords internal
rlang::`%|%`

valid_date <- function(.x, .out, v) {
  purrr::walk2(.x, .out, ~{
    if (is.na(.out) || lubridate::year(.out) < 1792 || lubridate::year(.out) > lubridate::year(lubridate::today()) + 50) {
      warning(
        paste(.x,"was parsed to", .out, ". Is this expected?")
        , immediate. = TRUE
        , call. = FALSE)
    }
  })
}

try_date.character <- function(.x, tz, v) {
  .orders <- c("Ymd", "mdY", "dmY", "ymd", "mdy", "dmy")
  if (grepl("\\:", .x)) {
    # if a character and datetime
    .out <- lubridate::parse_date_time(.x, orders = paste(.orders,"R"), tz = tz)
  } else {
    # if a date
    .out <- lubridate::parse_date_time(.x, orders = .orders, tz = tz)
  }
  return(.out)
}

try_date.default <- function(.x, tz, v) {
  .out <- lubridate::force_tz(.x, tzone = tz)
} 

try_date.double <- function(.x, tz, v) {
  if (all(.x < 100000)) {
    .out <- lubridate::as_date(.x, origin = lubridate::origin)
  } else {
    
    .out <- lubridate::as_datetime(signif(as.numeric(paste0(".",round(.x,0))), 10) * 10 ^ 10, origin = lubridate::origin, tz = tz)
  }
  return(.out)
}
  
# try_date ----
# Sat Apr 18 11:55:17 2020
#' @title String to Date/Datetime conversion for market_data
#' @keywords internal
#' 
#' @description Attempts to coerce string to a valid date object in stock exchange time zone, or checks to see if it is already. If it fails to coerce to date, returns NA.
#' @importFrom purrr keep
#' @importFrom dplyr between
#' @importFrom lubridate parse_date_time force_tz origin as_date as_datetime year
#' @importFrom rlang is_empty


try_date <- function(.x, tz = "America/New_York") {
  if (inherits(.x, c("Date", "Datetime", "POSIXct", "POSIXlt"))) {
    .out <- try_date.default(.x, tz, evar$v)
  } else if (inherits(.x, "character")) {
    .out <- try_date.character(.x, tz, evar$v)
  } else if (inherits(.x, c("numeric", "integer"))) {
    .out <- try_date.double(.x, tz, evar$v)
  }
  valid_date(.x, .out)
  return(.out)
}

is_inf <- function(.x) {
  if (is.null(.x)) return(FALSE)
  # check for infinite
  out <- tryCatch(is.infinite(.x), error = function(e) FALSE)
  # if more than on object returned, then it's obviously not a single infinite
  out <- ifelse(length(out) > 1, FALSE, out)
  out
}

#' @title fetch variables 
#' @description Fetches variables from environment of containing function for use in internal function
#' @keywords internal
#' @param .vn named vector of variable names with their corresponding classes - to be tested with \link[base]{inherits}
#' @param e The list made from ellipsis if parameters are input as ellipsis argument
#' @param evar Environment with variables passed to top level of function
#' @param cenv The caller environment, stored automatically
#' @param penv The parent of the caller environment, also stored automatically
#' @param sf The system frames for searching if not found in previous two environments.
#' @importFrom rlang `!!!` env_bind env_get_list caller_env is_empty
#' @importFrom stringr str_extract
#' @importFrom purrr discard map2_lgl walk


fetch_vars <- function(.vn, e = NULL, evar = get0("evar", mode = "environment", envir = rlang::caller_env(), ifnotfound = new.env()), cenv = rlang::caller_env(), penv = parent.env(rlang::caller_env()), sf = rev(sys.frames())) {
  .top_level_vars <- ls(envir = evar, pattern = "^[^\\.]") 
  # get just the variables from the top level call that are not passed into the function explicitly (such that tf_reduce timeframe functionality works properly)
  .nms <- .top_level_vars[!.top_level_vars %in% names(e)]
  e <- append(e, rlang::env_get_list(env = evar, nms = .nms, default = NULL))
  try(list2env(e, envir = cenv), silent = TRUE)
  # remove the variables now in the current environment
  .vars_rem <- .vn[!names(.vn) %in% ls(all.names = TRUE, envir = cenv)]
  if (!rlang::is_empty(.vars_rem)) {
    .vars <- purrr::walk(append(sf, penv, after = 0), ~ {
      if (identical(globalenv(), .x))
        return(NULL)
      if (length(.vars_rem) == 0)
        return(NULL)
      .v <-
        rlang::env_get_list(
          env = .x,
          names(.vars_rem),
          default = Inf,
          inherit = FALSE
        )
      .v <- try({
        purrr::discard(.v, is_inf)
      })
      .v <- .v[purrr::map2_lgl(.v, .vn[names(.v)], ~ inherits(.x, .y))]
      .vars_rem <<- .vars_rem[!names(.vars_rem) %in% names(.v)]
      rlang::env_bind(cenv,!!!.v)
    })
  }  
  
    
  
  # if (!all(.vn %in% ls(all.names = T, env = cenv))) {
  #   browser()
  #   stop(paste0(stringr::str_extract(as.character(match.call()), "^\\w+")[1], " is missing necessary variables"))
  # }
  
}



#' @title convert timeframe to numeric `tf_num`
#' @description If `tf_num` is not found in environment, create it from `timeframe`. Throw errors if arguments are unacceptable
#' @inheritParams market_data
#' @param cenv Calling environment in which to assign variables
#' @param ... additional arguments
#' @return tf_num & timeframe in calling environment. 
#' @keywords internal
#' @importFrom purrr map_chr
#' @importFrom dplyr `%>%`
#' @importFrom rlang abort warn caller_env env_bind
#' @importFrom utils tail

tf_num <- function(timeframe, multiplier, ..., cenv = rlang::caller_env(), evar = get0("evar", mode = "environment", envir = rlang::caller_env())) {
  .e <- list(...)
  fetch_vars(evar$.vn["v"], e = .e)
  #quick detection of timespan abbreviations:  Thu Mar 26 08:34:00 2020 ----
  .tf_opts <- list(m = c("m","min","minute"), h = c("h", "hour"), d = c("d", "day"), w = c("w", "week"), M = c("M", "mo", "month"), q = c("q", "quarter"), y = c("y", "year"))
  # Create ordered factor or timeframe options
  tf_order <- purrr::map_chr(.tf_opts, utils::tail, 1) %>% {factor(., levels = .)}
  
  if (v == 1) {
    .t <- grep(stringr::regex(timeframe, ignore_case = T), .tf_opts[c(1,3)], ignore.case = T)
    if (length(.t) < 1) {
      stop(paste0("`",timeframe,"` is not a valid timeframe for the v1 API. See ?market_data documentation."))
    }
    # Get the timeframe
    timeframe <- utils::tail(.tf_opts[c(1,3)][[.t]], 1)
    
    # Check args
    if (timeframe == "minute" && !any(multiplier %in% c(1,5,15))) {
      rlang::abort("The v1 API only accepts multipliers of 1,5,15 for the minute timeframe")
    } else if (timeframe == "day" && multiplier != 1) {
      rlang::warn("The v1 API only accepts 1 as a `multiplier` for the day timeframe. One day bars will be returned.", class = "warning")
      multiplier <- 1
    }
    
  } else if (v == 2){
    timeframe <- utils::tail(.tf_opts[[grep(substr(ifelse(timeframe == "month", "Month", as.character(timeframe)), 0 , 1), names(.tf_opts), ignore.case = FALSE)[1]]], 1)
  }
  # Get the timeframe as a numeric
  tf_num <- which(tf_order %in% timeframe)
  rlang::env_bind(cenv, tf_num = tf_num, timeframe = timeframe, multiplier = multiplier, tf_order = tf_order)
}
#' @title Create API amenable boundaries based on user input for market_data

#' @description Internal function for fixing input date boundaries. The function is intended for use in an environment where its parameters already exist as it will use the variables from it's parent environment. See notes on specifying arguments in the *Details* section for \code{\link[AlpacaforR]{bars_complete}}.
#' @inheritParams market_data
#'@param fc Toggle for `first_call` mode that prints all messages to console. 
#'@return \code{(list)} A list with two Date/Datetime items that are boundaries for the date range necessary to call the API with to retrieve the appropriate data from the specified API. Items will have names corresponding to the non-NULL \code{from/to/after/until} arguments in the calling environment or supplied to the function.
#'@keywords internal
#' @importFrom purrr map compact imap keep
#' @importFrom lubridate force_tz wday interval floor_date ceiling_date days hour as_datetime as_date
#' @importFrom dplyr `%>%`
#' @importFrom stats setNames update
bars_bounds <- function(from = NULL, after = NULL, to = NULL, until = NULL, timeframe, multiplier, fc = NULL, evar = get0("evar", mode = "environment", envir = rlang::caller_env()), ...) {

  #trickery to get the variables from the calling environment
  .e <- list(...)
  fetch_vars(evar$.vn[c("v")], e = .e)
  if (!"tf_num" %in% ls(all.names = T)) tf_num(timeframe, multiplier)
  
  # Set defaults if non specified
  if (is.null(from) && is.null(after)) {
    from <- Sys.Date() - 7
    message(paste0("`from` argument omitted, setting to ", from))
  }
  if (is.null(to) && is.null(until)) {
    to <- Sys.Date()
    message(paste0("`to` argument omitted, setting to ", to))
  }
  .ls <- ls(all.names = T)
  if (all(c("from", "to") %in% .ls) || all(c("after", "until") %in% .ls)) {
    .date_vars <- list(from = from, after = after, to = to, until = until)
  }
  # Remove NULL arguments
  bounds <- purrr::compact(.date_vars)
  if ((missing(fc) || isFALSE(get0("fc", inherits = FALSE, mode = "logical"))) && length(bounds) > 2) {
    
    # in some instances when bars_missing is called, after was supplied by the user, but the from-to arguments are being used. fetch_vars will zap all three arguments, and make bars_bounds inaccurate. To accommodate this case, grab the argument names passed in the call and use those. If only one argument was passed (and values were filled internally), switch is used to add the other argument and sort alphabetically such that the order of the dates will be accurate (since [f]rom <  [t]o & [a]fter < [u]ntil).
    .b <- purrr::keep(names(match.call()), ~nchar(.x) > 0 && .x %in% c("from", "to", "after", "until"))
    if (length(.b) != 2) {
      .b <- sort(c(.b, switch(.b,
                               from = "to",
                               to = "from", 
                               after = "until",
                               until = "after")))
      
    }
    bounds <- bounds[.b]
  }
  # Coerce to floor/ceiling dates/datetimes or fail with NA
  bounds <- purrr::imap(bounds, ~{
    .out <- try_date(.x)
    
    # Floors or ceilings
    if (tf_num < 3) {
      # Case less than day, to include the day requested, the boundary must be the previous day
      .unit <- ifelse(v == 2, paste(1, "day"), paste(multiplier, timeframe))
      .wd <- lubridate::wday(.out, label = TRUE, abbr = FALSE)
      if (.wd %in% c("Sunday", "Saturday")) {
        warning(paste0("`",.y, "` is a ", .wd, ". Timeframes less than one week will return no data for weekend days."), call. = FALSE, immediate. = TRUE)
      }
    }  else {
      .unit <- paste(multiplier, timeframe)
    }
    
    if (.y == "from" || .y == "after") {
      .oout <- .out
      if (isTRUE(.y == "from")) {
          .call <- rlang::expr(lubridate::floor_date(.out, .unit))
        if (multiplier > 1 && timeframe == "week") {
          .call <- rlang::expr(suppressWarnings(!!.call) + lubridate::weeks(multiplier))
        }
          .out <- rlang::eval_bare(.call)
      }
      
      if (!identical(.out,.oout) && isTRUE(fc)) {
        message(paste0("'from'/'after' coerced to ", .out," to retrieve inclusive data."))
      }
      
    } else {
      .oout <- .out
      # To include the ending boundary, we need to add a full cycle of multiplier * timeframe
      if (isTRUE(.y == "to")) {
        .call <- rlang::expr(lubridate::ceiling_date(.out, .unit, change_on_boundary = FALSE))
        if (multiplier > 1 && timeframe == "week") {
          .call <- rlang::expr(suppressWarnings(!!.call) + lubridate::weeks(multiplier))
        }
        .out <- rlang::eval_bare(.call)
      }
      
      if (!identical(.out,.oout) && isTRUE(fc)) {
        message(paste0("'to'/'until' coerced to ", .out, " to retrieve inclusive data."))
      }
    }
    # For requests to the v2 API where aggregates are yearly, the floor date needs to be 12/31
    if (timeframe == "year") {
      .out <- .out - lubridate::days(1)
    } else if (timeframe == "quarter") {
      # Polygon uses the 30th of month multiples of 3 for quarters
      .out <- stats::update(.out - lubridate::days(3), days = 30)
    }
    .out
  }) 
  
  # API version specific bounds
  if (v == 1) {
    if (timeframe == "minute" && (lubridate::interval(bounds[[1]], bounds[[2]]) / (60 * 60 * 24)) < 1) {
      if (!any(lubridate::hour(seq.POSIXt(from = bounds[[1]], to = bounds[[2]], by = paste0(multiplier, " ",ifelse(timeframe == "minute","min",timeframe),"s"))) %in% 9:16)) {
        warning("Date range supplied does not include typical market hours, the V1 API is unlikely to return data", call. = FALSE, immediate. = TRUE)
      }
    }
  
    bounds <- purrr::imap(bounds, ~{
      if ((.y == "from" || .y == "after") && inherits(.x, "Date")){
        # If it's the start/after - use the beginning of the trading day
        .dt <- lubridate::as_datetime(paste0(.x, " 07:00"), format = "%Y-%m-%d %H:%M", tz = "America/New_York")
        
      } else if (inherits(.x, "Date")) {
        # If its the end/until - use the end of the trading day
        .dt <- lubridate::as_datetime(paste0(.x, " 19:00"), format = "%Y-%m-%d %H:%M", tz = "America/New_York")
      } else {
        .dt <- .x
      }
      .dt
    })
  } else if (v == 2) {
    bounds <- purrr::map(bounds, ~lubridate::force_tz(lubridate::as_date(.x), "America/New_York"))
    if (all(names(bounds) %in% c("from", "to"))) {
      bounds <- stats::setNames(bounds, c("from", "to"))
    }
  }

  return(bounds)
}

# bars_url ----
# Sun Mar 29 16:07:34 2020
#' @title Create URLs for market_data endpoints
#'
#'
#' @description Internal function for generating query urls. The function is intended for use in an environment where its parameters already exist. See notes on specifying arguments in the *Details* section for \code{\link[AlpacaforR]{bars_complete}}.
#' @inheritParams market_data
#' @param bounds Output from \code{\link[AlpacaforR]{bars_bounds}}. 
#' @return \code{(character)} URL to be called with \code{\link[AlpacaforR]{bars_get}}
#' @keywords internal
#' @importFrom rlang is_named env_bind current_env caller_env env_get "!!!" is_named
#' @importFrom purrr map_chr map
#' @importFrom lubridate as_date
#' @importFrom stringr str_extract str_sub
#' @importFrom httr parse_url build_url
#' @importFrom utils URLdecode
bars_url <- function(ticker, timeframe, multiplier, bounds, ..., evar = get0("evar", mode = "environment", envir = rlang::caller_env())) {
  
  .vn <- evar$.vn[c(
    "v",
    "unadjusted",
    "limit"
  )]
  .e <- list(...)
  fetch_vars(.vn, e = .e)
  
  # Format ticker:  Thu Mar 26 08:48:03 2020 ----
  if (v == 1) {
    .ticker = ifelse(length(ticker) > 1, paste0(trimws(ticker), collapse = ","), ticker)
  } else if (v == 2) {
    .ticker = ticker
  }
  .ticker <- toupper(.ticker)
  #Limit :  Thu Mar 26 08:50:30 2020 ----
  # If limit is null or full = T OR if limit > 1000 then set at 1000 for v1
  if (v == 1) {
    if (is.null(limit) || isTRUE(limit > 1000)) {
      if (isTRUE(limit > 1000)) message("`limit` cannot exceed 1000, coercing to 1000.")
      limit <- 1000
    }  
  }
  # timeframe is quote or trade ----
  # Sun Jun 14 16:08:07 2020
  if (v == 1 && timeframe %in% c("t", "q")) {
    .url = httr::parse_url("https://data.alpaca.markets") #Pricing data uses unique URL, see market data API documentation to learn more
    .url <- purrr::map_chr(setNames(ticker, ticker), ~{
      .url$path <- list("v1", ifelse(timeframe == "t", "last","last_quote"), "stocks", .x)
      .url <- utils::URLdecode(httr::build_url(.url))
    })
  } else if (v == 1) {
    .url = httr::parse_url("https://data.alpaca.markets") #Pricing data uses unique URL, see market data API documentation to learn more
    # multiplier & timeframe ----
    # Fri May 08 21:26:15 2020
    timeframe <- ifelse(timeframe == "minute", "Min", "day")
    timeframe <- ifelse(timeframe == "Min", paste0(multiplier, timeframe), timeframe)
    #full = F Make API requests:  Tue Mar 17 21:37:35 2020 ----
    .url$path <- list("v1", "bars", as.character(timeframe))
    # Coerce to appropriately formatted character strings
    bounds <- purrr::map(bounds, ~{
      .x <- format(.x, "%Y-%m-%dT%H:%M:%S%z")
      paste0(stringr::str_sub(.x, 1, -3),":", stringr::str_sub(.x, -2, nchar(.x)))
    })
    # NULL Values are automatically dropped, so only the set boundaries will remain
    .url$query <- list(symbols = .ticker,
                      limit = limit,
                      start = bounds$from,
                      after = bounds$after,
                      end = bounds$to,
                      until = bounds$until
    )
    # Build the url
    
    .url <- utils::URLdecode(httr::build_url(.url))
  } else if (v == 2) {
    .url <- purrr::map_chr(stats::setNames(.ticker, .ticker), ~{
      .url = httr::parse_url(get_url_poly())
      .url$path <- list(
        v = "v2",
        ep = "aggs",
        "ticker",
        ticker = .x,
        "range",
        multiplier = multiplier,
        timeframe = as.character(timeframe),
        from = as.character(lubridate::as_date(bounds[[1]])),
        to = as.character(lubridate::as_date(bounds[[2]]))
      )
      .url$query <- list(unadjusted = unadjusted,
                        apiKey = Sys.getenv("APCA-LIVE-API-KEY-ID"))
      .url <- httr::build_url(.url)
    })
  }
  return(.url)
}


# bars_get ----
# Sun Mar 29 16:07:49 2020
#' @title Retrieve market data from the respective API
#'
#' @description Internal function for retrieving data from Alpaca API and outputting data.frame with query attribute. See notes on specifying arguments in the *Details* section for \code{\link[AlpacaforR]{bars_complete}}.
#' @param url \code{(character)} the url rendered by \code{\link[AlpacaforR]{bars_url}}
#' @return \code{(list)} See \code{\link[AlpacaforR]{market_data}} as the output is the same.
#' @keywords internal
#' @importFrom dplyr `%>%`
#' @importFrom rlang is_named env_bind current_env caller_env env_get warn "%||%"
#' @importFrom purrr iwalk imap imap_dfr modify_if
#' @importFrom httr GET
#' @importFrom lubridate with_tz parse_date_time
#' @importFrom stringr str_extract str_replace
bars_get <- function(url, ..., evar = get0("evar", mode = "environment", envir = rlang::caller_env())) {
  .e <- list(...)
  fetch_vars(evar$.vn[c("v", "timeframe")], e = .e)
  
  if (timeframe %in% c("t","q")) {
    headers = get_headers()
    query <- list()
    bars <- purrr::imap_dfr(url, ~{
      agg_quote = httr::GET(url = .x, headers)
      .query <- list()
      .query$status_code <- agg_quote$status_code
      .query$url <- agg_quote$url
      .query$ts <- lubridate::with_tz(lubridate::parse_date_time(agg_quote[["headers"]][["date"]], "a, d b Y T"), tz = "America/New_York")
      agg_quote <- response_text_clean(agg_quote)
      .out <- bars_transform(agg_quote)
      query[[.y]] <<- .query
      return(.out)
    })
    attr(bars, "query") <- query
  } else if (v == 1) {
    headers = get_headers()
    if (isTRUE(get0(".dbg", .GlobalEnv, "logical", F))) message(paste0("Retrieving\n", url))
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
    if (!timeframe %in% c("q","t")) {
      # only check output if it's a bars request
      purrr::iwalk(agg_quote, ~{
        .warn <- try({is.null(nrow(.x)) || nrow(.x) == 0})
        if (inherits(.warn, "try-error") || get0(".warn", inherits = FALSE, ifnotfound = FALSE)) {
          rlang::warn(paste0(.y, " returned no data."))
        }  
      })
    }
    bars <- bars_transform(agg_quote, timeframe)
    # add query metadata to match v2 api
    bars <- purrr::imap(bars, ~{
      .query$ticker <- .y
      if (!is.null(.x)) attr(.x, "query") <- .query  
    })
  
  } else if (v == 2) {
    # get for v2 API
    if (is.null(names(url))) {
      names(url) <- stringr::str_extract(url, "(?<=ticker\\/)\\w+")
    }
    bars <- purrr::imap(url, ~{
      .url <- .x
      # redact the key
      .murl <- stringr::str_replace(.x, "(?<=\\=)[:alnum:]+$", "[REDACTED]")
      if (isTRUE(get0(".dbg", .GlobalEnv, "logical", F))) message(paste0("Retrieving ", .y, ":\n", .murl))
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
      query <- append(agg_quote[stringr::str_which(names(agg_quote), "results", negate = TRUE)], .query)
      agg_quote$results$t = agg_quote$results$t/1000
      
      agg_quote <- bars_transform(agg_quote["results"], timeframe)$results
      if(!is.null(agg_quote)) attr(agg_quote, "query") <- query
      return(agg_quote)
    })
  }
  bars <- purrr::modify_if(bars, is.data.frame, tsibble::as_tsibble, index = "time", regular = FALSE)
  return(bars)
}





#' @title tf_as_duration
#' @keywords Internal
#' @description convert quarter timeframe to duration
#' @inheritParams market_data
#' @importFrom lubridate duration

tf_as_duration <- function(multiplier, timeframe) {
  if (timeframe == "quarter") {
    .tf_dur <- lubridate::duration(3 * multiplier, ifelse(timeframe == "quarter", "months", timeframe))  
  } else {
    .tf_dur <- lubridate::duration(multiplier, timeframe)
  }
  return(.tf_dur)
}

#' @title bars_missing
#' @keywords Internal
#' @description A wrapper for \code{\link[tsibble]{fill_gaps}}
#' @inherit tsibble::fill_gaps

bars_missing <- function(bars, ...) {
  .dots <- rlang::dots_list(...)
  purrr::map(bars, tsibble::fill_gaps, !!!.dots)
}



# bars_complete ----
# Wed Apr 01 17:24:47 2020
#' @title Query the API as many times as necessary to fill a market_data query
#'
#' @description This function uses the output from bars_missing and fills the missing data. It's arguments are represented in the first object \code{.vn}. 
#'@details The arguments for all bars_\* functions  will be called from the environment from which the function is called if not specified explicitly. If params need to be specified explicitly, **all params must be named**. 
#' @param \code{bars} Returned by \code{bars_get}
#' @param \code{missing} Returned by \code{bars_missing}
#' @importFrom purrr map map2 pmap_dfr map2_dfr
#' @importFrom stringr str_extract str_extract_all
#' @importFrom dplyr arrange distinct `%>%` 
#' @importFrom lubridate ymd interval `%within%`
#' @importFrom rlang env_bind current_env caller_env env_get "!!!" "%|%" "%||%" is_named
#' @keywords internal
bars_complete <- function(bars, missing, bounds, timeframe, multiplier, ..., evar = get0("evar", mode = "environment", envir = rlang::caller_env())) {
  
  
  
  .vn <- evar$.vn[c("v", "unadjusted", "limit", "full")]
  .e <- list(...)
  fetch_vars(.vn, e = .e)
  if (!"tf_num" %in% ls(all.names = T)) tf_num(timeframe, multiplier)
 if (is.data.frame(bars)) bars <- list(bars)
  .newbars <- purrr::map2(missing, bars, ~{
    # ticker is passed in as the name of object
    ticker <- attr(.y, "query")$ticker %||% attr(.y, "query")[[1]]$ticker
    # If there are no missing data, return the data
    if (is.null(.x)) return(.y)
    # if days or hours
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
        .new <- bars_get(..2, timeframe  = timeframe, multiplier = multiplier)[[1]]
        if (!is.data.frame(.new)) {
          return(NULL) 
        }
        if (exists(".tf_reduce", envir = ext)) {
          # create an ordered vector of all dates in the final intended data
          .d <- sort(c(ext$.out$time, .md))
          # we need intervals that will contain times of the reduced timeframe data from which we can compute aggregates similar to how Polygon computes aggregates to create data for "unreturnable" dates
          .d_int <- purrr::map(.m_tib_missing, ~{
            # for each missing data find the interval between the closest prior date and the missing_date
            
            if (tf_num %in% 4:6) {
              # for weeks : quarter, the date returned is either the floor_date or representative of the period for which the aggregates are computed (or at least this appears to the case since using the previous period results in similar numbers to the previous bar data returned by polygon)
              .b <- bars_bounds(from = .x, to = .x, multiplier = multiplier, timeframe = timeframe)
              .out <- lubridate::interval(start = .x, end = .b$to)
            } else if (v == 2) {
              .s <- .d[which.max(which(.d < .x))]
              # sometimes there will be none greater and .d_int will be 1 shorter than .m_tib_missing - in these instances use <=
              if (length(.s) < 1) .s <- .d[which.max(which(.d <= .x))]
              .out <- lubridate::interval(start = .s, end = .x)
            } else {
              .e <- .d[which.min(which(.d > .x))]
              # sometimes there will be none less and .d_int will be 1 shorter than .m_tib_missing - in these instances use >=
              if (length(.e) < 1) .e <- .d[which.max(which(.d >= .x))]
              .out <- lubridate::interval(start = .x, end = .e)
            }
            return(.out)
          }) %>% do.call(c, .)
            # Compute the aggregates for the times in the interval
          #browser(expr = length(.d_int) != length(.m_tib_missing))
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
        ext$.out <- dplyr::bind_rows(purrr::keep(list(ext$.out, .new[.new$time %in% .md,]), ~inherits(.x, "data.frame")))
        # and make the still missing, the .md object
        if (length(.still_md) > 0) {
          .m_tib <- bars_missing(ext$.out, bounds = bounds, .tf_reduce = ext$.tf_reduce %||% F, ticker = ticker, timeframe = timeframe, multiplier = multiplier)
          # ticker must be passed here
          if (is.null(.m_tib)) break
        }
        .md <- .still_md
      } else if (identical(.still_md, .md)) {
        # otherwise, if we turned up nothing, reduce the timeframe for the next call
        ext$.tf_reduce <- TRUE
        # re-populate the .m_tib object with the remaining missing data
        .m_tib <- bars_missing(ext$.out, bounds = bounds, .tf_reduce = ext$.tf_reduce, ticker = ticker, timeframe = timeframe, multiplier = multiplier)
        # and here
        # add 1 to the count, or if it doesn't exist yes, make it 1
        .count <- 1 + (get0(".count", inherits = FALSE) %||% 0)
        if (.count == 2) break # for IPO where there might consistently be missing bars but no new bars will ever return because they don't exist
      }
      #browser(expr = get0("dbg", .GlobalEnv, ifnotfound = F) && !exists(".md"))
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
#' @importFrom dplyr `%>%`

bars_transform <- function(bars, timeframe) {
  
  if (any(grepl("^last", names(bars)[3]))) {
    # if last quote or trade
    .sym <- bars$symbol
    bars <- tibble::as_tibble(bars[[grep("^last", names(bars))]])
    bars$symbol <- .sym
    bars <- bars %>% 
      dplyr::select(symbol, dplyr::everything()) %>% 
      dplyr::mutate_at(dplyr::vars(timestamp), ~lubridate::as_datetime(. / 1e9, tz = "America/New_York", origin = lubridate::origin))
  } else {
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
  return(bars)
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
  purrr::iwalk(.headers$headers, ~{
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
#' @description  Get the correct URL for the Server Request that is sent to interact with the API. If the user is on a paper account, then the paper account URL will be returned.  See \link[httr]{parse_url} & \link[httr]{build_url} for details.
#' @param path \code{(character)} of values to append to the url path ie  `c("p1","p2")` become `url/p1/p2`
#' @param query \code{(named list)} of values to add as query parameters
#' @param ... \code{(named arguments)} to be added as query parameters
#' @keywords internal
#' @return The correct URL according to account type (live or paper) that will be sent in the API request.
get_url <- function(path, query, ..., live = FALSE, v = 2){
  
  .url <- ifelse(live, 
                "https://api.alpaca.markets",
                "https://paper-api.alpaca.markets")
  .url <- httr::parse_url(.url)
  if (!missing(path)) .url$path <- c(paste0("v",v), path)
  if (!missing(query)) .url$query <- query
  if (rlang::dots_n(...) > 0) .url$query <- rlang::dots_list(...)
  return(httr::build_url(.url))
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
#' @importFrom stringr str_detect
response_text_clean <- function(dat){
  # If empty, return empty list
  if (length(dat$content) == 0) {
    out <- list()
  } else {
    dat = httr::content(dat, as = "text", encoding = "UTF-8")
    # if it's just text, return as is, otherwise fromJSON will throw an error.
    if (!stringr::str_detect(dat, "\\{|\\[")) return(dat)
    out <- jsonlite::fromJSON(dat)
  }
  
  return(out)
}

#' @title Check if value provided is an Alpaca ID
#' @keywords internal
#' @description for use in functions that accept `*_id`
#' @param . \code{(character)}
#' @return \code{logical} indicating whether the object is an id
#' @importFrom stringr str_detect

is_id <- function(.) {
  out <- tryCatch({
    .out <- stringr::str_detect(.,"[:alnum:]{8}\\-[:alnum:]{4}\\-[:alnum:]{4}\\-[:alnum:]{4}\\-[:alnum:]{12}") && !is.na(.) && !is.null(.)
    isTRUE(.out)
  }, error = function(e) FALSE)
  return(out)
}

#' @title transform positions objects
#' 
#' @description Cleans arrays of position objects, and position objects
#' @keywords internal
#' @importFrom stringr str_extract
#' @importFrom rlang warn `%||%`
#' @importFrom purrr pwalk
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
  
  # if related orders
  .held <- !is.null(.pos$body$held_for_orders)
  if (.held) {
    # close all orders. As of 2020-05-15, all open related orders must be canceled to close positions
    # This should properly return orders
    message("Related orders prevent positions from being closed. Canceling related orders...")
    .pos <- purrr::pmap(.pos$body, ~{
      .vars <- list(...)
      if (is.null(.vars$related_orders)) return(NULL)
      .e <- new.env()
      withCallingHandlers(message = function(e) {
        if (e$message == "Order canceled successfully\n") 
          rlang::warn(message = paste0("Canceled order ", .vars$related_orders," for ", .vars$symbol)) 
        else
          rlang::warn(message = paste0("Could not cancel order ", .vars$related_orders," for ", .vars$symbol, ". Position for ", .vars$symbol, " remains open."))
      }, {
        .out <- order_submit(.vars$related_orders, action = "cancel")
        .out <- positions(.vars$symbol, a = "c")
      })
      return(.out)
    })
    # order_transform should properly transform orders
    
  }
  
  if(any(grepl(pattern = "^4", x = .code))) {
    rlang::warn(paste("Position was not",ifelse(grepl("GET", .method, ignore.case = TRUE), "found.", "modified."),"\n Message:", .message))
    return(.pos)
  } else if (.sym != "positions" && .sym %in% .pos$symbol && grepl("DELETE", .method, ignore.case = TRUE)) {
    message(paste0(.sym, " closed successfully."))
  } else if (grepl("DELETE", .method, ignore.case = TRUE) && any(grepl("^2", .pos$body$code %||% .pos$status))) {
    message(paste0("All positions closed successfully.\nClosed Position(s): ", paste0(.pos$body$sym[grepl("^2", .pos$body$code %||% .pos$status)], collapse = ", ")))
  }
  
  #Check if any pos exist before attempting to return
  if(length(.pos) == 0) {
    message("No positions are open at this time.")
    out <- .pos
  } else if(length(.pos) > 1 && !(grepl("DELETE", .method, ignore.case = TRUE) && .sym == "positions")) {
    out <- order_transform(pos)
  } else if (.sym == "positions") {
    # if close_all
    if (.held) {
      out <- bind_rows(.pos)
    } else {
      out <- order_transform(.pos$body)
      attr(out, "info") <- .pos[1:2]
    }
    
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
  
  if (class(resp) != "response") {
    .code <- resp$code
    .message <- resp$message
  } else if (class(resp) == "response") {
    .method <- resp$request$method
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
        dplyr::mutate_at(dplyr::vars("transaction_time"), ~lubridate::as_datetime(., tz = Sys.timezone())) %>% 
        dplyr::mutate_at(dplyr::vars("price", "qty", "leaves_qty", "cum_qty"), as.numeric)
    })
    
  }
  return(out)
}

#' @title transform portfolio history
#' @description Transforms a [PortfolioHistory](https://alpaca.markets/docs/api-documentation/api-v2/portfolio-history/#portfoliohistory-entity) response object returned from `account_portfolio`.
#' @param resp `(response)` The response object
#' @inherit account_portfolio return
#' @keywords internal
#' @importFrom tibble as_tibble
#' @importFrom dplyr `%>%`
#' @importFrom stringr str_extract
#' @importFrom rlang warn
#' @importFrom dplyr mutate_at vars
#' @importFrom lubridate as_datetime origin

port_transform <- function(resp) {
  
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
    rlang::warn(paste("History not retrieved.\n Message:", .message))
    return(.resp)
  }
  
  #Check if any pos exist before attempting to return
  if(length(.resp) == 0) {
    message("No History available with specified criteria.")
    out <- .resp
  } else if(length(.resp) > 1) {
    .out <- tibble::as_tibble(.resp[1:4])
    # coerce to numeric in positions objects
    if (nrow(.out) > 0) {
      suppressMessages({
        out <- .out %>%
          dplyr::mutate_at(dplyr::vars("timestamp"), ~lubridate::as_datetime(., origin = lubridate::origin, tz = Sys.timezone())) %>% 
          dplyr::mutate_at(dplyr::vars("equity", "profit_loss", "profit_loss_pct"), as.numeric)
      })
    } else {
      out <- .out
    }
    
    attr(out, "info") <- .resp[5:6]
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
#' @description Replaces character quantities with numeric and character dates with POSIXct
#' @param .o An order object
#' @keywords internal
#' @importFrom dplyr mutate_at mutate_if vars ends_with 
#' @importFrom lubridate ymd_hms
o_transform <- function(.o) {
  .o <- dplyr::mutate_at(.o, dplyr::vars(dplyr::ends_with("at")),list(~lubridate::ymd_hms(., tz = Sys.timezone())))
  out <- dplyr::mutate_if(.o, ~is.character(.) && !is.na(as.numeric(toNum(.))), list(toNum))
  return(out)
}

#' @title Transform order responses
#' 
#' @description Parses order type responses and replaces plain text quantities and dates with respective R objects
#' @param orders A dataframe returned from any orders_* endpoint
#' @return \code{(tibble)}  with respective R compliant objects (numeric, POSIXct/Datetime, character)
#' @keywords internal
#' @importFrom tibble as_tibble
#' @importFrom rlang `%||%` warn
#' @importFrom purrr map

order_transform <- function(o) {
  if (class(o) == "response") {
    if (length(o$content) == 0 && grepl("^2", o$status_code)) {
      message(paste0("Order canceled successfully"))
    } else if (grepl("^5", o$status_code)) {
      rlang::warn("Failed to cancel order.")
    }
    .method <- o$request$method
    .code <- o$status_code
    .o <- response_text_clean(o)
    if (!inherits(.o, "character")) {
      .message <- .o$message
    } else {
      .message <- .o
    }
  } else if (class(o) != "response") {
    .method <- "DELETE"
    .code <- 200
    .o <- o
  }
  
  if (grepl("^4", .code)) {
    rlang::warn(paste0("Code: ",.code,",\nMessage:", .message))
    return(.o)
  }
  
  if ((is.list(.o) && length(.o) > 0) || ("body" %in% names(.o) && grepl("DELETE", .method, ignore.case = TRUE))) {
    if (grepl("DELETE", .method, ignore.case = TRUE) && "body" %in% names(.o)) {.o <- .o$body;.q <- .o[1:2]}
    .o <- tibble::as_tibble(purrr::map(.o, rlang::`%||%`, NA))
    suppressMessages({
      suppressWarnings({
        if (!is.null(.o$legs) && !is.na(.o$legs)) {
          if (inherits(.o$legs, "list")) {
            .o$legs <- purrr::map(.o$legs, ~{
              if (!is.null(.x)) {
                .out <- o_transform(.x)
              } else {
                .out <- .x
              }
              return(.out)
            })
          } else {
            .o$legs <- o_transform(.o$legs)
          }
        }
        out <- o_transform(.o) 
      })})
    
  } else if (length(.o) == 0 && grepl("GET", .method, ignore.case = TRUE)) {
    message(paste("No orders for the selected query/filter criteria.","\nCheck `ticker_id` or set status = 'all' to see all orders."))
    out <- .o
  } else if (grepl("DELETE", .method, ignore.case = TRUE)) {
    # case when deleting single order
    out <- .o
  }
  if (exists(".q", inherits = FALSE)) attr(out, "query") <- .q
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
  
  
  # add the arguments to the environment ----
  # Thu Apr 30 17:29:18 2020
  .o <- try(list2env(as.list(penv), environment()))
  if (class(.o) == "try-error") {
    .vn <-
      list(
        ticker_id = "character",
        action = "character",
        type = "character",
        qty = c("numeric", "integer"),
        side = "character",
        time_in_force = "logical",
        limit = c("numeric", "integer"),
        stop = c("numeric", "integer"),
        extended_hours = "logical",
        client_order_id = "character",
        order_class = "character",
        take_profit = "list",
        stop_loss = "list",
        trail_price = c("numeric", "integer"),
        trail_percent = c("numeric", "integer")
      )
    .e <- list(...)
    fetch_vars(.vn, e = .e)
  }
  
  # ticker_id ----
  # Fri May 01 11:15:39 2020
  # Check if ticker is id
  .is_id <- is_id(ticker_id)
  if (isTRUE(.is_id)) {
    # if vector length 2 and duplicated (complex orders), remove the dupes
    if (any(duplicated(ticker_id))) ticker_id <- ticker_id[!duplicated(ticker_id)]
    if (action == "s") {
      #if ticker_id is ID, action is submit and qty is NULL, populate qty from previous order
      .oo <- orders(ticker_id)
      if (.oo$side == "buy") {
        side <- "sell";message("`side` set to 'sell'")
        if (is.null(qty)) qty <- .oo$qty;message(paste0("`qty` set to ",qty))
        ticker_id <- .oo$symbol;message(paste0("`ticker_id` set to ",.oo$symbol))
        if (isTRUE(client_order_id)) client_order_id <- .oo$id;message(paste0("`client_order_id` set to ", .oo$id))
      }
    } 
  } else if (!isTRUE(.is_id)) {
    #Convert ticker argument to upper if action is submit
    ticker_id <- toupper(ticker_id)
  }
  #  smart detect order_class ----
  # Fri May 15 13:48:32 2020
  if (!is.null(order_class)) {
    .oc <- tolower(substr(order_class, 0, 1))
    if (.oc == "b") {
      order_class <- "bracket"
    } else if (!order_class %in% c("oto", "oco")) {
      rlang::abort(paste0(order_class, "is invalid `order_class`. See ?order_submit for help."))
    }
  }
  
  # if side is partialled or missing ----
  # Thu Apr 30 20:32:52 2020
  if (action == "s") {
    
    # set type if partialled and order_class is NULL  ----
    # Thu Apr 30 20:20:16 2020
    if (!is.null(type) && is.null(order_class)){
      type <- tolower(type)
      if (grepl("^s", type) && grepl("(?<!i)l", type, perl = TRUE)) {
        type <- "stop_limit"
      } else if (grepl("^t", type) && grepl("s", type)) {
        type <- "trailing_stop"
      } else if (substr(type,1,1) == "s") {
        type <- "stop"
      } else if (substr(type,1,1) == "l") {
        type <- "limit"
      } else if (substr(type,1,1) == "m") {
        type <- "market"
      }
    }
    
    if (!is.null(side)) {
      side <- try(c(b = "buy", s = "sell")[tolower(substr(side, 0, 1))])
      if (class(side) == "try-error") rlang::abort("Invalid value for `side`")
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
    
    # Short sell/stop buy warning
    if (side == "sell") {
      try({
        .pos <- positions(ticker_id)
        .ss <- ticker_id[!ticker_id %in% .pos$symbol]
      }, silent = TRUE)
      
      if (!rlang::is_empty(.ss)) {
        warning("No positions exist for ",paste0(.ss, collapse = ", "),". This order will be a short sell.", immediate. = TRUE)
      }
    } else if (side == "buy" && (!is.null(stop))) {
      if (stop_price == "stop_price") {
        .warn_msg <- paste0("reaches ", stop)
      } else if (stop_price == "trail_price") {
        .warn_msg <- paste0("decreases by ", stop)
      } else if (stop_price == "trail_percent") {
        .warn_msg <- paste0("decreases by ", stop, " percent")
      }
      warning("This stop buy order will execute when the price ", .warn_msg , immediate. = TRUE)
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
    if ((order_class %||% "none") == "bracket" && (type %||% "none") != "market") {
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
      # throw errors if not detected or arguments don't match
      if (type == "limit" && is.null(limit)){ 
        rlang::abort(paste0("Please set limit price."))
      } else if (type == "stop" && is.null(stop)) {
        rlang::abort(paste0("Please set value for `stop` argument when `type = ", type,"`."))
      } else if ((is.null(stop) || is.null(limit)) && type == "stop_limit") {
        rlang::abort(paste0(paste0(unlist(purrr::imap(list(stop = stop, limit = limit), ~{
          if (is.null(.x)) .y else NULL
        })), collapse = ", "), " must be set."))
      } else if (is.null(stop) && type == "trailing_stop") {
        rlang::abort(paste0("Please set `trail_price` or `trail_percent` when `type = 'trailing_stop'`"))
        if (stop_price == "trail_percent" && stop < 1) {
          rlang::abort("`trail_percent` must be an integer greater than one")
        } 
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
  } else if (action == "c") {
    if (is.null(ticker_id)) rlang::abort("`ticker_id` is NULL, the order may not have been placed successfully?")
  } 
  out <- list(
    ticker_id = ticker_id,
    action = action,
    type = type,
    qty = qty,
    side = side,
    time_in_force = time_in_force,
    limit = limit,
    stop = stop,
    extended_hours = extended_hours,
    client_order_id = client_order_id,
    order_class = order_class,
    take_profit = take_profit,
    stop_loss = stop_loss
  )
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
#' @importFrom dplyr `%>%`
#' @importFrom rlang abort `%||%` 
#' @importFrom tibble as_tibble tibble
#' @importFrom purrr map

wl_transform <- function(wl, action, wl_info = NULL) {
  
  
  if (class(wl) == "response") {
    if (length(wl$content) == 0 && grepl("^2", wl$status_code)) {
      message(paste0("Watchlist deleted successfully"))
      return(tibble::tibble())
    }
    .method <- wl$request$method
    .code <- wl$status_code
    .wl <- response_text_clean(wl)
    if ("message" %in% names(.wl))
      .message <- .wl$message
    else {
      .message <- .wl
    }
    
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
  if (class(wl) == "response" && length(.wl) > 0) {
    wl_info <- suppressMessages({
      suppressWarnings({
        dplyr::mutate_at(tibble::as_tibble(purrr::map(.wl[1:5], rlang::`%||%`, NA)), dplyr::vars(dplyr::ends_with("at")),list(~lubridate::ymd_hms(., tz = Sys.timezone()))) %>% 
          dplyr::select(name, updated_at, dplyr::everything())
      })
    })
  } else if (class(wl) == "response") {
    message(paste0("No watchlists exist."))
    out <- .wl
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


# .mode ----
# Sun May 03 08:54:39 2020
#'@title get the mode
#'@keywords internal
.mode <- function(.) {
  .u <- unique(.)
  tab <- tabulate(match(., .u))
  .u[tab == max(tab)]
}




.ints <- setNames(c("year", "quarter", "month", "week", "day", "hour", "minute", "second"), c("year", "quarter", "month", "week", "day", "hour", "minute", "second"))
.ints <- purrr::map(.ints, getFromNamespace, ns = "lubridate")

#' @title irregular_interval
#' @description Finds the interval of an irregular timeseries
#' @param x \code{(POSIXct/Date/Datetime)} input for which interval will be computed
#' @return The appropriate interval via \code{\link[tsibble]{new_interval}}
#' @importFrom purrr map_lgl map
#' @importFrom rlang inform exec
#' @importFrom tsibble new_interval

irregular_interval <- function(x) {
  
  dfx <- purrr::map(.ints, ~{
    .out <- sort(.mode(round(abs(diff(rlang::exec(.x, x))), digits = 6)))
    .out[.out > 0][1]
  })
  
  dfx <- dfx[min(which(purrr::map_lgl(dfx, ~any(.x > 0))))]
  
  
  
  if (is.null(dfx[[1]])) {
    rlang::inform(paste0("Unable to determine interval."))
    tsibble:::irregular()
  } else {
    rlang::inform(paste0("Using ",dfx," ", names(dfx), " as interval."))
    tsibble::new_interval(
      !!!dfx
    )
  }
  
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
  
  
  
  .code <- resp$status_code
  .resp <- response_text_clean(resp)
  if ("error" %in% names(.resp)) {
    .message <- .resp$error
  } else {
    .message <- .resp
  }
  
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
    .o <- list(.tbl = .resp, .vars = c("updated", "listdate"), .f = try_date)
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
    if (length(.resp$tickers) < 1) {
      # when market is closed
      rlang::warn("Snapshot: All Tickers returns no data when market is closed.")
      .o <- list(.tbl = .resp$tickers, .q = .resp[1:2])
    } else {
      .o <- list(.tbl = .resp$tickers[1:9], .vars = c("lastQuote.t", "lastTrade.t", "updated"), .f = rlang::expr(~lubridate::as_datetime(. / 1e9, tz = "America/New_York", origin = lubridate::origin)), .q = .resp[1:2])
    }
  } else if (ep == "st") {
    .o <- list(.tbl = .resp$ticker[1:9], .vars = c("lastQuote.t", "lastTrade.t", "updated"), .f = rlang::expr(~lubridate::as_datetime(. / 1e9, tz = "America/New_York", origin = lubridate::origin)), .q = .resp[1])
  } else if (ep == "sg") {
    if (length(.resp$tickers) < 1) {
      # when market is closed
      rlang::warn("Snapshot: Gainers/Losers returns no data when market is closed.")
      .o <- list(.tbl = .resp$tickers, .q = .resp[1])
    } else {
      .o <- list(.tbl = .resp$tickers[1:9], .vars = c("lastQuote.t", "lastTrade.t", "updated"), .f = rlang::expr(~lubridate::as_datetime(. / 1e9, tz = "America/New_York", origin = lubridate::origin)), .q = .resp[1])
    }
    
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
      .t <- unlist(.o$.tbl[1:5], recursive = FALSE)
      if (ep == "st") .t <- purrr::map_if(.t, ~length(.x) > 1 || is.null(.x), list)
      .o$.tbl <- dplyr::bind_cols(tibble::as_tibble(.t), .o$.tbl[6:9]) 
    }
  .o$.tbl <- purrr::modify_depth(.o$.tbl, .depth = -1, .f = ~{
    .out <- rlang::`%||%`(x = .x, y = NA)
    if (length(.x) < 1) .out <- NA
    return(.out)
    }, .ragged = T)
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
#' @importFrom utils object.size memory.size
ws_msg <- function(out, msg, .o = NULL, toConsole = T) {
  # Update the last message
  if (exists("lastmessage", out$env)) rm(list = "lastmessage", envir = out$env)
  assign("lastmessage", msg, out$env)
  if (out$env$toConsole) cat("Message: ", msg, "\n")
  if (exists("msgs", out$env)) {
    wsmsg <- get("msgs", out$env)
    wsmsg <- dplyr::bind_rows(wsmsg, tibble::tibble(Timestamp = lubridate::now(tz = Sys.timezone()), Message = stringr::str_remove(msg, "^\\d{4}\\-\\d{2}\\-\\d{2}\\s\\d{2}\\:\\d{2}\\:\\d{2}\\,\\s")))
    # if the object has reached 1/3rd of the allowable memory allocation
    if (utils::object.size(wsmsg) / (utils::memory.size(NA) * 1048567) > .33) {
      # half it's size by removing the first half
      wsmsg <- wsmsg[- c(1:(nrow(wsmsg) %/% 2)),]
    } 
    assign("msgs", wsmsg, out$env)
  } else {
    assign("msgs", tibble::tibble(Timestamp = lubridate::now(tz = Sys.timezone()), Message = msg), out$env)
  }
  if (!is.null(.o)) {
    if (.o$ev %in% c("T", "Q", "A", "AM")) {
      if (!exists("bars", envir = out$env, inherits = FALSE)) {
        bars <- list()
        bars[[paste0(.o$ev,".",.o$sym)]] <- .o
        assign("bars", bars, out$env)
      } else {
        .bars <- get0("bars", out$env, inherits = FALSE)
        .nm <- paste0(.o$ev,".",.o$sym)
        .bars[[.nm]] <- dplyr::bind_rows(.bars[[.nm]], .o)
        if (utils::object.size(.bars) / (utils::memory.size(NA) * 1048567) > .33) {
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
#' @importFrom rlang caller_env
#' @importFrom purrr map
ws_log <- function(out, ..., .o = NULL, msg = NULL, penv = rlang::caller_env()) {
  
  # add the arguments to the environment ----
  # Thu Apr 30 17:29:18 2020
  .e <- try(list2env(list(penv), environment()), silent = TRUE)
  .vn <- list(.o = "data.frame", .log = "logical", out = "list", log_bars = "logical", log_msgs = "logical", log_path = "logical", logfile = "character", api = "character")
  if (!exists(msg, inherits = FALSE)) .vn$.msg <- "character"
  if (!all(.vn %in% ls(all.names = TRUE))){
    .e <- list(...)
    fetch_vars(.vn, e = .e, penv = penv)
    if (!exists("api", inherits = FALSE)) api <- attr(out, "api")
  }
  if (!.log) return(NULL) # stop if no logging
  # If listening to a subscription channel & logging bars
  if (api == "p" && !is.null(.o)) {
    if (.o$ev %in% c("T", "Q", "A", "AM") && out$env$log_bars) {
      # Create the name of the CSV log for Polygon channels
      .log_ev <- paste0(log_path, paste0(.o$ev,".",.o$sym,".csv"))
      write(paste0(.o, collapse = ", "), file = .log_ev, append = TRUE)
    }
  }
  if (out$env$log_msgs) write(ifelse(exists(msg, inherits = FALSE), msg, .msg), file = out$env$logfile, append = TRUE)
}
