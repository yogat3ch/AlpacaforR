
# Internal functions for market_data ----
# Sat Mar 28 09:40:23 2020

#  Re-exports ----
# Sun Sep 20 11:15:14 2020
#' @keywords internal



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


fetch_vars <- function(.vn,
                       ...,
                       evar = get0(
                         "evar",
                         mode = "environment",
                         envir = rlang::caller_env(),
                         ifnotfound = new.env()
                       ),
                       cenv = rlang::caller_env()
) {
  e <- rlang::dots_list(...)
  if (!rlang::is_empty(e)) rlang::env_bind(cenv, !!!e)
  .vn <- .vn[!names(.vn) %in% c(names(e), ls(cenv))]
  if (rlang::is_empty(.vn)) return(NULL)
  purrr::iwalk(.vn, ~{
    .v <- get0(.y, envir = evar, inherits = FALSE)
    if (inherits(.v, .x)) rlang::env_bind(cenv, !!!rlang::list2(!!.y := .v))
  })
}




#' @title tf_as_duration
#' @keywords Internal
#' @description convert quarter timeframe to duration
#' @inheritParams market_data

tf_as_duration <- function(multiplier, timeframe) {
  if (timeframe == "quarter") {
    .tf_dur <- lubridate::duration(3 * multiplier, ifelse(timeframe == "quarter", "months", timeframe))  
  } else {
    .tf_dur <- lubridate::duration(multiplier, timeframe)
  }
  return(.tf_dur)
}


# get_headers Get Headers for Server Request function  ----
# Sun Mar 29 16:05:32 2020  
#' @title return headers for API calls
#'
#' @keywords internal
#' @return The correct headers that will be sent in the API request.
#' @param live TRUE / FALSE if you are connecting to a live account. Default to NULL, so it will use the key variables set by the user for their respective paper account. Set live = TRUE to find your live key credentials.
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
      #TODO if package name change, change this value
      rlang::abort(message = paste0(.ev," is unset. Please set your API credentials as environmental variables. See vignette('installation', 'AlpacaforR') for details.")) 
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
get_url <- function(path, query, ..., live = FALSE, v = 2, poly = FALSE){
  if (poly) {
    .url <- "https://api.polygon.io" 
  } else {
    .url <- ifelse(live, 
                   "https://api.alpaca.markets",
                   "https://paper-api.alpaca.markets")
  }
  
  .url <- httr::parse_url(.url)
  if (!missing(path)) {
    if (!ifelse(is.character(path), grepl("v\\d", path), "v" %in% names(path))) {
      .url$path <- rlang::list2(v = paste0("v",v), !!!path)
    } else {
      .url$path <- path
    }
  } 
  if (!missing(query)) .url$query <- query
  if (rlang::dots_n(...) > 0) .url$query <- rlang::dots_list(...)
  return(httr::build_url(.url))
}

# Internals:  Sun Jan 12 10:20:31 2020 ----
# response_text_clean Clean data from Server Response function  ----
# Sun Mar 29 16:02:00 2020
#' @title Clean API responses
#' 
#' @description Clean the response text (usually in unreadable json) and convert to a readable format using \link[jsonlite]{fromJSON}. 
#' @param x The response from our server GET request 
#' @keywords internal
#' @return The response in a readable format as a list.
response_text_clean <- function(x){
  if (inherits(x, "response")) {
    query <- list(
      status_code = x$status_code,
      url = gsub(
        "(?<=apiKey\\=)[[:alnum:]]+$",
        "[REDACTED]",
        x$url,
        perl = TRUE
      ), # redact the apikey
      ts = lubridate::with_tz(
        lubridate::parse_date_time(x$headers$date, "a, d b Y T"),
        tzone = "America/New_York"
      )
    )
  } 
  
  if (length(x$content) == 0) {
    # If empty, return empty list
    out <- list()
  } else {
    x = httr::content(x, as = "text", encoding = "UTF-8")
    # if it's just text, return as is, otherwise fromJSON will throw an error.
    if (!stringr::str_detect(x, "\\{|\\[")) return(x)
    out <- jsonlite::fromJSON(x)
  }
  
  if (grepl("aggs", query$url)) {
    query <- append(query, out[names(out) != "results"])
    out <- out$results
  }
  attr(out, "query") <- query
  return(out)
}


#' @title Check if value provided is an Alpaca ID
#' @keywords internal
#' @description for use in functions that accept `*_id`
#' @param . \code{(character)}
#' @return \code{logical} indicating whether the object is an id

is_id <- function(.) {
  out <- tryCatch({
    .out <- grepl("[[:alnum:]]{8}\\-[[:alnum:]]{4}\\-[[:alnum:]]{4}\\-[[:alnum:]]{4}\\-[[:alnum:]]{12}", .) && !is.na(.) && !is.null(.)
    isTRUE(.out)
  }, error = function(e) FALSE)
  return(out)
}


# Format orders to workable and readable format before returning
#' @title Convert money strings to numeric
#' 
#' @description remove $ from cash quantity strings and convert to numeric
#' @keywords internal 
toNum <- function(x){
  as.numeric(stringr::str_replace_all(x, "\\$|\\,", ""))
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



