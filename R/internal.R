
# Internal functions for market_data ----
# Sat Mar 28 09:40:23 2020

#  Re-exports ----
# Sun Sep 20 11:15:14 2020
#' @inherit rlang::`%||%`
#' @importFrom rlang `%||%`
#' @export
`%||%` <- rlang::`%||%`

#' @inherit rlang::`%|%`
#' @importFrom rlang `%|%`
#' @export
`%|%` <- rlang::`%|%`

#' @inherit rlang::`%@%`
#' @importFrom rlang `%@%`
#' @export
`%@%` <- rlang::`%@%`

glubort <- function(..., .sep = "", .envir = parent.frame()) {
  rlang::abort(glue(..., .sep = .sep, .envir = .envir))
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
  force(cenv)
  force(evar)
  e <- rlang::dots_list(...)
  # only bind named
  e <- e[nzchar(names(e))]
  if (!rlang::is_empty(e)) rlang::env_bind(cenv, !!!e)
  # Which variables in cenv
  .vn <- purrr::flatten(list(purrr::compact(.vn[names(.vn) %in% ls(cenv)]) %>%
                               # if the variables in cenv are not of appropriate type, then keep them in the search.
                               `[`(!purrr::imap_lgl(
                                 ., ~ inherits(get0(.y, cenv), .x)
                               )), 
                             # if the variables are absent in cenv then search for them
                             .vn[!names(.vn) %in% ls(cenv)]))
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
get_headers <- function(live = get_live()){
  httr::add_headers(
    'APCA-API-KEY-ID' = get_key(live),
    'APCA-API-SECRET-KEY' = get_secret(live)
  )
}



# get_url for Server Request ----
# Sun Mar 29 16:02:51 2020
#' @title Return the Alpaca URL
#' 
#' @description  Get the correct URL for the Server Request that is sent to interact with the API. If the user is on a paper account, then the paper account URL will be returned.  See \link[httr]{parse_url} & \link[httr]{build_url} for details.
#' @param path \code{(character)} of values to append to the url path ie  `c("p1","p2")` become `url/p1/p2`
#' @param query \code{(named list)} of values to add as query parameters
#' @param ... \code{(named arguments)} to be added as query parameters
#' @param live \code{(logical)} `TRUE` to use the live account, `FALSE` for paper.
#' @param v \code{(numeric)} The API version used for Alpaca
#' @param data \code{(logical)} Whether to use the \href{https://alpaca.markets/docs/api-documentation/api-v2/market-data/}{Alpaca Data API} or the standard API.
#' @param api \code{(character)} The API to call: `api` or `ws` (for websockets).
#' @keywords internal
#' @return The formatted URL.
get_url <-
  function(path = NULL,
           query,
           ...,
           live = get_live(),
           v = 2,
           data = FALSE,
           poly = FALSE,
           api = c("api", "ws")[1]
  ) {
  
  if (poly) {
    .url <- list(
      scheme = ifelse(api == "ws", "wss", "https"),
      hostname = paste0(c(
        purrr::when(
          api,
          . == "api" ~ "api",
          . == "ws" ~ "socket",
          ~ stop("No url match for parameters", call. = FALSE)
        ),
        "polygon.io"
      ), collapse = ".")
    )
  } else {
    .url <- list(
      scheme = ifelse(api == "ws", "wss", "https"),
      hostname = paste0(c(
        purrr::when(
          api,
          . == "api" && isTRUE(data) ~ "data",
          . == "ws" && isTRUE(data) && v == 2 ~ "stream.data",
          . %in% c("ws", "api") && isTRUE(live) && v == 2 ~ "api",
          . %in% c("ws", "api") && isFALSE(live) && v == 2 ~ "paper-api",
          . == "ws" && v == 1 ~ "data",
          ~ stop("No url match for parameters", call. = FALSE)
        ),
        "alpaca.markets"
      ), collapse = ".")
    )
  }
  # create url output object
  .url <- structure(.url, class = c("list", "url"))
  # Path ----
  # Tue Mar 16 14:58:01 2021 Path
  # Don't add version to path if:
  # - Using polygon websocket or Alpaca v1 websocket
  # and only if the input url doesn't already have vX
  
  .url$path <- purrr::compact(list(purrr::when(isTRUE(grepl("v\\d", path)),
                                !. && api == "ws" && isFALSE(data) ~ NULL,
                                !. ~ paste0("v", v)
  ),
  purrr::when(poly,
              isTRUE(.) && api == "ws" ~ "stocks",
              isFALSE(.) && v == 1 && api == "ws" ~ "stream",
              ~ NULL)))
  
  if (!is.null(path))
    .url$path <- append(.url$path, path)

  # Add dots
  if (rlang::dots_n(...) > 0) 
    .url$query <- rlang::list2(
      if (!missing(query)) query,
      !!!rlang::dots_list(...))
  # Add polygon apiKey to end of query for Polygon EPs
  if (!missing(query)) 
    .url$query <- purrr::when(poly, 
                              isTRUE(.) ~ append(query,
                                                 list(apiKey = get_key(api = "p"))),
                               ~ query
                              )
  return(utils::URLdecode(httr::build_url(.url)))
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
    if (inherits(x, "response"))
      x = httr::content(x, as = "text", encoding = "UTF-8")
    # if it's just text, return as is, otherwise fromJSON will throw an error.
    
    out <- tryCatch(jsonlite::fromJSON(x), error = rlang::as_function(~{x})) 
  }
  
  
  
  if (grepl("aggs|(?:v2/stocks)", query$url)) {
    query <- append(query, out[!names(out) %in% c("results", "bars")])
    out <- suppressWarnings(out$results %||% out$bars %||% out)
  }
  
  attr(out, "query") <- query
  check_response(out)
  
  return(out)
}

check_response <- function(resp, query = NULL) {
  query <- query %||% get_query(resp)
  if (rlang::is_empty(query) && "error" %in% names(resp)) {
    rlang::abort(paste("code:", resp$status, "\nmessage:", resp$error))
  } else if(grepl(pattern = "^4", x = query$status_code)) {
    glubort("code: {query$status_code}\nmessage: {resp$message}")
  }
  
  .warn <- try({NROW(resp) > 0})
  if (is_error(.warn)) 
    rlang::warn(paste0(query$symbol, " returned no data."))

}

#' @title Check if value provided is an Alpaca ID
#' @keywords internal
#' @description for use in functions that accept `*_id`
#' @param . \code{(character)}
#' @return \code{logical} indicating whether the object is an id

is_id <- function(.) {
  out <- tryCatch({
    .out <- grepl("[[:alnum:]]{8}\\-[[:alnum:]]{4}\\-[[:alnum:]]{4}\\-[[:alnum:]]{4}\\-[[:alnum:]]{12}", .) && !is.na(.) && !is.null(.) && is.character(.)
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



