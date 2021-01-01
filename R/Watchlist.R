# watchlist ----
# Wed Apr 22 20:37:02 2020
#' @family Watchlist
#' @title Access all Watchlist Endpoints
#'
#' @description Access all [Watchlist](https://alpaca.markets/docs/api-documentation/api-v2/watchlist) endpoints. See link for details. \lifecycle{stable}
#' @param watchlist_id `(character)` **Watchlist Name or ID**. If `NULL` **Default**, the array of all saved `Watchlist` will be returned. All other actions to be performed on the provided `Watchlist` name or ID must be explicitly specified via `action`. See below for description of the behavior of this argument based on the value supplied to `action`.
#' @param symbols `(character)` Vector of symbols. Must be specified with a `watchlist_id`. If the `action` is:
#' \itemize{
#'   \item{\code{"get"\"g" **Default**}}{ `action` assumed to be add and `symbols` will be added.}
#'   \item{\code{"create"}}{ A `Watchlist` cakked `watchlist_id` will be created with `symbols`. If symbols are missing the new `Watchlist` will be empty.}
#'  \item{\code{"add"}}{ The `symbols` will be added to the specified `watchlist_id`. If this argument is specified with `watchlist_id` and `new_name` the `Watchlist` will be renamed to `new_name` and the `symbols` will be added. Set `action = "update"` explicitly to replace existing symbols with `symbols` when renaming.}
#'  \item{\code{"update"}}{ The `symbols` will replace those in `watchlist_id`.}
#'  \item{\code{"delete"}}{ If this argument is specified with `watchlist_id`, the `symbols` will be deleted from the `Watchlist`. If `symbols` is missing the `Watchlist` will be deleted.}
#' }
#' @param action `(character)` The action to take. See other parameters for detailed descriptions of how this argument determines the action when combinations of parameters are specified. This argument can be abbreviated with a single non-case-sensitive letter. One of:
#' \itemize{
#'  \item{\code{"get"}}{ **Default** If no `watchlist_id` is specified, an array of all `Watchlist` will be returned, otherwise a the watchlist specified by `watchlist_id` will be returned. If `symbols` are specified, the symbols will be added to the `watchlist_id`.}
#'   \item{\code{"create"/"c"}}{ A `Watchlist` will be created named according to `watchlist_id` with any `symbols` provided.}
#'  \item{\code{"add"/"a"}}{ the `watchlist_id` will have `symbols` added. If `new_name` is also specified, `symbols` will be added (unless `action = "update"`)}
#'  \item{\code{"update"/"u"}}{ `watchlist_id` is *required* and will have it's symbols replaced by those provided to `symbols`. If `new_name` and `symbols` are provided the `Watchlist` will be renamed and the `symbols` will *replace* the existing ones. If the `symbols` are meant to be added, do not set action, or set as `"add"`.}
#'  \item{\code{"delete"/"d"}}{ If `action = "delete"` with `watchlist_id`, the `symbols` will be deleted from the `Watchlist`. If `symbols` is missing the `Watchlist` with id/name `watchlist_id` will be deleted.}
#' }
#' @param new_name `(character)` arbitrary name string, up to 64 characters. If present, then `action = "update"` is assumed and the `Watchlist` specified by `watchlist_id` will be renamed to `new_name` preserving existing symbols, unless `symbols` is present, in which case they will replace the existing. 
#' @inheritParams account
#' @return `Watchlist` `(tibble)` If `watchlist()` is called with no arguments, an `tibble` of [Watchlist](https://alpaca.markets/docs/api-documentation/api-v2/watchlist/#watchlist-entity) Object.  For all others `action`s, a `tibble` of [Asset](https://alpaca.markets/docs/api-documentation/api-v2/assets/#asset-entity) Objects with the `Watchlist` stored as an attribute retrieved with `attr(Asset, "info")` and query metadata stored as `attr(Asset, "query")`. A `Watchlist` object has the following information:
#' \itemize{
#'  \item{\code{name}}{`(character)` The name of the watchlist.}
#'  \item{\code{updated_at}}{`(POSIXct)` The timestamp of the last update.}
#'  \item{\code{id}}{`(character)` Watchlist ID}
#'  \item{\code{account_id}}{`(character)` The account ID associated with the watchlist}
#'  \item{\code{created_at}}{`(POSIXct)` The timestamp of the watchlist creation.}
#' }
#' @inherit assets return
#' @examples 
#' # get the existing watchlists
#' watchlist()
#' # create a watchlist named test with Microsoft
#' (wl <- watchlist("test", symbols = "AAPL", action = "c"))
#' # See it in the list of watchlists
#' watchlist()
#' # Get it
#' (test <- watchlist("test"))
#' all.equal(test,wl, check.attributes = F)
#' # Get that Watchlist's info
#' attr(wl, "info")
#' # Add "FB", "AMZN", "NFLX", "GOOG" and update the watchlist name to "FAANG"
#' (wl <- watchlist("test", new_name = "FAANG", symbols = c("FB", "AMZN", "NFLX", "GOOG")))
#' # Add individual stocks by specifying symbols (action assumed to be add when symbols are present)
#' (wl <- watchlist("FAANG", symbol = "GOOGL"))
#' # Delete individual items (or multiple)
#' (wl <- watchlist("FAANG", action = "d", symbols = "GOOGL"))
#' # Rename it appropriately and replace the symbols to match the name by specifying action = "update" explicitly.
#' (wl <- watchlist("FAANG", new_name = "FANG", symbols = c("FB", "AAPL", "NFLX", "GOOG"), action = "u"))
#' # Delete it using a partial argument (may cause a warning)
#' watchlist("FANG", a = "d")
#' @export
watchlist <-
  function(watchlist_id = NULL,
           symbols = NULL,
           action = "get",
           new_name,
           live = as.logical(Sys.getenv("APCA-LIVE", FALSE))) {
  
  
  action <- tolower(substr(action,0,1))
  # Set URL & Headers
  .url <- purrr::when(
    watchlist_id,
    is_id(.) ~ list(path = c("watchlists", watchlist_id)),
    is.character(.) && action != "c" ~ list(path = "watchlists:by_name", query = list(name = watchlist_id)),
    is.null(.) || action == "c" ~ list(path = "watchlists")
  ) %>%
    {
      rlang::exec(get_url,!!!., live = live)
    }
  headers = get_headers(live)
  # logical indicating if new_name is present
  .nn <- !missing(new_name)
  
  ## non-destructive action assumption
  .action <- purrr::when(.nn, 
  # if new_name is present, and action != "update", assume action = "add" (non-destructive rename)
              . && action != "u" ~ "a",
              ~ action)
  # if assumed action = add combine the specified symbols,
  symbols <- purrr::when(.action, 
                         . == "a" ~ unique(c(watchlist(watchlist_id)$symbol, symbols)),
                         ~ symbols)
  # convenience action assumption
  action <- purrr::when(action,
  # If symbols are present, new_name is absent & action = get (default)  assume action = "add"
                        . == "g" && !.nn && !is.null(symbols) ~ "a",
  # If new_name is present, assume action = "update"
                        .nn ~ "u",
                        ~ action)
  
  # Select the appropriate action
  fn <- switch(action, 
              g = httr::GET,
              a = ,
              c = httr::POST,
              u = httr::PUT,
              d = httr::DELETE)
  
  
  # Make body
  bodyl <- purrr::compact(list2(
    name = purrr::when(action,
                       . == "c" ~ watchlist_id,
                       .nn ~ new_name), 
    symbols = if (length(symbols) == 1) list(symbols) else symbols
  ))
  bodyl <- if (!rlang::is_empty(bodyl)) jsonlite::toJSON(bodyl, auto_unbox = TRUE, pretty = TRUE) else NULL
  # iterative actions
  if(action %in% c("a", "d") && !rlang::is_empty(symbols)) {
    out <- purrr::map(symbols, ~{
      if (action == "d") .args <- list(url = httr::build_url(purrr::list_merge(httr::parse_url(.url), path = .x)))
      else # if action = add symbol goes in body
        .args <- list(url = .url, body = jsonlite::toJSON(list(symbol = .x), auto_unbox = TRUE, pretty = TRUE))
      resp <- rlang::exec(fn, !!!.args, headers)
      wl_transform(resp, action = action)
    }) %>% {.[[length(.)]]}
  } else {
  # single operations
    out <- rlang::exec(fn, url = .url, headers, body = bodyl)
    out <- wl_transform(out, action = action)
  }
  
  return(out)
}


# wl_transform ----
# Sun May 03 08:55:01 2020
#' @title Transform watchlist objects
#'
#' @description Takes the watchlist endpoints server response and transforms to R amenable output
#' @param resp The response
#' @return \code{(tibble)} with respective R compliant objects (POSIXct)
#' @keywords internal

wl_transform <- function(resp, action) {
  
  stopifnot(inherits(resp, "response"))
  .wl <- response_text_clean(resp)
  if(!grepl(pattern = "^2", x = resp$status_code)) rlang::abort(paste("code:",resp$status_code,"\nmessage:", .wl$message))
  
  .q <- get_query(.wl)
  # Handle info
  if (!rlang::is_empty(.wl)) {
    .info <- purrr::map_at(.wl[1:5], c("created_at", "updated_at"), try_date, timeframe = "minute")
  } else if (action == "d" && resp$status_code == 204) {
    message(paste0("Watchlist deleted successfully"))
    out <- .wl
  }
  
  
  
  if ("assets" %in% names(.wl)) {
    # If it's a specific watchlist, make the watchlist info an attribute
    out <- .wl$assets
    attr(out, "info") <- .info
    
  } else if (length(.wl)  == 5) {
    # if it's an array of watchlists
    out <- .info 
  } else if (action == "d") {
    # if empty, return empty tibble
    out <- structure(data.frame(), query = get_query(.wl))
  }
  attr(out, "query") <- get_query(.wl)
  as_watchlist(out)
}

# TODO Create print methods, declare class
as_watchlist <- function(x) {
  if ("symbol" %in% names(x)) {
    .a <- purrr::list_merge(attributes(x), class = "watchlist")
  } else {
    .a <- attributes(x)
  }
  
  rlang::exec(structure, .Data = tibble::as_tibble(x), !!!.a)
}

