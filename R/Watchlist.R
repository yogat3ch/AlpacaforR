# watchlist ----
# Wed Apr 22 20:37:02 2020
#' @family Watchlist
#' @title Access all Watchlist Endpoints
#'
#' @description Access all [Watchlist](https://alpaca.markets/docs/api-documentation/api-v2/watchlist) endpoints. See link for details.
#' @param watchlist_id `(character)` **Name or ID** of watchlist to `GET`, `PATCH` or `DELETE`. If `NULL` **(Default)** the array of all saved `Watchlist` will be returned. If `NULL` and `name` is specified, `action` is assumed to be `"create"` and a `Watchlist` named `name` will be created with any `tickers` specified. All other actions to be performed on the provided `Watchlist` name or ID must be explicitly specified via `action`. See other parameters for details. The list below describes the behavior of this argument based on the value supplied to `action`:
#' \itemize{
#'  \item{\code{NULL}}{ If `watchlist_id = NULL` & `action = NULL`, and no other arguments specified. An array of all `Watchlist` will be returned. If `name` is specified, a `Watchlist` by that name will be created.}
#'   \item{\code{"create"}}{ A `Watchlist` by this name will be created. If `watchlist_id` & `name` are specified the `Watchlist` specified with `watchlist_id` will be renamed to `name` preserving existing tickers.}
#'   \item{\code{"add"}}{ If `tickers` are specified they will be added to the `Watchlist` specified.}
#'   \item{\code{"replace"}}{ If `tickers` are specified they will replace existing tickers or if `tickers = NULL`, all tickers will be removed.}
#'   \item{\code{"delete"}}{ If no tickers are specified, the `Watchlist` specified by `watchlist_id` will be deleted. If `tickers` are specified, the `Watchlist` specified by `watchlist_id` will have the specified tickers deleted from it.}
#' }
#' @param name `(character)` arbitrary name string, up to 64 characters. Default `NULL`. If only this is provided, `action` is assumed to be `"create"`. If only `name` & `watchlist_id` is provided the `Watchlist` specified by `watchlist_id` will be renamed to `name` preserving existing tickers. If this argument is specified and `action` is:
#' \itemize{
#'   \item{\code{"create"/NULL}}{ A `Watchlist` by this name will be created, unless `watchlist_id` is specified, in which case the specified `Watchlist` will be renamed.}
#'   \item{\code{"add"}}{ The `watchlist_id` specified will be renamed to this `name`. If `tickers` are specified they will be added or existing tickers will be preserved if `tickers = NULL`.}
#'   \item{\code{"replace"}}{ The `watchlist_id` specified will be named to this `name`. If `tickers` are specified they will replace existing tickers or if `tickers = NULL`, all tickers will be removed.}
#'   \item{\code{"delete"}}{ If no tickers are specified, this argument will be ignored and the `Watchlist` specified by `watchlist_id` will be deleted. If tickers are specified, the `Watchlist` specified by `watchlist_id` will be renamed to this `name` and the specified tickers will be deleted from it.}
#' }
#' @param tickers `(character)` Vector of symbols. Must be specified with a `watchlist_id` or `name`. Default `NULL`. If the `action` is:
#' \itemize{
#'   \item{\code{"create"/ NULL}}{ If this argument is specified with `name` a `Watchlist` will be created with these symbols. If `NULL` the `Watchlist` will be empty.}
#'  \item{\code{"add"}}{ If this argument is specified with `watchlist_id` the `tickers` will be added to the specified `Watchlist`. If this argument is specified with `watchlist_id` and `name` the `Watchlist` will be renamed and the `tickers` will be added.}
#'  \item{\code{"replace"}}{ If this argument is specified with `watchlist_id` the `tickers` will replace those in the specified `Watchlist`. If this argument is specified with `watchlist_id` and `name` the `Watchlist` will be renamed and the `tickers` will replace the existing ones. If `NULL` all tickers will be removed and the `Watchlist` will be empty.}
#'  \item{\code{"delete"}}{ If this argument is specified with `watchlist_id`, the `tickers` will be deleted from the `Watchlist`. If a `name` is also specified, the `Watchlist` will also be renamed. If `tickers` is `NULL` the `Watchlist` will be deleted.}
#' }
#' @param action `(character)` The action to take. See other parameters for detailed descriptions of how this argument determines the action when combinations of parameters are specified. This argument can be abbreviated with a single non-case-sensitive letter. One of:
#' \itemize{
#'  \item{\code{NULL}}{ If `action = NULL` & `watchlist_id = NULL`, and no other arguments specified. An array of all `Watchlist` will be returned. If only `name` is specified, `action` is assumed to be `create`. If `tickers` are specified, the tickers will be added.}
#'   \item{\code{"create"/"c"}}{`name` must be provided, and the a list will be created by this `name`. This action is assumed when only `name` is provided and `action = NULL`}
#'  \item{\code{"add"/"a"}}{ If `"add"`, the specified `watchlist_id` will have `tickers` added. If `name` is present, the `Watchlist` will be renamed to `name` and the `tickers` will be added. If `watchlist_id` & `tickers` are specified and `action = NULL` this action is assumed.}
#'  \item{\code{"replace"/"r"}}{ If `"replace"` is specified `watchlist_id` is *required* and will have it's tickers replaced by those provided to `tickers`. If `name` is also provided the `Watchlist` will be renamed and the `tickers` will replace the existing ones.}
#'  \item{\code{"delete"/"d"}}{ If `"delete"` is specified with `watchlist_id`, the `tickers` will be deleted from the `Watchlist`. If a `name` is also specified, the `Watchlist` will also be renamed. If `tickers` is `NULL` the `Watchlist` will be deleted.}
#' }
#' @inheritParams account
#' @return `Watchlist` `(tibble)` If `action = "create"/NULL` a [Watchlist](https://alpaca.markets/docs/api-documentation/api-v2/watchlist/#watchlist-entity) Object or array of `Watchlist` objects. For all others `action`s, an [Asset](https://alpaca.markets/docs/api-documentation/api-v2/assets/#asset-entity) Object or array of Asset objects with the `Watchlist` stored as an attribute retrieved with `attr(Asset, "info")`. A `Watchlist` object has the following information:
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
#' (wl <- watchlist(name = "test", tickers = "MSFT"))
#' # See it in the list of watchlists
#' watchlist()
#' # Get it
#' (test <- watchlist("test"))
#' identical(test,wl)
#' # Get that Watchlist's info
#' attr(wl, "info")
#' # delete that watchlist
#' watchlist("test", action = "d")
#' # create it again, this time with Apple and Walmart
#' watchlist(name = "test", tickers = c("AAPL", "WMT"))
#' # Update the watchlist name to test2 and replace the symbols with "FB", "AAPL", "AMZN", "NFLX", "GOOG"
#' (wl <- watchlist("test", name = "test2", tickers = c("FB", "AAPL", "AMZN", "NFLX", "GOOG", "WMT"), action = "r"))
#' attr(wl, "info")
#' # Walmart needn't be in there, remove it and rename the watchlist appropriately
#' (wl <- watchlist("test2", name = "FAANG", ticker = "WMT", action = "d"))
#' attr(wl, "info")
#' # A new go-to for gains: "FABANGG"
#' (wl <- watchlist("FAANG", "FABANGG", ticker = c("BYND", "GOOGL"), action = "a"))
#' # Back to the original FAANG
#' (wl <- watchlist("FABANGG", tickers = c("BYND", "GOOGL"), action = "d"))
#' # rename it appropriately
#' (wl <- watchlist("FABANGG", "FAANG"))
#' attr(wl, "info")
#' # remove it (with partial action argument)
#' watchlist("FAANG", a = "d")
#' @export
watchlist <-
  function(watchlist_id = NULL,
           name = NULL,
           tickers = NULL,
           action = NULL,
           live = as.logical(Sys.getenv("APCA-LIVE", FALSE))) {
    
  
  
  # parse action
  if (is.character(action)) {
    action <- tolower(substr(action,1,1))
  } else if (!is.null(name) && is.null(action) && is.null(watchlist_id)) {
    action <- "c"
  } else if (!is.null(name) && is.null(action) && !is.null(watchlist_id) || (!is.null(watchlist_id) && !is.null(tickers))) {
    # if a name is provided, and action is NULL, and a watchlist_id is provided
    action <- "a"
  } else {
    # action is assumed to be get
    action <- "g"
  }

  # Check parameters
  if (action == "c" && is.null(name)) {
    rlang::abort("`name` must be specified.")
  } else if (any(action %in% c("a", "r", "d")) && is.null(watchlist_id)) {
    rlang::abort("`watchlist_id` must be specified")
  } else if (action == "a" && is.null(watchlist_id) && is.null(tickers)) {
    rlang::abort("`tickers` must be specified")
  }
  
  
  # Set URL & Headers
  if (is_id(watchlist_id)) {
    .url <- get_url(c("watchlists", watchlist_id), live = live)
  } else if (!rlang::is_empty(nzchar(watchlist_id))) {
    .url = get_url(c("watchlists:by_name"), name = watchlist_id, live = live)
  } else {
    .url <- get_url("watchlists", live = live)
  }
  headers = get_headers(live)
  # Perform the appropriate action
  if (action == "g" || (is.null(action) && !is.null(watchlist_id))) {
    out = httr::GET(url = .url, headers)
  } else if (action == "c") {
    # if action is create, create the list 
    if (length(tickers) == 1) {
      # if only one ticker (NULL will be 0), must be a list such that JSON is correct
      tickers <- list(tickers)
    }
    bodyl <- jsonlite::toJSON(purrr::compact(list(
      name = name,
      symbols = tickers
    )), auto_unbox = TRUE)
    out = httr::POST(url = .url, body = bodyl, headers)
  } else if ((action == "a" && !is.null(name)) || action == "r" || (action == "d" && !is.null(name))) {
  
    # if we're adding/deleting and a name is specified, make the replacement symbols to PUT a combination of previous symbols and new tickers
    existing_wl <- watchlist(watchlist_id, live = live)
    if (action == "a") {
      .symbols <- c(tickers, existing_wl$symbol)
    } else if (action == "d") {
      .symbols <- existing_wl$symbol[!existing_wl$symbol %in% tickers]
    } else if (action == "r"){
      .symbols <- tickers
    }
    
    if (length(.symbols) == 1) {
      # must be wrapped in a list if length 1 such that the json comes out correctly
      .symbols <- list(.symbols)
    }
  
  
    bodyl <- jsonlite::toJSON(purrr::compact(list(
      name = name,
      symbols = .symbols
    )), auto_unbox = TRUE)
    out = httr::PUT(url = .url, body = bodyl, headers)
  } else if (action == "a") {
    .wl_info <- list()
    # if we just want to add, map over the provided tickers and add each of them
    out <- purrr::map(tickers, ~{
      bodyl <- jsonlite::toJSON(list(
        symbol = .x
      ), auto_unbox = T)
      .out = httr::POST(url = .url, body = bodyl, headers)
      .out = wl_transform(.out, action = action)
      if (length(.wl_info) == 0) {
        # assign the watchlist info only once
        .wl_info <<- attr(.out,"info")
      }
      .out
    })
    out <- out[[length(out)]]
  } else if (action == "d" && !is.null(tickers)) {
    .wl_info <- list()
    out <- purrr::map(tickers, ~{
      .url <- httr::parse_url(.url)
      .url$path <- c(.url$path, .x)
      .url <- httr::build_url(.url)
      .out = httr::DELETE(url = .url, headers)
      .out = wl_transform(.out, action = action)
      if (length(.wl_info) == 0) {
        # assign the .out info only once
        .wl_info <<- attr(.out,"info")
      }
      .out
    })
    out <- out[[length(out)]]
  } else if (action == "d"){
    out = httr::DELETE(url = .url, headers)
  }
  
  out <- wl_transform(out, action = action, wl_info = get0(".wl_info", inherits = FALSE))
  return(out)
}


# wl_transform ----
# Sun May 03 08:55:01 2020
#' @title Transform watchlist objects
#'
#' @description Replaces timestamps with POSIXct in watchlist info
#' @param wl The watchlist object
#' @return \code{(tibble)} with respective R compliant objects (POSIXct)
#' @keywords internal

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
        dplyr::mutate(tibble::as_tibble(purrr::map(.wl[1:5], rlang::`%||%`, NA)), dplyr::across(tidyselect::ends_with("at")), ~lubridate::ymd_hms(.x, tz = Sys.timezone())) %>% 
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
