
# watchlist ----
# Wed Apr 22 20:37:02 2020
#' @family Watchlist
#' @title Access all Watchlist Endpoints
#'
#' @description Access all [Watchlist](https://alpaca.markets/docs/api-documentation/api-v2/watchlist) endpoints. See link for details.
#' @param watchlist_id `(character)` Name or ID of watchlist to get or delete,  or if `tickers` is non NULL, to add tickers to, delete tickers from, or replace tickers of. If `NULL` (Default) the array of all `Watchlist` will be returned. If `NULL` and `name` is specified, `action` is assumed to be `"create"` and a `Watchlist` named `name` will be created with any `tickers` specified. All other actions to be performed on the provided `Watchlist` name or ID must be explicitly specified via `action`. See other parameters for details.
#' \itemize{
#'  \item{\code{NULL}}{ If `watchlist_id = NULL` & `action = NULL`, and no other arguments specified. An array of all `Watchlist` will be returned. If `name` is specified, `action` is assumed to be `create`.}
#'   \item{\code{"create"}}{ A `Watchlist` by this name will be created, unless `watchlist_id` is specified, in which case the specified `Watchlist` will be renamed preserving existing tickers. To replace tickers, `action` must explicitly be `"replace"`.}
#'   \item{\code{"add"}}{If `tickers` are specified they will be added to the `Watchlist`.}
#'   \item{\code{"replace"}}{If `tickers` are specified they will replace existing tickers or if `tickers = NULL`, all tickers will be removed.}
#'   \item{\code{"delete"}}{ If no tickers are specified, the `Watchlist` specified by `watchlist_id` will be deleted. If `tickers` are specified, the `Watchlist` specified by `watchlist_id` will have the specified tickers deleted from it.}
#' }
#' @param name `(character)` arbitrary name string, up to 64 characters. Default `NULL`. If only this is provided, `action` is assumed to be `"create"`. If only this & `watchlist_id` is provided, `action` is assumed to be rename the `Watchlist` specified by `watchlist_id` and preserve existing tickers. If this argument is specified and `action` is:
#' \itemize{
#'   \item{\code{"create"/NULL}}{ A `Watchlist` by this name will be created, unless `watchlist_id` is specified, in which case the specified `Watchlist` will be renamed.}
#'   \item{\code{"add"}}{ The `watchlist_id` specified will be renamed to this `name`. If `tickers` are specified they will be added or existing tickers will be preserved if `tickers = NULL`.}
#'   \item{\code{"replace"}}{ The `watchlist_id` specified will be named to this `name`. If `tickers` are specified they will replace existing tickers or if `tickers = NULL`, all tickers will be removed.}
#'   \item{\code{"delete"}}{ If no tickers are specified, this argument will be ignored and the `Watchlist` specified by `watchlist_id` will be deleted. If tickers are specified, the `Watchlist` specified by `watchlist_id` will be renamed to this `name` and the specified tickers will be deleted from it.}
#' }
#' @param tickers `(character)` Vector of symbols. Must be specified with a `watchlist_id` or `name`. Default `NULL`. If the `action` is:
#' \itemize{
#'   \item{\code{"create"/ NULL}}{If this argument is specified with `name` a `Watchlist` will be created with these symbols. If `NULL` the `Watchlist` will be empty.}
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
#'  \item{`name`}{`(character)` The name of the watchlist.}
#'  \item{`updated_at`}{`(POSIXct)` The timestamp of the last update.}
#'  \item{`id`}{`(character)` Watchlist ID}
#'  \item{`account_id`}{`(character)` The account ID associated with the watchlist}
#'  \item{`created_at`}{`(POSIXct)` The timestamp of the watchlist creation.}
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
#' @importFrom httr GET PUT POST DELETE parse_url build_url
#' @importFrom purrr compact map_dfr
#' @importFrom stringr str_count
#' @importFrom rlang abort
#' @importFrom jsonlite toJSON
#' @importFrom magrittr `%>%`
#' @importFrom dplyr distinct
#' @export
watchlist <- function(watchlist_id = NULL, name = NULL, tickers = NULL, action = NULL, live = FALSE, v = 2){
  
  # Set URL & Headers
  .url = httr::parse_url(get_url(live))
  headers = get_headers(live)
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
  
  # Get the appropriate ID
  .wlid_len <- tryCatch(nchar(watchlist_id), error = function(e) 0)
  .wl_dash <- ifelse(length(stringr::str_count(watchlist_id, "-")) == 0, 0, stringr::str_count(watchlist_id, "-")) 
  if ((.wlid_len == 36 && .wl_dash == 4) || is.null(watchlist_id)) {
    # if watchlist_id is an id or left blank
    .id <- watchlist_id
  } else if (.wlid_len > 0) {
    .id <- wl_nm2id(watchlist_id)
  }
  # Set the URL Path
  .url$path <- purrr::compact(list(paste0("v",v),
                                   "watchlists",
                                   .id))
  .url <- httr::build_url(.url)
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
    )), auto_unbox = T)
    out = httr::POST(url = .url, body = bodyl, headers)
  } else if ((action == "a" && !is.null(name)) || action == "r" || (action == "d" && !is.null(name))) {
  
    # if we're adding/deleting and a name is specified, make the replacement symbols to PUT a combination of previous symbols and new tickers
    existing_wl <- watchlist(.id, live = live, v = v)
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
    )), auto_unbox = T)
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
      .out = httr::DELETE(url = paste0(.url,"/",.x), headers)
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
  
  out <- wl_transform(out, action = action, wl_info = get0(".wl_info", inherits = F))
  return(out)
}
#' @family Watchlist
#' @rdname watchlist
#' @title create_watchlist
#' @description `create_watchlist` is deprecated. Use \code{\link[AlpacaforR]{watchlist}} instead.
#' @examples create_watchlist(name = "test", tickers = c("AAPL","WMT"), live = FALSE)
#' watchlist("test", action = "d")
#' @export

create_watchlist <- watchlist


#----------------------------------------------------------------------------------------------
#' @family Watchlist
#' @title Delete Watchlist function (Deprecated)
#'
#' @description Deprecated, use \code{\link[AlpacaforR]{watchlist}} instead. Delete a watchlist. This is a permanent deletion.
#' @param watchlist_id Watchlist ID
#' @param live TRUE / FALSE if you are connecting to a live account. Default to FALSE, so it will use the paper url if nothing was provided.
#' @examples 
#' delete_watchlist(watchlist_id = "ea3cc05b-2844-4985-b0c9-8f412718ac9e", live = FALSE)
#' @importFrom httr DELETE
#' @export
delete_watchlist <- function(watchlist_id = NULL, live = FALSE){
  message(paste0("Deprecated, use `watchlist` instead"))
  # Set URL & Headers
  url = get_url(live)
  headers = get_headers(live)
  
  
  watchlist = httr::DELETE(url = paste0(url, "/v2/watchlists/",watchlist_id), headers)
  
  if(watchlist$status_code == 204){ cat(paste0("Success, Watchlist ID",watchlist_id, " deleted"))}
  
  else{
    cat("Watchlist ID",watchlist_id," not sucessfully deleted")
  }
  
}
#NEW for V2
#delete_watchlist(watchlist_id = "ea3cc05b-2844-4985-b0c9-8f412718ac9e", live = FALSE)








#' @title get_watchlist
#' @rdname watchlist
#' @description `get_watchlist` is deprecated. Use \code{\link[AlpacaforR]{watchlist}} instead. 
#' @examples get_watchlist()
#' @export
get_watchlist <- watchlist







# watchlist_update ----
# Wed Apr 22 20:38:33 2020
#' @family Watchlist
#' @title Update Watchlist function
#'
#' @description Update the name and/or content of watchlist. For symbols, You must provide current tickers, and new ticker to add.
#' @inheritParams watchlist
#' @param name `(character)` arbitrary replacement name, up to 64 characters.
#' @param tickers `(character)` Set of symbols to add, replace or delete those in the watchlist according to the `action` argument. See Details for specifics
#' @param action `(character)` Action in reference to symbols specified by `tickers`. Whether to add ("add"/"a") them to the existing,  replace ("replace","r") all existing assets in the watchlist with them or delete ("delete"/"d") them from the existing symbols in a watchlist. Case-insensitive. Default `"add"`. See Details for specifics.
#' @details The default `action` is `"add"` which will add any `tickers` specified to the watchlist indicated with `watchlist_id`. When a new watchlist `name` is specified in addition to `action = "add"` (Default), existing symbols in the watchlist will be preserved if no `tickers` are specified. If `tickers` are provided they will be added after the watchlist is renamed. If `action = "replace"`, symbols in the specified watchlist will be replaced by those provided to the `tickers` argument. If `action = "delete"`, the symbols specified by `tickers` will be removed from the watchlist. If a new `name` is provided concurrently, the `name` will first be changed, then the `tickers` will be removed.
#' @inheritParams account
#' @inherit watchlist return
#' @inherit assets return
#' @inherit watchlist details
#' @examples
#' watchlist("test", tickers = c("AAPL", "WMT"))
#' # Update the watchlist name to test2 and replace the symbols with "FB", "AAPL", "AMZN", "NFLX", "GOOG"
#' (wl <- watchlist_update("test", name = "test2", tickers = c("FB", "AAPL", "AMZN", "NFLX", "GOOG"), action = "r"))
#' # Walmart needn't be in there, remove it and rename the watchlist appropriately
#' (wl <- watchlist_update("test2", name = "FAANG", ticker = "WMT", action = "d"))
#' # The next iteration of "FAANG", "FABANGG"
#' (wl <- watchlist_update("FAANG", "FABANGG", ticker = c("BYND", "GOOGL"), action = "a"))
#' # Back to the original FAANG
#' (wl <- watchlist_update("FABANGG", tickers = c("BYND", "GOOGL"), action = "d"))
#' # rename it appropriately
#' (wl <- watchlist_update("FABANGG", "FAANG"))
#' attr(wl, "info")
#' # remove it
#' watchlist("FAANG", a = "d")
#' @importFrom httr GET PUT POST parse_url build_url
#' @importFrom purrr compact map_dfr
#' @importFrom stringr str_count
#' @importFrom rlang warn abort
#' @importFrom jsonlite toJSON
#' @importFrom magrittr `%>%`
#' @importFrom dplyr distinct
#' @keywords internal
watchlist_update <- function(watchlist_id = NULL, name = NULL, tickers = NULL, action = "add", live = FALSE, v = 2){
  if (is.null(watchlist_id)) stop("Must specify `watchlist_id`")
  # allow action to be partialed with a single letter. r for "remove"
  action <- c(a = "add", d = "delete", r = "replace")[tolower(substr(action,1,1))]
  # Set URL & Headers
  .url = httr::parse_url(get_url(live))
  headers = get_headers(live)
  # Determine the watchlist_id, and create the url path ----
  .wlid_len <- tryCatch(nchar(watchlist_id), error = function(e) 0)
  .wl_dash <- ifelse(length(stringr::str_count(watchlist_id, "-")) == 0, 0, stringr::str_count(watchlist_id, "-")) 
  if ((.wlid_len == 36 && .wl_dash == 4)) {
    # if watchlist_id is an id or left blank
    .url$path <- purrr::compact(list(paste0("v",v),
                                     "watchlists",
                                     watchlist_id))
    
  } else if (.wlid_len > 0) {
    # if watchlist_id is a name
    .id <- wl_nm2id(watchlist_id)
    if (!is.character(.id)) {
      rlang::warn("Returning watchlist.")
      return(.id)
    }
    
    # make the path
    .url$path <- purrr::compact(list(paste0("v",v),
                                     "watchlists",
                                     .id))
    
  }
  .url <- httr::build_url(.url)
  # Determine the sequence of actions
  # create an env for the watchlist info
  wl <- list()
  if ((action == "add" && !is.null(name)) || action == "replace") {
    # if we're adding when name is specified or just replacing, create the appropriate list of symbols
    if (action == "add" && !is.null(name)) {
      # if we're adding and a name is specified, make the replacement symbols to PUT a combination of previous symbols and new tickers
      existing_wl <- watchlist(.id)
      .symbols <- c(tickers, existing_wl$symbol)
      if (length(.symbols) == 1) {
        # must be wrapped in a list if length 1 such that the json comes out correctly
        .symbols <- list(.symbols)
      }
    } else {
      if (length(tickers) == 1) {
        # must be wrapped in a list if length 1 such that the json comes out correctly
        .symbols <- list(tickers)
      } else {
        # IF null, will be removed by purrr::compact when constructing body
        .symbols <- tickers
      }
    }
    
    bodyl <- jsonlite::toJSON(purrr::compact(list(
      name = name,
      symbols = .symbols
    )), auto_unbox = T)
    watchlist = httr::PUT(url = .url, body = bodyl, headers)
    out <- wl_transform(watchlist)
  } else if (action == "replace" || (action == "delete" && !is.null(name))) {
    if (action == "delete" && !is.null(name)) {
      # if we're deleting and a name is specified, make the replacement symbols to PUT a subset of previous symbols without new tickers
      existing_wl <- watchlist(.id)
      .symbols <- existing_wl$symbol[!existing_wl$symbol %in% tickers]
      if (length(.symbols) == 1) {
        # must be wrapped in a list if length 1 such that the json comes out correctly
        .symbols <- list(.symbols)
      }
    } else {
      if (length(tickers) == 1) {
        # must be wrapped in a list if length 1 such that the json comes out correctly
        .symbols <- list(tickers)
      } else {
        # IF null, will be removed by purrr::compact when constructing body
        .symbols <- tickers
      }
    }
    # if we're adding but we're also changing the name
    bodyl <- jsonlite::toJSON(purrr::compact(list(
      name = name,
      symbols = .symbols
    )), auto_unbox = T)
    watchlist = httr::PUT(url = .url, body = bodyl, encode = "json", headers)
    out <- wl_transform(watchlist)
  } else if (action == "add") {
    # if we just want to add, map over the provided tickers and add each of them
    out <- purrr::map_dfr(tickers, ~{
      bodyl <- jsonlite::toJSON(list(
        symbol = list(.x)
      ), auto_unbox = T)
      watchlist = httr::POST(url = .url, body = bodyl, headers)
      watchlist = wl_transform(watchlist)
      if (length(wl) == 0) {
        # assign the watchlist info only once
        wl <<- attr(watchlist,"info")
      }
      watchlist
    }) %>% dplyr::distinct()
  } else if (action == "delete") {
    out <- purrr::map_dfr(tickers, ~{
      watchlist = httr::DELETE(url = paste0(.url,"/",.x), headers)
      watchlist = wl_transform(watchlist)
      if (length(wl) == 0) {
        # assign the watchlist info only once
        wl <<- attr(watchlist,"info")
      }
      watchlist
    }) %>% dplyr::distinct()
  }
  if (length(wl) > 0) {
    attr(out, "info") <- wl
  }
  return(out)
}
#NEW for V2
#update_watchlist(watchlist_id = "ea3cc05b-2844-4985-b0c9-8f412718ac9e", name = "test_watch", tickers = c("AAPL","WMT"), live=TRUE)
#' @family Watchlist
#' @description `update_watchlist` is deprecated. Use \code{\link[AlpacaforR]{watchlist}} instead.
#' @rdname watchlist
#' @export
update_watchlist <- watchlist_update







#----------------------------------------------------------------------------------------------
#' @family Watchlist 
#' @aliases watchlist_update
#' @title Add to Watchlist function (Deprecated)
#'
#' @description `add_to_watchlist` is deprecated. Use \code{\link[AlpacaforR]{watchlist}} with `action = "add"` instead. Append an asset for the symbol to the end of watchlist asset list. You must provide current tickers, and new ticker to add.
#' @param watchlist_id Watchlist ID 
#' @param tickers Set of symbols. 
#' @param live TRUE / FALSE if you are connecting to a live account. Default to FALSE, so it will use the paper url if nothing was provided.
#' @return A watchlist object with updated content.
#' @examples 
#' add_to_watchlist(watchlist_id = "ea3cc05b-2844-4985-b0c9-8f412718ac9e", tickers = c("AAPL","WMT"), live = FALSE)
#' @importFrom httr PUT
#' @export

add_to_watchlist <- function(watchlist_id = NULL, tickers = NULL, live = FALSE){
  message(paste0("add_to_watchlist is deprecated, use `watchlist_update` instead."))
  # Set URL & Headers
  url = get_url(live)
  headers = get_headers(live)
  
  
  bodyl = list(symbols=tickers)
  watchlist = httr::PUT(url = paste0(url, "/v2/watchlists/",watchlist_id), body = bodyl, encode = "json", headers)
  watchlist = response_text_clean(watchlist)
  return(watchlist)
  
}
#NEW for V2
#add_to_watchlist(watchlist_id = "ea3cc05b-2844-4985-b0c9-8f412718ac9e", tickers = c("AAPL","WMT","AMD"), live = FALSE)
















#----------------------------------------------------------------------------------------------
#' @family Watchlist
#' @title Delete from Watchlist (Deprecated)
#'
#' @description Deprecated, use \code{\link[AlpacaforR]{watchlist}} with `action = "delete"` instead. Delete one entry for an asset by symbol name.
#' @seealso watchlist_update
#' @param watchlist_id Watchlist ID
#' @param tickers Symbol name to remove from the watchlist content.
#' @param live TRUE / FALSE if you are connecting to a live account. Default to FALSE, so it will use the paper url if nothing was provided.
#' @examples 
#' \dontrun{
#' delete_from_watchlist(watchlist_id = "ea3cc05b-2844-4985-b0c9-8f412718ac9e", ticker = "AAPL", live = FALSE)
#' }
#' @importFrom httr DELETE
#' @export

delete_from_watchlist <- function(watchlist_id = NULL, ticker = NULL, live = FALSE){
  message(paste0("delete_from_watchlist is deprecated, use `watchlist_update` instead."))
  # Set URL & Headers
  url = get_url(live)
  headers = get_headers(live)
  
  
  watchlist = httr::DELETE(url = paste0(url, "/v2/watchlists/", watchlist_id, "/", ticker), headers)
  watchlist = response_text_clean(watchlist)
  return(watchlist)
  
}
#NEW for V2
#delete_from_watchlist(watchlist_id = "ea3cc05b-2844-4985-b0c9-8f412718ac9e", ticker = "AAPL", live = FALSE)

