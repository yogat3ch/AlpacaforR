#Websockets:  Wed Mar 25 14:00:19 2020 ----
#' @import websocket

#' @family Websockets
#' @title ws_create: Create a Websocket to the Alpaca or Polygon API
#' @description The [Alpaca Streaming API](https://alpaca.markets/docs/api-documentation/api-v2/streaming/) provides Trade & Account updates, while the [Polygon Websocket API](https://polygon.io/sockets) offers Trade, Quote, Aggregate (per second) and Aggregate (per minute) streaming. 
#' @param api `(character)` The streaming API to connect to, either `"Alpaca"/"a"` or `"Polygon"/"p"`. 
#' @param log_msgs \code{(logical/character)} A logical indicating whether to use a log file for websocket messages (defaults to *"ws_{API}.log"* in the working directory if `TRUE`), where `{API}` is `a/p` depending on the API selected. Can also be the name of the text file where logs for this session will be stored. IE if you want to save all messages in `Alpaca.log` in the working directory, set `log_msgs = "Alpaca"`. If `".log"` is not included in the name it will be appended. Previous logfiles of the same name will be reused without overwrite. No logs created if `FALSE`. **Default `TRUE`**. If a path is specified to `log_path`, the logs will reside in this directory.
#' @param log_bars `(logical)` *Polygon only* Flag to indicate whether to save streaming tick data from [Polygon.io channels](https://polygon.io/sockets). Tick data is saved into a `.csv` instead of a `.log` file. The filename will be `[Channel].csv` where channel is the name of the channel supplied to the `channel` argument in \code{\link[AlpacaforR]{ws_listen}}. If a path is specified to `log_path`, the CSV will reside in this directory.
#' @param log_path `(character)` The path where the .log (for log_msgs) or .csv (for log_bars) files are to be stored. Tilde path expansion is supported, see \code{\link[base]{path.expand}}.
#' @param action `(expression)` An R expression, see \code{\link[rlang]{expr}}. This expression will be executed upon receipt of each message. Useful for setting trailing stop algorithms, or executing an order based on specific criteria. **Note** that all values and objects necessary to the function of the `action` should be defined within the expression. Calls to objects stored in the global environment are likely to cause errors. *Tidy evaluation is supported.* The relevant objects that may be referred to when writing this expression are:
#' \itemize{
#'   \item{\code{.o}}{ Data returned in message. For Polygon subscriptions, `.o` is a `data.frame`. See [Polygon - Sockets: Stock Schemas](https://polygon.io/sockets) for the properties specific to each channel.}
#'   \item{\code{out$env}}{ The environment in the `list` returned by this function.}
#' }  
#' @param aenv \code{(environment)} An environment can be supplied in which `action` will be evaluated. Useful to make variables available to the evaluation of `action` from the global or other environments to avoid overwrite/removal of these dependent variables if they reside in the current global environment. One use case: if certain functions within `action` for a Polygon websocket are triggered by messages from the Alpaca websocket. The associated `env` from the Alpaca websocket list object may be included as a part of this environment such that these functions can be triggered as intended. 
#' @param toConsole `(logical)` Whether to print messages to the console. All open/close/error status messages will be printed by default, but subscribed feed messages may be muted with this argument. Default `TRUE`.
#' @details This function allows a simple set-up for the majority of use cases. The connection can be closed at any time by using the `[OBJECT]$ws$close()` method, where `[OBJECT]` is the object returned from `ws_create`.
#' `log_msgs` , `log_bars`, and `toConsole` are all `logical` values stored to `env`, and can be modified in place to update the behavior of the websocket.  selectively toggling on/off these features. `action` & `aenv` are also stored to `env` and can be modified in place as well.  `log_path` and `logfile` are stored, but cannot be modified.
#' If `log_msgs = TRUE` or a file name is provided, the log file will contain:
#' \itemize{
#'  \item{The time the connection is created}
#'  \item{The time and content of each message}
#'  \item{The time the connection is closed}
#' }
#' This log file can be read with \code{\link[base]{readLines}} or \code{\link[utils]{read.csv}}.
#' If `log_msgs` is FALSE, no log file will be created.
#' If you wish to perform actions based on specific message content, you can do one of following: 
#' \enumerate{
#'  \item{For simple functions, use the \code{action} argument.}
#'  \item{Use a background process with \href{https://www.rdocumentation.org/packages/later/versions/1.0.0/topics/later}{later::later} or \href{https://www.rdocumentation.org/packages/callr/versions/3.4.3/topics/r_bg}{callr::r_bg} to monitor the \code{[OBJECT]$env$lastmessage} object or the \code{[OBJECT]$env$msgs} or \code{[OBJECT]$env$bars} (Polygon) object and perform an action when a specific message appears.}
#'  \item{Build a websocket from scratch using the \href{https://rstudio.github.io/websocket/}{websocket documentation} and include the appropriate hooks and functions in the \code{onMessage} method.}
#' }
#' The websocket is authenticated using the environment variables `APCA-LIVE-API-KEY-ID`. If unset, please see the Installation vignette `vignette("Installation", "AlpacaforR")` for instructions on setting this value. 
#' @return `(list)` With the following objects:
#' \itemize{
#'  \item{`ws`}{ `(environment)`  The Websocket environment object.}
#'  \item{`env`}{ `(environment)` Environment in which the `lastmessage` and `tibble` of messages will be stored.}
#' }
#' Within `env` are the following variables:
#' \itemize{
#'   \item{\code{lastmessage}}{ \code{(character)} **Note** An object with the last message received from the websocket.}
#'  \item{\code{msgs}}{ \code{(tibble)}  object that stores the timestamps `ts` and message content `msg` for all messages received via the websocket.}
#'  \item{\code{bars}}{ \code{(list)} For Polygon websockets, all channel data are stored in this object as `tibble`s named according to the channel.}
#' }
#' @examples 
#' \dontrun{
#' # must be run in an interactive session
#' if (interactive()) {
#'   # Create the Alpaca Websocket and message environment. 
#'   wsa <- ws_create("a")
#'   # Create the Polygon websocket and message environment, with ticks being logged.
#'   wsp <- ws_create("p", log_bars = T)
#'   # if you're unable to remember which API the ws object corresponds to:
#'   attr(wsa, "api")
#' }
#' }
#' @export

ws_create <- function(api = c("a", "p")[1], log_msgs = TRUE, log_bars = FALSE, log_path = "", action = NULL, aenv = NULL, toConsole = T) {
  api <- tolower(substr(api, 0, 1))
  if (api == "a" && isTRUE(log_bars)) {
    message(paste0("Bars are only logged for Polygon websocket endpoints. `log_bars` will be ignored."))
    log_bars <- FALSE
  }
  if (!interactive()) stop("ws_create only runs properly in an interactive session")
  # Determine if a logs should be created
  .log <- ifelse(isTRUE(log_msgs) || isTRUE(log_bars) || inherits(log_msgs, "character"), T, F)
  if (.log) {
    # If logfile is selected, but not yet a character, make it the default "ws"
    logfile <- ifelse(is.character(log_msgs), log_msgs, paste0("ws_", api))
    # If .log was not provided in the character provided, append it
    logfile <- ifelse(grepl("\\.log$", logfile), logfile, paste0(logfile,".log"))
    # if a tilde path is provided expand it
    if (grepl("^~", log_path)) log_path <- path.expand(log_path)
    # if directory paths are provided, create them if need be
    if (grepl("\\/", log_path)) {
      .paths <- stringr::str_split(log_file, "/")[[1]] %>% .[-length(.)]
      purrr::walk(purrr::accumulate(.paths, ~{
        .p <- paste0(.x,"/",.y)
      }), ~{
        if (!dir.exists(.x)) dir.create(.x)
      })
    } else if (nchar(log_path) > 0) {
      log_path <- ifelse(grepl("/$", log_path), log_path, paste0(log_path,"/"))
      if (!dir.exists(log_path)) dir.create(log_path)
    }
    logfile <- paste0(log_path,logfile)
    if (!file.exists(logfile)) {
      file.create(logfile)
      write("Timestamp, Message", file = logfile)
    }
    # change log_msgs to boolean now that logfile exists
    log_msgs <- ifelse(inherits(log_msgs, "character") || isTRUE(log_msgs), TRUE, FALSE)
  }
  
  # Create the Websocket
  if (api == "a") {
    ws <- websocket::WebSocket$new(url = paste0(gsub("^https", "wss", get_url("stream", live = TRUE))), autoConnect = FALSE)
  } else {
    ws <- websocket::WebSocket$new(url = "wss://socket.polygon.io/stocks", autoConnect = FALSE)
  }
  out <- list(ws = ws, env = new.env())
  e <- environment()
  purrr::walk(c("log_bars", "log_path", "log_msgs", "logfile", "action", "toConsole", "action"), ~{
    assign(.x, get0(.x, e), out$env)
  })
  rm(e)
  ws$onOpen(function(event) {
    .msg <- paste0(Sys.time(),", ", "Connection Created")
    ws_msg(out, msg = .msg)
    ws_log(out, .log = .log, msg = .msg) 
    if (api == "a") {
      .auth <- jsonlite::toJSON(list(action = c("authenticate"),
                                     data = list(key_id = Sys.getenv("APCA-LIVE-API-KEY-ID"),
                                                 secret_key = Sys.getenv("APCA-LIVE-API-SECRET-KEY"))), complex = "string", auto_unbox = TRUE)
      out$ws$send(.auth)
    } 
  })
  ws$onMessage(function(event) {
    if (api == "a") {
      .msg <- paste0(readBin(event$data, character(), n=1000), collapse="")
      .o <- NULL
    } else {
      
      .o <- jsonlite::fromJSON(event$data)
      if (.o$ev %in% c("T", "Q", "A", "AM")) {
        `!!` <- rlang::`!!`
        if(.o$ev %in% c("T", "Q")) {
          .vars <-  c("t")
        } else {
          .vars <- c("s","e")
        }
        .o <- dplyr::mutate(tibble::as_tibble(.o), dplyr::across(.vars, ~ lubridate::as_datetime(.x / 1e3, tz = "America/New_York", origin = lubridate::origin)))
      }
      if (!is.null(.o$status)) {
        if (.o$status == "connected") {
          .auth <- jsonlite::toJSON(list(action = c("auth"),
                                         params = Sys.getenv("APCA-LIVE-API-KEY-ID")), complex = "string", auto_unbox = TRUE)
          out$ws$send(.auth)
        }
      }
        
      .msg <- paste0(purrr::imap_chr(.o, ~{
         .msg <- paste0(.y,": ",.x)
       }), collapse = " | ")
      # If listening to a subscription channel
    }
    if (rlang::is_environment(aenv)) {
      out$env$aenv <- aenv
      parent.env(aenv) <- environment()
      .aenv <- aenv
    } else {
      .aenv <- environment()
    }
    
    if (!is.null(out$env$action) && rlang::is_expression(out$env$action)) rlang::eval_tidy(out$env$action, env = .aenv)
    
    ws_msg(out, .o = .o, msg = .msg)
    ws_log(out, .log = .log, .o = .o, msg = .msg)
    return(.msg)
  })
  ws$onClose(function(event) {
    .r <- c(`1000` = "closed by user", `1005` = "no status received", `1006` = "end of broadcast")[as.character(event$code)]
    
    .msg <- paste0(Sys.time(),", Websocket client disconnected with code: ", event$code,
        " and reason: ", .r)
    ws_msg(out, msg = .msg)
    ws_log(out, .log = .log, msg = .msg)
  })
  ws$onError(function(event) {
    .msg <- paste0(readBin(event$message, character(), n=1000), collapse="")
    .msg <- paste0(Sys.time(),", Websocket Error: '", .msg, "'")
    ws_msg(out, msg = .msg)
    ws_log(out, .log = .log, msg = .msg)
  })
  attr(out, "api") <- api
  out$ws$connect()
  return(out)
}

# Generate Endpoint return lists

# .j <- list()
# .j[["Trade"]] <- ('
# {
#     "ev": "T",              // Event Type
#     "sym": "MSFT",          // Symbol Ticker
#     "x": 4,                 // Exchange ID
#     "i": "12345",           // Trade ID
#     "z": 3,                 // Tape ( 1=A 2=B 3=C)
#     "p": 114.125,           // Price
#     "s": 100,               // Trade Size
#     "c": [0, 12],           // Trade Conditions
#     "t": 1536036818784      // Trade Timestamp ( Unix MS )
# }
# ')
# 
# .j[["Quote"]] <- '
# 
# {
#     "ev": "Q",              // Event Type
#     "sym": "MSFT",          // Symbol Ticker
#     "bx": "4",              // Bix Exchange ID
#     "bp": 114.125,          // Bid Price
#     "bs": 100,              // Bid Size
#     "ax": "7",              // Ask Exchange ID
#     "ap": 114.128,          // Ask Price
#     "as": 160,              // Ask Size
#     "c": 0,                 // Quote Condition
#     "t": 1536036818784      // Quote Timestamp ( Unix MS )
# }
# '
# 
# .j[["Agg"]] <- '
# 
# {
#     "ev": "AM",             // Event Type ( A = Second Agg, AM = Minute Agg )
#     "sym": "MSFT",          // Symbol Ticker
#     "v": 10204,             // Tick Volume
#     "av": 200304,           // Accumulated Volume ( Today )
#     "op": 114.04,           // Todays official opening price
#     "vw": 114.4040,         // VWAP (Volume Weighted Average Price)
#     "o": 114.11,            // Tick Open Price
#     "c": 114.14,            // Tick Close Price
#     "h": 114.19,            // Tick High Price
#     "l": 114.09,            // Tick Low Price
#     "a": 114.1314,          // Tick Average / VWAP Price
#     "s": 1536036818784,     // Tick Start Timestamp ( Unix MS )
#     "e": 1536036818784,     // Tick End Timestamp ( Unix MS )
# }'
#   .j %>% purrr::map_depth(2, ~{
#     stringr::str_split(.x, "\\n") %>%
#       magrittr::extract2(1) %>% 
#       subset(nchar(.) > 4) %>% 
#       stringr::str_match_all('(\\w+)(?=\\"\\:).*(?<=\\/\\/)(.*)') %>% 
#       do.call(rbind, .) %>% 
#       magrittr::extract(,-1) %>% 
#       apply(1, function(r){
#         glue::glue('\\item{`{{r[1]}}`}{ {{trimws(r[2])}}}', .open = "{{", .close = "}}")
#       }) %>% 
#       paste0(collapse = "\n")
#   }) %>% purrr::imap(~paste0("\n",.y,"\n\\itemize{\n", .x,"\n}")) %>% 
#     do.call(c, .) %>% cat


#' @family Websockets
#' @title ws_listen: A function to set listening streams with the Alpaca websocket API
#' @description  Given the `websocket environment` the function will subscribe to any channels specified to the `channel` argument. Must be called on each Websocket object individually. 
#' @param ws \code{(list/Websocket)} The websocket list or Websocket object created with \code{\link[AlpacaforR]{ws_create}}.
#' @param channel \code{(character)} vector of the channels to connect to.
#' For an Alpaca websocket object provided as \code{ws} these are:
#' \itemize{
#'  \item{\code{"Account"/"a"}}{[Alpaca account stream](https://alpaca.markets/docs/api-documentation/api-v2)}
#'  \item{\code{"Trade"/"t"}}{[Alpaca trade stream](https://alpaca.markets/docs/api-documentation/api-v2/streaming#order-updates)}
#' }
#' The Default is to connect to both.
#' For a Polygon websocket object provided as `ws` these are:
#' [Available Channels](https://polygon.io/sockets):
#' \itemize{
#'  \item{T.* Trades}
#'  \item{Q.* Quotes}
#'  \item{A.* Aggregate ( per second )}
#'  \item{AM.* Aggregate ( per minute )}
#' }
#' where * is the ticker symbol and the argument is case-insensitive. IE `"am.bynd"` will subscribe to by minute aggregates for Beyond Meat. Multiple channels can be subscribed to using a character vector.
#' @param unsub `(logical)` flag indicating whether to unsubscribe from the arguments provided to `channel` *Polygon only*.
#' @return Returns nothing. All received messages are stored according to the parameters provided to \code{\link[AlpacaforR]{ws_create}}. For Polygon, the data returned depends on the subscription:
#' 
#' Trade
#' \itemize{
#'   \item{`ev`}{ Event Type}
#'   \item{`sym`}{ Symbol Ticker}
#'   \item{`x`}{ Exchange ID}
#'   \item{`i`}{ Trade ID}
#'   \item{`z`}{ Tape ( 1=A 2=B 3=C)}
#'   \item{`p`}{ Price}
#'   \item{`s`}{ Trade Size}
#'   \item{`c`}{ Trade Conditions}
#'   \item{`t`}{ Trade Timestamp ( Unix MS )}
#' } 
#' Quote
#' \itemize{
#'   \item{`ev`}{ Event Type}
#'   \item{`sym`}{ Symbol Ticker}
#'   \item{`bx`}{ Bix Exchange ID}
#'   \item{`bp`}{ Bid Price}
#'   \item{`bs`}{ Bid Size}
#'   \item{`ax`}{ Ask Exchange ID}
#'   \item{`ap`}{ Ask Price}
#'   \item{`as`}{ Ask Size}
#'   \item{`c`}{ Quote Condition}
#'   \item{`t`}{ Quote Timestamp ( Unix MS )}
#' } 
#' Agg
#' \itemize{
#'   \item{`ev`}{ Event Type ( A = Second Agg, AM = Minute Agg )}
#'   \item{`sym`}{ Symbol Ticker}
#'   \item{`v`}{ Tick Volume}
#'   \item{`av`}{ Accumulated Volume ( Today )}
#'   \item{`op`}{ Todays official opening price}
#'   \item{`vw`}{ VWAP (Volume Weighted Average Price)}
#'   \item{`o`}{ Tick Open Price}
#'   \item{`c`}{ Tick Close Price}
#'   \item{`h`}{ Tick High Price}
#'   \item{`l`}{ Tick Low Price}
#'   \item{`a`}{ Tick Average / VWAP Price}
#'   \item{`s`}{ Tick Start Timestamp ( Unix MS )}
#'   \item{`e`}{ Tick End Timestamp ( Unix MS )}
#' }
#' @examples
#'  \dontrun{
#'  # must be run in an interactive session
#'  # See ?AlpacaforR::ws_create for tutorial on creating a connection
#'  if (interactive()) {
#'    # Subscribe to Alpaca trade and account updated by default
#'    ws_listen(wsa)
#'    #Subscribe to the aggregates (per second) for Beyond Meat
#'    ws_listen(wsp, channel = "a.bynd")
#'    #You can close the connection(s) at any point with the following:
#'    wsa$ws$close()
#'    wsp$ws$close()
#'  }
#'  }
#' @export

ws_listen <- function(ws, channel = NULL, unsub = FALSE) {
  if (is.list(ws)) ws_o <- ws$ws
  api <- attr(ws, "api")
  if (api == "a") {
    if (is.null(channel)) {
      .s <- list("trade_updates","account_updates")
    } else {
      .c <- tolower(substr(channel, 0, 1))
      .s <- purrr::map_chr(.c, ~purrr::pluck(list(a = "account_updates", t = "trade_updates"),.x))
      if (length(.s) == 1) .s <- list(.s)
    }
    .listen <- jsonlite::toJSON(list(action = "listen",
                                     data = list(streams = .s)), auto_unbox = TRUE)
    ws_o$send(.listen)
  } else {
    .c <- toupper(channel)
    .sub <- ifelse(unsub, "unsubscribe", "subscribe")
    purrr::walk(.c, ~{
      .listen <- jsonlite::toJSON(list(action = .sub,
                                       params = .x), auto_unbox = TRUE)
      
      
      if (ws$env$log_bars) {
        # Create the name of the CSV log for Polygon channels
        .log_ev <- paste0(ws$env$log_path,.x,".csv")
        # if the file doesnt exist, create it
        if (!file.exists(.log_ev)) {
          file.create(.log_ev)
          write(paste0(c("ev", "sym", "v", "av", "op", "vw", "o", "c", "h", "l", "a", "z", "n", "s", "e"), collapse = ", "), file = .log_ev, append = TRUE)
        } 
      }
      ws_o$send(.listen)
    })
  }
}




#' @title Update Websocket message objects
#' @keywords internal
#' 
#' Used in ws_create to to instantiate and update websocket message objects
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
ws_log <- function(out, ..., .o = NULL, msg = NULL, penv = rlang::caller_env()) {
  
  # add the arguments to the environment ----
  # Thu Apr 30 17:29:18 2020
  .e <- try(list2env(list(penv), environment()), silent = TRUE)
  .vn <- list(.o = "data.frame", .log = "logical", out = "list", log_bars = "logical", log_msgs = "logical", log_path = "logical", logfile = "character", api = "character")
  if (!exists(msg, inherits = FALSE)) .vn$.msg <- "character"
  if (!all(.vn %in% ls(all.names = TRUE))){
    fetch_vars(.vn, ...)
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

