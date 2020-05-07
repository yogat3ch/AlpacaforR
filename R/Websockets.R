#Websockets:  Wed Mar 25 14:00:19 2020 ----


#' @family Websockets
#' @title ws_create: Create a Websocket to the Alpaca or Polygon API
#' @description The [Alpaca Streaming API](https://alpaca.markets/docs/api-documentation/api-v2/streaming/) provides Trade & Account updates, while the [Polygon Websocket API](https://polygon.io/sockets) offers Trade, Quote, Aggregate (per second) and Aggregate (per minute) streaming. 
#' @param api `(character)` The streaming API to connect to, either `"Alpaca"/"a"` or `"Polygon"/"p"`. 
#' @param log_msgs \code{(logical/character)} A logical indicating whether to use a log file for websocket messages (defaults to *"ws_{API}.log"* in the working directory if `TRUE`), where `{API}` is `a/p` depending on the API selected. Can also be the name of the text file where logs for this session will be stored. IE if you want to save all messages in `Alpaca.log` in the working directory, set `log_msgs = "Alpaca"`. If `".log"` is not included in the name it will be appended. Previous logfiles of the same name will be reused without overwrite. No logs created if `FALSE`. **Default `TRUE`**. If a path is specified to `log_path`, the logs will reside in this directory.
#' @param log_bars `(logical)` *Polygon only* Flag to indicate whether to save streaming tick data from [Polygon.io channels](https://polygon.io/sockets). Tick data is saved into a `.csv` instead of a `.log` file. The filename will be `[Channel].csv` where channel is the name of the channel supplied to the `channel` argument in \code{\link[AlpacaforR]{ws_listen}}. If a path is specified to `log_path`, the CSV will reside in this directory.
#' @param log_path `(character)` The path where the .log (for log_msgs) or .csv (for log_bars) files are to be stored. Tilde path expansion is supported, see \code{\link[base]{path.expand}}.
#' @param action `(expression)` An R expression, see \code{\link[rlang]{rlang::expr}}. This expression will be executed upon receipt of each message. Useful for setting trailing stop algorithms, or executing an order based on specific criteria. **Note** that all values and objects necessary to the function of the `action` should be defined within the expression. Calls to objects stored in the global environment are likely to cause errors. *Tidy evaluation is supported.* The relevant objects that may be referred to when writing this expression are:
#' \itemize{
#'   \item{\code{.o}}{ Data returned in message. For Polygon subscriptions, `.o` is a `data.frame`. See [Polygon - Sockets: Stock Schemas](https://polygon.io/sockets) for the properties specific to each channel.}
#'   \item{\code{out$env}}{ The environment in the `list` returned by this function.}
#' }  
#' @param aenv \code{(environment)} An environment can be supplied in which `action` will be evaluated. Useful to make variables available to the evaluation of `action` from the global or other environments to avoid overwrite/removal of these dependent variables if they reside in the current global environment. One use case: if certain functions within `action` for a Polygon websocket are triggered by messages from the Alpaca websocket. The associated `env` from the Alpaca websocket list object may be included as a part of this environment such that these functions can be triggered as intended. 
#' @param toConsole `(logical)` Whether to print messages to the console. All open/close/error status messages will be printed by default, but subscribed feed messages may be muted with this argument. Default `TRUE`.
#' @details  If log_msgs is `TRUE` or a file name is provided, the .log file will contain:
#' \itemize{
#'  \item{The time the connection is created}
#'  \item{The time and content of each message}
#'  \item{The time the connection is closed}
#' }
#' This log file can be read with \code{\link[base]{readLines}} or \code{\link[utils]{read.csv}}.
#' If `log_msgs` is FALSE, no log file will be created. This function allows a simple set-up for the majority of use cases. The connection can be closed at any time by using the `[OBJECT]$ws$close()` method, where `[OBJECT]` is the object returned from `ws_create`. If you wish to use certain message content as hooks to execute further functions specific to your Alpaca algorithm, you can do one of following: 
#' \enumerate{
#'  \item{For simple functions, use the \code{action} argument.}
#'  \item{Use a background process with \code{\link[later]{later::later}} or \code{\link[callr]{callr::r_bg}} to monitor the \code{[OBJECT]$env$lastmessage} object or the \code{[OBJECT]$env$msgs} or \code{[OBJECT]$env$bars} (Polygon) object and perform an action when a specific message appears.}
#'  \item{Build a websocket from scratch using the \href{https://rstudio.github.io/websocket/}{websocket documentation} and include the appropriate hooks and functions in the \code{onMessage} method.}
#' }
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
#' @importFrom lubridate now
#' @importFrom dplyr bind_rows
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom purrr walk accumulate imap_chr
#' @importFrom tibble tibble
#' @importFrom stringr str_split str_remove
#' @importFrom magrittr `%>%`
#' @importFrom rlang eval_tidy is_expression
#' @import websocket
#' @examples 
#' \dontrun{
#' # must be run in an interactive session
#' if (interactive()) {
#'   # Create the Alpaca Websocket and message environment. 
#'   ws <- ws_create("a")
#'   ws$ws$connect()
#'   # Create the Polygon websocket and message environment, with ticks being logged.
#'   wsp <- ws_create("p", log_bars = T)
#'   wsp$ws$connect()
#'   # if you're unable to remember which API the ws object corresponds to:
#'   attr(ws$ws, "api")
#' }
#' }
#' @export

ws_create <- function(api = c("a", "p")[1], log_msgs = T, log_bars = F, log_path = "", action = NULL, aenv = NULL, toConsole = T) {
  `%>%` <- magrittr::`%>%`
  api <- tolower(substr(api, 0, 1))
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
      write("Timestamp, Message\n", file = logfile)
    }
  }
  
  # Create the Websocket
  if (api == "a") {
    ws <- websocket::WebSocket$new(url = paste0(gsub("^https", "wss", get_url(T)),"/stream"), autoConnect = F)
  } else {
    ws <- websocket::WebSocket$new(url = "wss://socket.polygon.io/stocks", autoConnect = F)
  }
  out <- list(ws = ws, env = new.env())
  
  ws$onOpen(function(event) {
    .msg <- paste0(lubridate::now(),", ", "Connection Created\n")
    ws_msg(out, msg = .msg)
    ws_log(.log = .log, .o = .o, penv = environment()) 
  })
  ws$onMessage(function(event) {
    if (api == "a") {
      .msg <- httr:::parse_text(event$data, type = "text", encoding = "UTF-8")
    } else {
      .o <- jsonlite::fromJSON(event$data)
      .msg <- paste0(purrr::imap_chr(.o, ~{
         .msg <- paste0(.y,": ",.x)
       }), collapse = " | ")
      # If listening to a subscription channel
    }
    if (rlang::is_environment(cenv)) {
      parent.env(aenv) <- environment()
      .aenv <- aenv
    } else {
      .aenv <- environment()
    }
    
    if (!is.null(action) && rlang::is_expression(action)) rlang::eval_tidy(action, env = .aenv)
    ws_msg(out, .o = .o, msg = .msg, toConsole)
    ws_log(.log = .log, .o = .o, .msg = .msg, penv = environment())
    return(.msg)
  })
  ws$onClose(function(event) {
    print(event$code)
    .r <- c(`1000` = "closed by user", `1005` = "no status received", `1006` = "end of trading hours")[as.character(event$code)]
    
    .msg <- paste0(lubridate::now(tz = Sys.timezone()),", Websocket client disconnected with code: ", event$code,
        " and reason: ", .r, "\n")
    ws_msg(out, msg = .msg)
    ws_log(.log = .log, .msg = .msg, penv = environment())
  })
  ws$onError(function(event) {
    .msg <- paste0(lubridate::now(tz = Sys.timezone()),", Websocket Error: '", httr:::parse_text(event$message, encoding = "UTF-8"), "'\n")
    ws_msg(out, msg = .msg)
    ws_log(.log = .log, .msg = .msg, penv = environment())
  })
  attr(out$ws, "api") <- api
  return(out)
}


#' @family Websockets
#' @title ws_listen: A function to set listening streams with the Alpaca websocket API
#' @description  Given the `websocket environment` the function will first authenticate using the environment variables `APCA-LIVE-API-KEY-ID` and `APCA-LIVE-API-SECRET-KEY`. If these are unset, please see the Installation vignette `vignette("Installation", "AlpacaforR")` for instruction on setting them. Secondly, it will subscribe to any channels specified to the `channel` argument. Must be called on each Websocket object individually. 
#' @param ws \code{(list/Websocket)} The websocket list or Websocket object created with \code{\link[AlpacaforR]{ws_create}}.
#' @param channel \code{(character)} of the channel to connect to.
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
#' @return Returns nothing. All received messages are stored according to the parameters provided to \code{\link[AlpacaforR]{ws_create}}.
#' @importFrom jsonlite toJSON
#' @importFrom purrr map_chr walk
#' @examples
#'  \dontrun{
#'  # must be run in an interactive session
#'  # See ?AlpacaforR::ws_create for tutorial on creating a connection
#'  if (interactive()) {
#'    if (attr(ws$ws, "api") == "a") {
#'      # Subscribe to Alpaca trade and account updated by default
#'      ws_listen(ws)
#'    } 
#'    if (attr(wsp$ws, "api") == "p") {
#'      #Subscribe to the aggregates (per second) for Beyond Meat
#'      ws_listen(wsp, channel = "a.bynd")
#'    }
#'    #You can close the connection(s) at any point with the following:
#'    ws$ws$close()
#'    wsp$ws$close()
#'  }
#'  }
#' @export

ws_listen <- function(ws, channel = NULL, unsub = F) {
  if (is.list(ws)) ws <- ws$ws
  api <- attr(ws, "api")
  # Authorize first
  if (api == "a") {
    .auth <- jsonlite::toJSON(list(action = c("authenticate"),
                                   data = list(key_id = Sys.getenv("APCA-LIVE-API-KEY-ID"),
                                               secret_key = Sys.getenv("APCA-LIVE-API-SECRET-KEY"))), complex = "string", auto_unbox = T)
  } else {
    .auth <- jsonlite::toJSON(list(action = c("auth"),
                                   params = Sys.getenv("APCA-LIVE-API-KEY-ID")), complex = "string", auto_unbox = T)
  }
  ws$send(.auth)
  if (api == "a") {
    if (is.null(channel)) {
      .s <- list("trade_updates","account_updates")
    } else {
      .c <- tolower(substr(channel, 0, 1))
      .s <- purrr::map_chr(.c, ~purrr::pluck(list(a = "account_updates", t = "trade_updates"),.x))
      if (length(.s) == 1) .s <- list(.s)
    }
    .listen <- jsonlite::toJSON(list(action = "listen",
                                     data = list(streams = .s)), auto_unbox = T)
    ws$send(.listen)
  } else {
    .c <- toupper(channel)
    .sub <- ifelse(unsub, "unsubscribe", "subscribe")
    purrr::walk(.c, ~{
      .listen <- jsonlite::toJSON(list(action = .sub,
                                       params = .x), auto_unbox = T)
      ws$send(.listen)
    })
  }
}

