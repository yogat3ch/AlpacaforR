#Websockets:  Wed Mar 25 14:00:19 2020 ----
#' ws_create: Create a Websocket for the Alpaca API
#' 
#' Alpaca has a websocket protocol built into the API that can be accessed at wss://api.alpaca.markets/stream. The full documentation is here: \href{https://alpaca.markets/docs/api-documentation/api-v2/streaming/}{AlpacaDOCS: Streaming}
#' @param ws_name \code{(string)} The function will assign a list object `{ws_name}_msgs` to the environment specified by `env` with a log of websocket messages and their timestamps with prefix `ws_name`. The default is "ws". With the default, the object will be `ws_msgs`.
#' @param logfile \code{(character)} The path to and name of the text file where logs for all websocket sessions will be stored. Defaults to *"ws.log"* in the working directory.
#' @param env \code{(environment)} An environment in which to store the `{ws_name}_msgs` websocket message list, and the `.lastmessage` object. Defaults to the **global environment**.
#' @details The function allows a simple set-up for the majority of use cases. If you wish to use certain messages content as hooks to execute further functions specific to your Alpaca algorithm, you can do one of two things. 
#' \enumerate{
#'  \item{Build a websocket from scratch using the \href{https://rstudio.github.io/websocket/}{websocket documentation} and inclue the appropriate hooks and functions into the `onMessage` method.}
#'  \item{Use a background process with \code{\link[later]{later}} or \code{\link[callr]{r_bg}} to monitor the `.lastmessage` object or the `{ws_name}_msgs` object and perform an action when a specific message appears.}
#' }
#' @return `websocket environment` \code{(environment)} The websocket environment object.
#' @return `{ws_name}_msgs` \code{(list)} **Note** The message list object is *returned indirectly, assigned to the environment passed to the `env` argument*, which defaults to the global environment if unset.
#' @return `.lastmessage` \code{(character)} **Note** An invisible object *assigned to the `env` environment* that can be called explicitly (sending it's name to the console) to return the last message received from the websocket. If an environment was specified this would be called via `ENVIRONMENT_NAME$.lastmessage` otherwise running `.lastmessage` will provide it. 
#' @return `logfile` A file is created in the working directory with the name supplied as the `logfile` argument with a log for this and all future websocket sessions with the same `logfile` name that logs:
#' \itemize{
#'  \item{The time the connection is created}
#'  \item{The time and content of each message}
#'  \item{The time the connection is closed}
#' }
#' @importFrom lubridate now
#' @import websocket
#' @importFrom httr parse_text
#' @export

ws_create <- function(ws_name = "ws", logfile = "ws.log", env = .GlobalEnv) {
  if (!interactive()) stop("ws_create only runs properly in an interactive session")
  ws <- websocket::WebSocket$new(url = paste0(gsub("^https", "wss", get_url(T)),"/stream"),
                      autoConnect = F)
  ws$onOpen(function(event) {
    cat("Connection opened\n")
    fs::file_create(logfile)
    if (exists(".lastmessage", env)) rm(list = ".lastmessage", envir = env)
    .msg <- paste0(lubridate::now(),": ", "Connection Opened")
    write(.msg, file = logfile, append = T)
    assign(".lastmessage", .msg, env)
    assign(paste0(ws_name,"_msgs"), list(list(ts = lubridate::now(), msg = "Connection Opened")), env)
  })
  ws$onMessage(function(event) {
    .msg <- httr:::parse_text(event$data, encoding = "UTF-8")
    cat("Message: ", .msg, "\n")
    if (exists(".lastmessage", env)) rm(list = ".lastmessage", envir = env)
    assign(".lastmessage", .msg, env)
    wsmsg <- get(paste0(ws_name,"_msgs"), env)
    wsmsg[[{length(wsmsg) + 1}]] <- list(ts = lubridate::now(tz = Sys.timezone()), msg = .msg)
    assign(paste0(ws_name,"_msgs"), wsmsg, env)
    write(paste0(lubridate::now(),": ", .msg), file = logfile, append = T)
    return(.msg)
  })
  ws$onClose(function(event) {
    cat("Client disconnected with code ", event$code,
        " and reason ", event$reason, "\n", sep = "")
    write(paste0(lubridate::now(),": ", "Connection Closed"), file = logfile, append = T)
  })
  ws$onError(function(event) {
    .msg <- httr:::parse_text(event$message, encoding = "UTF-8")
    cat("Client failed to connect: ", .msg, "\n")
    wsmsg <- get(paste0(ws_name,"_msgs"), env)
    wsmsg[[{length(wsmsg) + 1}]] <- list(ts = lubridate::now(tz = Sys.timezone()), msg = .msg)
    assign(paste0(ws_name,"_msgs"), wsmsg, env)
    write(paste0(lubridate::now(),": ERROR - ", .msg), file = logfile, append = T)
  })
  
  
  ws$connect()
  return(ws)
}

#' ws_auth: A function to authenticate a websocket with the Alpaca API
#' 
#' Given the `websocket environment` the function will authenticate using the environment variables `APCA-LIVE-API-KEY-ID` and `APCA-LIVE-API-SECRET-KEY`. If these are unset, please see the \href{https://github.com/jagg19/AlpacaforR}{documentation} for instruction on setting them.
#' @param `ws` \code{(environment)} The websocket environment created with \link{`ws_create`}.
#' @return Returns nothing. Provided the websocket is properly connected by calling \code{\link{ws_create}}, the console will print the server response message outlined in the \href{https://alpaca.markets/docs/api-documentation/api-v2/streaming/#authentication}{websocket documentation}.
#' @importFrom jsonlite toJSON
#' @export

ws_auth <- function(ws) {
  .auth <- jsonlite::toJSON(list(action = c("authenticate"),
                                 data = list(key_id = Sys.getenv("APCA-LIVE-API-KEY-ID"),
                                             secret_key = Sys.getenv("APCA-LIVE-API-SECRET-KEY"))), complex = "string", auto_unbox = T)
  ws$send(.auth)
}

#' ws_listen: A function to set listening streams with the Alpaca websocket API
#' 
#' Given the `websocket environment` the function will listen to the specified websocket streams.
#' @param `ws` \code{(environment)} The websocket environment created with \link{`ws_create`}.
#' @param `trade` \code{logical} indicating whether to connect to the trade stream. Defaults to TRUE.
#' @param `account` \code{logical} indicating whether to connect to the account stream. Defaults to TRUE.
#' @return Returns nothing. Provided the websocket is properly connected, the console will print the server response message outlined in the \href{https://alpaca.markets/docs/api-documentation/api-v2/streaming/#order-updates}{websocket documentation}.
#' @importFrom jsonlite toJSON
#' @export
ws_listen <- function(ws, trade = T, account = T) {
  .listen <- jsonlite::toJSON(list(action = "listen",
                                   data = list(streams = list(ifelse(trade, "trade_updates", NULL), ifelse(account,"account_updates", NULL)))), auto_unbox = T)
  ws$send(.listen)
}

