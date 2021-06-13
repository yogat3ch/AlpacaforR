#Websockets:  Wed Mar 25 14:00:19 2020 ----
.tbq <- c("trades", "bars", "quotes")



#' @export
auth.AlpacaSocket <- function(self, live) {
  .data <- stringr::str_detect(self$opts$url, "data")
  .x <- purrr::when(.data,
              . ~ c("auth", "key", "secret"),
  ~ c("authenticate", "key_id", "secret_key"))
  .auth <- jsonlite::toJSON(purrr::when(.data,
              . ~ rlang::list2(action = .x[1],
                               !!.x[2] := get_key(live),
                               !!.x[3] := get_secret(live)),
              ~ list(
                action = .x[1],
                data = rlang::list2(
                  !!.x[2] := get_key(live),
                  !!.x[3] := get_secret(live))
              )),
              complex = "string",
              auto_unbox = TRUE,
              pretty = !.data
  )
  self$send(.auth)
}

#' @export
auth.PolygonSocket <- function(self, live) {
  .auth <- jsonlite::toJSON(list(action = c("auth"),
                                 params = get_key("p")), complex = "string", auto_unbox = TRUE)
  
  self$
    send(.auth)
}

#' @title Authorize an *Socket connection
#' @description Authorizes the websocket connection passed as the first argument
#' @param self \code{(websocket)} The websocket to authorize
#' @inheritParams account

auth <- function(self, live) {
  UseMethod("auth")
}

#' @title Convert url to valid file path
#' @param url \code{(character)} url to convert to file path
url_to_path <- function(url) {
  glue::glue_data(httr::parse_url(url), "{hostname}_{stringr::str_replace(path, '/', '_')}")
}

#' @title Prepare to record websocket data
#' @description Create folders & files for recording based on inputs
#' @param write_dir \code{(character/logical)} **Default: "AlpacaStreams"** top-level directory in which to store log files. 
#' @param url \code{(character)} the url of the stream for labelling the folder in which it's logs will reside
#' @param channel \code{(character)} The name of the channel (will be used as the filename)
#' @param overwrite \code{(logical)} **Default: `FALSE`** Indicator for whether to overwrite previous log files by making new files (`TRUE`) or to leave old files (`FALSE`)
#' @param private \code{(environment)} The `*Socket` private environment
#' @keywords Internal

write_prep <- function(write_dir, url, channel, overwrite, private) {
  # Prepare for msg writing to disk
  
  if (is.character(write_dir)) {
    # check the private options to see if it's already been overwritten
    if (is.null(private$.opts[[channel]])) private$.opts[[channel]] <- list()
    overwrite <- private$.opts[[channel]]$overwrite %||% overwrite
    logfile <- file.path(write_dir, url_to_path(url), paste0(channel,".log"))
    # Create if need be
    if (overwrite || !file.exists(logfile)) {
      # if directory paths are provided, create them if need be
      if (grepl("\\/", logfile)) {
        .paths <- stringr::str_split(logfile, "/")[[1]] %>% .[-length(.)]
        purrr::walk(purrr::accumulate(.paths, ~{
          .p <- paste0(.x,"/",.y)
        }), ~{
          if (!dir.exists(.x)) dir.create(.x)
        })
      }
      # Create channel logfile
      file.create(logfile)
      # once overwritten do not overwrite again
      private$.opts[[channel]]$overwrite <- FALSE
    }
    private$.opts[[channel]]$logfile <- logfile
  } 
  
}

#' @title Extract the socket name from class of object
#' @description \lifecycle{stable} Determines the socket from the class of the object
#' @param self \code{(*Socket)} A socket object
#' @return \code{(character)} the name of the socket (`"Alpaca"/"Polygon"`)
#' @keywords Internal

class_prefix <- function(self) {
  na.omit(stringr::str_extract(class(self)[1], "(?:Alpaca)|(?:Polygon)"))
}

#' @title The socket name formatted for display on the console
#' @description Use colors specific to each websocket for easy visual discernment of which socket is reporting to the console
#' @param msg \code{(msg/tbl)} The `msg` tibble object returned from \href{../../AlpacaforR/html/msg.html}{\code{msg()}} which consists of the following:
#' \itemize{
#'   \item{\code{ts}}{ \code{(POSIXct)} timestamp}
#'   \item{\code{socket}}{ \code{(character)} the socket over which the message arrived}
#'   \item{\code{channel}}{ \code{(character)} the channel over which the message arrived}
#'   \item{\code{...}}{ \code{(varies)} all fields encoded in the event data JSON}
#' }
#' @return \code{(character)} prefix for console display
#' @keywords Internal

console_prefix <- function(msg) {
  .fn <- switch(unique(msg$socket),
         Alpaca = crayon::yellow,
         Polygon = crayon::magenta)
  rlang::exec(.fn, msg$socket, ": ")
}

#' @export
event_data.character <- function(., msg) jsonlite::fromJSON(.)

#' @export
event_data.raw <- function(., msg) {
  out <- jsonlite::fromJSON(paste0(readBin(., character(), n = 10), collapse = ""))
  if (purrr::vec_depth(out) > 2) out <- unlist(out, recursive = FALSE)
}

#' @export
event_data.default <- function(., msg) msg

#' @title Extract event data
#' @description Extract message data from `event` list object
#' @param . \code{(event$data)} object returned by the websocket
#' @inheritParams console_prefix
#' @return \code{(event data/data.frame)} Returns event data that is typically a `tbl/data.frame`
#' @keywords Internal

event_data <- function(., msg) {
  UseMethod("event_data")
}


#' @export
event_channel.data.frame <- function(msg)
{
  .type <- msg$ev %||% msg$`T`%||% msg$stream
  .sym <- msg$sym %||% msg$S
  if (suppressWarnings(grepl("^T|^B|^Q|^A|(?:^AM)", .type, ignore.case = TRUE))) 
    channel <- paste0(.type,".",.sym)
  else if (isTRUE(.type %in% c("status", "success", "error", "subscription", "authorization", "listening")))
    channel <- "status"
  else
    channel <- .type
}

#' @export
event_channel.default <- function(msg) "status"

#' @title Get the event channel
#' @description Given a `msg` object return the channel from which it came
#' @inheritParams console_prefix
#' @return \code{(character)} the channel
#' @keywords Internal

event_channel <- function(msg) UseMethod("event_channel")


#' @title Create a message (`msg`) from an event
#' @param event \code{(list)} sole argument to the websocket `onMessage` callback
#' @param msg \code{(character)} A message to be used if the event has no corresponding message
#' @param channel \code{(character)} a default for channel if it cannot be assumed from the event. See `\link[AlpacaforR]{event_channel}`
#' @keywords Internal

msg <- function(event, msg = NULL, channel = NULL) {
  # Code here to extract socket name
  .socket <- class_prefix(event$target)
  msg <- event_data(event$data, msg)
  channel <- channel %||% event_channel(msg)
  browser(expr = isTRUE(msg$code == 400))
  structure(
    do.call(tibble::tibble_row, purrr::compact(rlang::list2(
    ts = Sys.time(),
    socket = .socket,
    channel = channel,
    !!!purrr::map_if(msg, ~length(.x) > 1, ~list(.x))
    ))),
    class = c("msg", "tbl_df", "tbl", "data.frame")
  )
  
}



#' @title  Determine logical precedence
#' @description return a single logical from a series of logical values in order of precedence (specific to default)
#' @param ... logical values in order of precedence
#' @return \code{(logical)}
#' @keywords Internal

lgl_pcdnce <- function(...) {
  .d <- rlang::dots_list(...)
  if (...length() == 1) .d <- unlist(.d, recursive = FALSE)
  # if highest precedence is TRUE, return TRU
  if (isTRUE(.d[[1]])) TRUE
  if (purrr::some(.d, isTRUE)) {# If some true
    # filter NULLS
    .d <- .d[- which(purrr::map_lgl(.d, is.null))]
    # false indices
    .f <- which(purrr::map_lgl(.d, isFALSE))
    # true indices
    .t <- which(purrr::map_lgl(.d, isTRUE))
    # if first true precedes first false then TRUE
    .t[1] < .f[1]
  } else {
    FALSE
  }
}

#' @title Send a `msg` to the console
#' @inheritParams console_prefix
#' @param opts \code{(list)} The list of options from the *Socket object. Used to determine whether the object should be sent to console.
#' @keywords Internal

console_msg = function(msg, opts) {
  # if console logging, log
  if (opts[[unique(msg$channel)]]$toConsole %||% opts$toConsole) {
    cli::cli_text(
      crayon::silver(crayon::underline(unique(msg$ts)), " - "),
      console_prefix(msg)
    )
    if (suppressWarnings(inherits(msg$msg, "ansi_string")))
      msg$msg
    else 
      print(msg[-c(1:2)])
    
    
  }
}

#' @title message logging
#' @description Logs message based on conditions supplied as options to *Socket object.
#' @inheritParams console_prefix
#' @inheritParams console_msg
#' @inheritParams write_prep
#' @keywords Internal
 
log_msg <- function(msg, opts, private) {
  .channel <- unique(msg$channel)
  if (length(.channel) > 1) rlang::abort("Channel must be length 1", trace = rlang::trace_back())
  # if logging, log messages & bars
  if (opts[[.channel]]$log %||% opts$log) {
    # log bars
    if (suppressWarnings(isTRUE(msg$ev %in% c("T", "Q", "A", "AM")))) {
      .bars <- dplyr::bind_rows(private$.bars[[.channel]], msg[!names(msg) %in% c("socket","channel","ev", "sym")])
      if (NROW(.bars) > opts[[.channel]]$bars_limit %||% opts$bars_limit) {
        if (private$bars_warn) {
          cli::cli_text(cli::col_red("Bars limit reached, old bars will be deleted."))
          private$bars_warn <- FALSE
        } 
        .bars <- .bars[-1,]
      }
      private$.bars[[.channel]] <- .bars
    } else {
      # log messages
      .log <- dplyr::bind_rows(private$.log[[.channel]], msg[!names(msg) %in% "socket"])
      # if the object reaches the limit
      if (NROW(.log) > opts[[.channel]]$log_limit %||% opts$log_limit) {
        if (private$limit_warn) {
          cli::cli_text(cli::col_red("Message limit reached, old messages will be deleted."))
          private$limit_warn <- FALSE
        } 
        .log <- .log[-1,]
      } 
      private$.log[[.channel]] <- .log
    }
  }
    # Memory bailout 
    # Wed Jan 20 15:16:26 2021
  if(utils::memory.size() / utils::memory.limit() > .9) {
    if (utils::memory.size() / utils::memory.limit() > .98) stop("Memory at 98% capacity, stopping websocket.")
    # half it's size by removing the first half
    warning(paste0("Memory at 90% capacity, websocket logging will error at 98%"))
  }
}

#' @title Write messages to disk 
#' @description Write messages based on conditions supplied as options to *Socket object (stored as logfile in private fields).
#' @inheritParams console_prefix
#' @inheritParams write_prep
#' @keywords Internal

write_msg <- function(msg, private) {
  .channel <- msg$stream %||% msg$channel
  .logfile <- private$.opts[[.channel]]$logfile
  if (is.character(.logfile)) {
    # remove superfluous columns if recording bars
    if (isTRUE(suppressWarnings(isTRUE(msg$ev %in% c("T", "Q", "A", "AM")))))
      msg <- msg[!names(msg) %in% c("channel", "socket", "ev")]
    
    if (private$.opts[[.channel]]$do_label %||% TRUE) {
      .head <- readLines(.logfile)
      write(c(paste0(names(msg), collapse = ", "), .head),
            .logfile,
            sep = "\n", ncolumns = length(msg))
      private$.opts[[.channel]]$do_label <- FALSE
    }
    # write to file
    write(paste0(msg, collapse = ", "), file = .logfile, append = TRUE, ncolumns = length(msg))
  }
}


channel_names <- function(channel) {
  unlist(purrr::imap(channel, ~paste0(purrr::when(.y %in% .tbq, . ~ paste0(substr(.y, 0 ,1),"."), ~ NULL), .x)))
}

channel_set <- function(channel, socket) {
  # Set channel if unspecified
  if (socket == "Alpaca") {
    if (is.null(channel)) 
      .s <- list("trade_updates", "account_updates")
    else if (!any(.tbq %in% names(channel))) {
      .s <- as.list(match.arg(
        tolower(channel),
        c(a = "account_updates", t = "trade_updates"),
        several.ok = TRUE
      ))
    } else {
      if (!any(.tbq %in% names(channel))) rlang::abort("Check syntax of `channel` argument.\nSee ?AlpacaStreams.")
      .s <- channel
    }
  } else if (socket == "Polygon") {
    if (is.null(channel)) rlang::abort("Polygon requires a channel to be specified.\nSee ?AlpacaStreams") 
    .s <- toupper(channel)
  } 
 .s 
}



#' @title Determine the socket from the channel
#' @param channel \code{(character)} Name of the channel
#' @return \code{(character)} socket name based on channel

socket_detect <- function(channel) {
  # Detect socket based on channel
  purrr::when(channel,
              inherits(., "list") ~ "Alpaca", 
              stringr::str_detect(., "^\\w{1,2}\\.") ~ "Polygon",
              ~ "Alpaca"
              )
}

channel_bars <- function(channel) {
  purrr::when(channel,
              is.null(.) ~ FALSE,
              stringr::str_detect(., "^\\w{1,2}\\.") ~ TRUE)
}

channel_get <- function(channel, private) {
  channel <- toupper(channel)
  if (channel_bars(channel))
    tryCatch(
      private$.bars[[match.arg(channel, names(private$.bars))]],
      error = rlang::as_function(~{
        rlang::warn(paste0(channel, " not found, returning all.\n"))
        private$.bars
      }))
  else
    tryCatch(
      private$.log[[match.arg(channel, names(private$.log))]],
      error = rlang::as_function(~{
        rlang::warn(paste0(channel, " not found, returning all."))
        private$.log
      }))
  
  
  
}

# Account information:
#   'trade_updates' connecting to wss://api.alpaca.markets/v2/stream (for ones live account)
# 'trade_updates' connecting to wss://paper-api.alpaca.markets/v2/stream (for ones paper account)
# Market Data from Alpaca:
#   wss://data.alpaca.markets/stream IEX data available to all subscribers. This is v1 included for backward compatibility.
# wss://stream.data.alpaca.markets/v2/iex IEX data available to the free data plan subscribers.
# wss://stream.data.alpaca.markets/v2/sip Full market 'SIP' data available to the Pro data plan subscribers.

#  AlpacaSocket ----
# Tue Jan 19 11:23:12 2021
#' @title AlpacaSocket
#' @family AlpacaStreams
#' @description A \link[websocket]{WebSocket} with modifications for Alpaca/Polygon websocket streams. \lifecycle{experimental}


AlpacaSocket <- R6::R6Class(
  "AlpacaSocket",
  inherit = websocket::WebSocket,
  public = list(
    #' @description Connect to an Alpaca websocket 
    #' @param opts passed from  \link[AlpacaforR]{AlpacaStreams}
    #' @param ... passed on to \link[websocket]{WebSocket}
    initialize = function(opts, ...) {
      rlang::exec(super$initialize, ...)
      self$opts <- opts
      write_prep(
        write_dir = opts$write_dir,
        url = rlang::dots_list(...)$url,
        channel = "status",
        overwrite = opts$overwrite,
        private = private
      )
      
      
      super$onMessage(function(event) {
        .msg <- msg(event)
        private$message_handle(.msg)
        .user_quo <- self$opts[[.msg$channel]]$msg_action %||% self$opts$msg_action %||% FALSE
        if (!is.null(rlang::quo_expr(.user_quo))) {
          .the_expr <- eval(rlang::get_expr(.user_quo))
          # create a temp eval env with the necessary objects
          .the_env <- rlang::env(attr(.user_quo, ".Environment"), msg = .msg, self = self)
          # eval the expression in that environment
          rlang::eval_bare(.the_expr, env = .the_env)
          # grab the variables supplied by the user that ended up in the evaluation environment from the parent by negating the developer supplied variables from the `ls` results 
          .vars <- rlang::env_get_list(.the_env, stringr::str_subset(ls(.the_env), "(?:msg)|(?:self)", negate = TRUE), default = NULL)
          
          # add the env from the quosure to reassign them to
          .vars$.env <- attr(.user_quo, ".Environment")
          # reassign them
          rlang::exec(rlang::env_bind, !!!.vars)
        }
          
          
      })
      
      super$onOpen(function(event) {
        if (class_prefix(event$target) == "Alpaca") {
          
          .msg <- msg(event,
                      purrr::when(
                        self$readyState() == 1,
                        . ~ list(message = "Connected Successfully"),
                        ~ list(message = "Connection Unsuccessful")
                      ),
                      channel = "status")
          private$message_handle(.msg)
        }
      })
      
      super$onClose(function(event) {
        .r <- switch(
          as.character(event$code),
          `1000` = "closed by user",
          `1005` = "no status received",
          `1006` = "end of broadcast"
        )
        .msg <-
          list(error = paste0(
            "Code: ",
            event$code,
            "\nDescription: ",
            .r
          ))
        .msg <- msg(event, .msg, channel = "status")
        private$message_handle(.msg)
      })
      
      super$onError(function(event) {
        .msg <- cli::col_red(msg(event))
        private$message_handle(.msg, channel = "status")
      })
      self$connect()
    },
    #' @field opts Options supplied to `AlpacaStreams$new()` are stored here. Channel specific streams are stored in nested lists with the channel/stream name. All of these options can be altered after the websocket has been activated, with the exception of `write_dir`.
    opts = NULL,
    #' @description Returns the last message received by the socket
    lastmessage = function() {
      private$.lastmessage
    },
    #' @description Returns the message log
    #' @param channel \code{(character)} Channel name of logs to be returned. Use `"status"` to get all status messages for the socket.
    logs = function(channel) {
      .socket <- class_prefix(self)
      .ch <- missing(channel)
      
      purrr::when(.socket,
                  . == "Polygon" && .ch ~ list(log = private$.log, bars = private$.bars),
                  . == "Polygon" && !.ch ~ channel_get(channel, private),
                  . == "Alpaca" && .ch ~ list(log = private$.log, bars = private$.bars),
                  . == "Alpaca" && !.ch ~ channel_get(channel, private))
    },
    #' @description Sends request to websocket to join the specified channel. Channel options are inherited from defaults set in \code{\link[AlpacaforR]{AlpacaStreams}$initialize} unless overridden. **Notes** 1. the Polygon website is only available to Polygon subscribers. 2. The `sip` data stream is only available to Alpaca PRO subscribers.
    #' @param channel The channel to join. 
#' \itemize{
#'   \item{For Alpaca v1: \code{(character)} vector of symbols}
#'   \item{For Alpaca Data v2: \code{(named list)} Items with names `trades`, `quotes`, `bars` are accepted. See the \href{Alpaca v2 Data Streaming websocket docs}{https://alpaca.markets/docs/api-documentation/api-v2/market-data/alpaca-data-api-v2/real-time#subscribe} for details.}
#'   \item{For Polygon: \code{(character)} vector of channels formatted according to the \href{https://polygon.io/docs/websockets/subscribe}{websocket documentation}.}
#' }
    #' @param subscribe \code{(logical)} **Default `TRUE`**. Set to `FALSE` to unsubscribe from specified channel(s).
    #' @param overwrite \code{(logical)} **Default** `FALSE`. Set to `TRUE` to overwrite data from previous instances of this channel socket.
    #' @param msg_action \code{(expression)} An expression that performs a user-specified action on the receipt of websocket message. These act on the `msg` object seen printed to the console when a message is received (if `toConsole = TRUE`). The `msg` object also contains a `$ts` column with the timestamp as a `POSIXct` and a `$socket` column with the socket name of origin (`"Alpaca"/"Polygon"`) that are not visible in what is printed to the console but accessible to `msg_action`. The expression can also reference the `self` internal environment of this \code{\link[R6]{R6Class}}.
    #' @param ... Named arguments from \code{AlpacaStreams$new()}
    #' @details 

    #' If `log = TRUE` for a Polygon channel, all non-status messages will be stored as tibbles named according to their respective channel name and accessed via the `$logs()` method.
    
    channel = function(
      channel = NULL,
      subscribe = TRUE,
      overwrite = FALSE,
      msg_action = NULL,
      ...
    ) {
  
      socket <- socket_detect(channel)
      # Save options
      .d <- rlang::dots_list(...)
      .d$msg_action <- msg_action
      purrr::walk(channel_names(channel), ~ {
        self$opts[[.x]] <- do.call(purrr::list_modify,
                      rlang::list2(self$opts,
                                   !!!.d,
                                    overwrite = overwrite))
        
        write_prep(
          write_dir = self$opts[[.x]]$write_dir,
          channel = .x,
          url = self$opts$url,
          overwrite = overwrite,
          private = private
        )
      })
      # Create object
      if (socket == "Alpaca" && !any(.tbq %in% names(channel))) {
        .listen <- list(jsonlite::toJSON(list(
          action = "listen",
          data = list(streams = channel))
          , auto_unbox = TRUE,
          pretty = TRUE
        ))
      } else if (socket == "Alpaca" && any(.tbq %in% names(channel))) {
        .listen <- list(jsonlite::toJSON(rlang::list2(
          action = ifelse(subscribe, "subscribe", "unsubscribe"),
          !!!purrr::map(channel, ~ purrr::when(length(.x) < 2, isTRUE(.) ~ list(.x), ~ .x))),
          auto_unbox = TRUE
        ))
      } else {
        .listen <- purrr::map(toupper(channel), ~ jsonlite::toJSON(list(
          action = ifelse(subscribe, "subscribe", "unsubscribe"),
          params = .x
        ), 
        auto_unbox = TRUE))
      }
      # Send
      purrr::walk(.listen, ~self$send(.x))
      
      
    },
    #' @description Authorize the stream (this is done automatically when initializing via the `AlpacaStreams` class.)
    auth = function() auth(self)
  ),
  private = list(
    limit_warn = TRUE,
    .log = list(),
    .opts = list(),
    .lastmessage = NULL,
    finalize = function() {
      self$close()
      super$close()
    },
    message_handle = function(msg) {
      # overwrite .lastmessage
      private$.lastmessage <- msg
      # to console, if requested
      console_msg(msg, self$opts)
      # log msg history, if requested
      log_msg(msg, self$opts, private = private)
      # write msg history, if requested
      write_msg(msg, private) 
    },
    .bars = list()
  )
)



#' @title R6 object for Polygon websocket (identical to AlpacaSocket)
#' @rdname AlpacaSocket
#' @export

PolygonSocket <- R6::R6Class(
  "PolygonSocket",
  inherit = AlpacaSocket
)

#' @title Alpaca Websocket Streaming
#' @description An `R6` class that allows interaction with \href{https://alpaca.markets/docs/api-documentation/api-v2/streaming/}{Alpaca} and \href{https://polygon.io/sockets}{Polygon} websockets. 
#' \lifecycle{experimental}
#' @details 
#' # Channels
#' Channels are joined by calling the `$channel()` method. The socket is automatically detected based on the channel request.
#' ## [Available Alpaca Channels](https://alpaca.markets/docs/api-documentation/api-v2):
#' \itemize{
#'  \item{\code{"account"/"a"}}{ [Alpaca account stream](https://alpaca.markets/docs/api-documentation/api-v2)}
#'  \item{\code{"trade"/"t"}}{ [Alpaca trade stream](https://alpaca.markets/docs/api-documentation/api-v2/streaming#order-updates)}
#'   \item{\code{All V2 streaming data channels}}{ See the \href{https://alpaca.markets/docs/api-documentation/api-v2/market-data/alpaca-data-api-v2/real-time/#subscribe}{V2 Streaming Documentation} for details.}
#' }
#' ## [Available Polygon Channels](https://polygon.io/sockets):
#' \itemize{
#'  \item{T.* Trades}
#'  \item{Q.* Quotes}
#'  \item{A.* Aggregate ( per second )}
#'  \item{AM.* Aggregate ( per minute )}
#' }
#' where * is the ticker symbol and the argument is case-insensitive. IE `"am.bynd"` will subscribe to by minute aggregates for Beyond Meat. Multiple channels can be subscribed to using a character vector.
#' ### Channel Data Format
#' Messages returned by the Polygon Websocket depend on the channel:
#' ### Trade
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
#' ### Quote
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
#' ### Agg
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
#' \dontrun{
#' AS <- AlpacaStreams$new()
#' AS$channel()
#' # Subscribe to by-minute data for Beyond Meat Inc
#' AS$channel(list(bars = "BYND"))
#' # Turn off console messages
#' AS$Alpaca$data$opts$b.BYND$toConsole <- FALSE
#' }
#' @export

#  AlpacaStreams ----
# Tue May 18 17:14:10 2021
AlpacaStreams <- R6::R6Class(
  "AlpacaStreams",
  public = list(
    #' @description Connect to and authorize Alpaca and Polygon websocket streams and set default options for these websockets.
    #' @param socket \code{(character)} Which Websocket stream to connect to - either `"alpaca"` or `"polygon"` (or an abbreviation thereof). **Default** connects to Alpaca.
    #' @param v \code{(integer)} Which version of the API to connect to. Either `1` or `2`.
    #' @param pro \code{(logical)} Whether to use a Pro account. **Default** `FALSE`
    #' @param toConsole \code{(logical)} flag for whether to report websocket messages to the console. **Default** `TRUE`.
    #' @param log \code{(logical)} flag for whether to retain a log of messages (from Alpaca) or messages & bars from symbol feeds from Polygon. **Default** `TRUE` to store logs.
    #' @param log_limit \code{(numeric)} indicating the number of previous messages to retain in the log. **Default** `5000`. See Details for memory handling specifics.
    #' @param bars_limit \code{(numeric)} indicating the number of previous bars (per subscription feed) to retain in the log. **Default** `5000`. See Details for memory handling specifics.
    #' @param write_dir \code{(character/logical)} The directory in which to store logs on disk. Use `FALSE` to disable logging to disk. Folders will be created. **Default** `"AlpacaStreams"`.
    #' @param overwrite \code{(logical)} indicating whether to overwrite data from previous instances of a websocket connection. **Default** `TRUE`.
    #' @param msg_action \code{(expression)} An expression that performs a user-specified action on the receipt of websocket message. These act on the `msg` object seen printed to the console when a message is received (if `toConsole = TRUE`). The `msg` object also contains a `$ts` column with the timestamp as a `POSIXct` and a `$socket` column with the socket name of origin (`"Alpaca"/"Polygon"`) that are not visible in what is printed to the console but accessible to `msg_action`. The expression can also reference the `self` internal environment of this \code{\link[R6]{R6Class}}. 
    
    #' @param live See [market_data]
    #' @param ... Passed on to \link[websocket]{WebSocket}
     
     initialize = function(socket = c("account_alpaca", "data_alpaca", "polygon")[1:2],
                          toConsole = TRUE,
                          log = TRUE,
                          log_limit = 5000,
                          bars_limit = 5000,
                          write_dir = "AlpacaStreams",
                          overwrite = TRUE,
                          msg_action = NULL,
                          live = get_live(),
                          ...) {
      
      socket <- match_letters(socket, "account_alpaca", "data_alpaca", "polygon", multiple = TRUE)
      
      # Set default opts
      .opts <- ls() 
      .opts <- rlang::env_get_list(nms = .opts[!.opts %in% c("self", "socket", "msg_action")], default = NULL)
      .opts$msg_action <- rlang::enquo(msg_action)
      
      if (any("Polygon" %in% socket)) {
        self$Polygon <- PolygonSocket$new(
          opts = .opts,
          url = get_url(poly = TRUE, api = "ws"),
          autoConnect = FALSE,
          ...
        )
        
      }
      
      # fetch subscription
      pro <- as.logical(get_cred("APCA-PRO"))
      if (any(grepl("alpaca", socket))) {
        .dots <- rlang::dots_list(...)
        wss <- purrr::compact(list(
          account_activities = if ("account_alpaca" %in% socket) get_url(v = 2, api = "ws", live = live, path = "stream"),
          data = if ("data_alpaca" %in% socket) get_url(ifelse(pro, "sip", "iex"), v = 2, api = "ws", data = TRUE, live = live)
        ))
        self$Alpaca <-
          purrr::map(wss, ~ {
            .opts$url <- .x
            eval(rlang::call2(
              AlpacaSocket$new,
              url = .x,
              opts = .opts,
              autoConnect = FALSE,
              !!!.dots
            ))
          })
        
      }

      on.exit({
        Sys.sleep(2)
        if (!is.null(self$Alpaca)) purrr::walk(self$Alpaca, ~auth(.x, live))
        if (!is.null(self$Polygon)) auth(self$Polygon)
      })
    },
    #' @description Sends request to websocket to join the specified channel. Channel options are inherited from defaults set in  \code{\link[AlpacaforR]{AlpacaStreams}$initialize} unless overridden.
    #' @param channel The channel to join
    #' @param subscribe \code{(logical)} **Default `TRUE`**. Set to `FALSE` to unsubscribe from specified channel(s).
    #' @param overwrite \code{(logical)} **Default** `FALSE`. Set to `TRUE` to overwrite data from previous instances of this channel socket.
    #' @param msg_action \code{(expression)} An expression that performs a user-specified action on the receipt of websocket message. These act on the `msg` object seen printed to the console when a message is received (if `toConsole = TRUE`). The `msg` object also contains a `$ts` column with the timestamp as a `POSIXct` and a `$socket` column with the socket name of origin (`"Alpaca"/"Polygon"`) that are not visible in what is printed to the console but accessible to `msg_action`. The expression can also reference the `self` internal environment of this \code{\link[R6]{R6Class}}.
    #' @param ... Named parameters of \code{AlpacaStreams$new()} to be applied to this particular channel.
    #' @details If `log = TRUE` for a Polygon channel, all non-status messages will be stored as tibbles named according to their respective channel name and accessed via the `$logs()` method.
    #' @export 
    channel = function(channel = NULL,
                       subscribe = TRUE,
                       overwrite = FALSE,
                       msg_action,
                       ...
    ) {
      # set the channel
      socket <- socket_detect(channel)
      if (!missing(msg_action)) msg_action <- rlang::enquo(msg_action)
      else
        msg_action <- NULL
      
      .list <- rlang::dots_list()
      # Detect socket based on channel
      ws <- purrr::when(socket,
                  . == "Polygon" ~ self$Polygon,
                  . != "Polygon" && (is.null(channel) || !any(.tbq %in% names(channel))) ~ self$Alpaca$account_activities,
                  ~ self$Alpaca$data)
      rlang::exec(ws$channel, channel = channel, subscribe = subscribe, overwrite = overwrite, msg_action, !!!.list)
      
        
    },
    #' @description Retrieve *Socket Logs/Bars
    #' @param channel \code{(character)} The name of the channel for which to retrieve logs. The socket is auto-filled if a matching channel is available. If blank, all logs are returned.
    #' @export
    logs = function(channel) {
      if (missing(channel)) {
        out <- list()
        if (!is.null(self$Alpaca))
          out$Alpaca <- purrr::map(self$Alpaca, ~.x$logs())
        if (!is.null(self$Polygon))
          out$Polygon <- self$Polygon$logs()
      } else {
        socket <- socket_detect(channel)
        ind <- list(socket)
        # If it's a bars channel and the socket is Alpaca
        if (channel_bars(channel) && socket == "Alpaca")
          ind <- append(ind, "data")
        ind <- append(ind, channel)
        out <- rlang::exec(purrr::pluck, !!!ind)$logs()
      }
      out
    },
    #' @field Alpaca slot for \href{../../AlpacaforR/html/AlpacaSocket.html}{\code{AlpacaSocket}}. Contains a socket for the v1 account websocket and the v2 data websocket.
    Alpaca = NULL,
    #' @field Polygon slot for \href{../../AlpacaforR/html/PolygonSocket.html}{\code{PolygonSocket}}
    Polygon = NULL,
    #' @description Connects both \href{../../AlpacaforR/html/AlpacaSocket.html}{\code{AlpacaSocket}} & \href{../../AlpacaforR/html/PolygonSocket.html}{\code{PolygonSocket}} to their respective server-side sockets.
    #' @param ... named arguments passed on to individual socket initialization methods. see \href{../../AlpacaforR/html/AlpacaSocket.html}{\code{AlpacaSocket}}
    connect = function(...) self$initialize(...),
    #' @description Closes connections for both \href{../../AlpacaforR/html/AlpacaSocket.html}{\code{AlpacaSocket}} & \href{../../AlpacaforR/html/PolygonSocket.html}{\code{PolygonSocket}}.
    close = function() {
      if (!is.null(self$Alpaca)) purrr::walk(self$Alpaca, ~.x$close())
      if (!is.null(self$Polygon)) self$Polygon$close()
    }
  ),
  private = list(
    finalize = function() {
      if (!is.null(self$Alpaca))
        if (any(purrr::map_lgl(self$Alpaca, ~.x$readyState() == 1))) purrr::walk(self$Alpaca, ~.x$close())
      if (!is.null(self$Polygon)) self$Polygon$close()
    }
  )
)








