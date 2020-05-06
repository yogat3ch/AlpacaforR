#' @title First Run Credential Setup
#' @description This function helps set-up your .Renviron file to work with `AlpacaforR`. It's for first time users and only needs to be run once.
#' @param paper \code{(named character)} of length two: `key` and `secret` for the Alpaca paper API.
#' @param live \code{(named chacter)} of length two: `key` and `secret` for the Alpaca live API.
#' @return Sets environment variables for this session and edits the .Renviron file in R's home directory `path.expand("~")` and adds the specified key/secret combinations for future sessions.
#' @importFrom purrr iwalk
#' @importFrom glue glue
#' @examples 
#' \dontrun{
#' firstrun(paper = c(key = "mypaperkey", secret = "mypapersecret"), live = c(key = "mylivekey", secret = "mylivesecret"))
#' }

firstrun <- function(paper, live) {
  purrr::iwalk(list(PAPER = paper, LIVE = live), ~{
    if (is.list(.x)) {
      .k <- .x$k
      .s <- .x$s
    } else {
      .k <- .x[grep("^k", names(.x))]
      .s <- .x[grep("^s", names(.x))]
    }
    Sys.setenv(glue::glue("APCA-{.y}-API-KEY-ID"), .k)
    Sys.setenv(glue::glue("APCA-{.y}-API-SECRET-KEY"), .s)
    write(paste0(glue::glue("APCA-{.y}-API-KEY-ID = '{.k}'"),"\n",glue::glue("APCA-{.y}-API-SECRET-KEY = '{.s}'")), file = path.expand("~/.Renviron"), append = T)
  })
}

