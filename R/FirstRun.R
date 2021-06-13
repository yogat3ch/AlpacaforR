#' @title First Run Credential Setup
#' @description This function helps edit the .Renviron file to work with `AlpacaforR`. It's for first time users and only needs to be run once.
#' @param paper_api \code{(named character)} of length two: `key` and `secret` for the Alpaca paper API.
#' @param live_api \code{(named character)} of length two: `key` and `secret` for the Alpaca live API.
#' @param polygon_api \code{(named character)} of length one: `key`
#' @param pro \code{(logical)} of length one: Set as `TRUE` to use the \href{Alpaca Market Data Pro Subscription}{https://alpaca.markets/docs/api-documentation/api-v2/market-data/alpaca-data-api-v2/#subscription-plans} otherwise the Basic plan will be used. **Default** `FALSE`
#' @param live \code{(logical)}: the default option for whether to use the live account `TRUE` or paper account `FALSE`. This option can be overridden for functions that accept the `live` argument, or can be changed at a later time. **Default** `FALSE`
#' @param scope \code{(character)} specify at which level of scope the changes to .Renviron should apply.
#' @return Sets environment variables for this session and edits the .Renviron file in R's home directory `path.expand("~")` and adds the specified key/secret combinations for future sessions. If an .Renviron file does not exist, it will be created.
#' @importFrom purrr iwalk
#' @importFrom glue glue
#' @examples 
#' \dontrun{
#' firstrun(
#' paper_api = c(key = "mypaperkey", secret = "mypapersecret"),
#' live_api = c(key = "mylivekey", secret = "mylivesecret"),
#' polygon_api = c(key = "polygonkey"),
#' live = FALSE,
#' pro = FALSE
#' )
#' }

firstrun <- function(paper_api, live_api, polygon_api, pro = FALSE, live = FALSE, scope = c("user", "project")) {
  .re <- switch(tolower(substr(scope, 0, 1)[1]), 
                u = path.expand("~/.Renviron"),
                p = file.path(usethis::proj_get(),".Renviron"))
  if (!file.exists(.re)) file.create(.re)
  .l <- readLines(.re)
  
    
  keys <- purrr::compact(
    list(
      `APCA-PAPER-KEY` = if (!missing(paper_api))
        paper_api[1],
      `APCA-PAPER-SECRET` = if (!missing(paper_api))
        paper_api[2],
      `APCA-LIVE-KEY` = if (!missing(live_api))
        live_api[1],
      `APCA-LIVE-SECRET` = if (!missing(live_api))
        live_api[2],
      `POLYGON-KEY` = if (!missing(polygon_api))
        polygon_api,
      `APCA-LIVE` = live,
      `APCA-PRO` = pro
    )
  )
  
  .lns <- purrr::map_dbl(rlang::set_names(c('APCA-PAPER-KEY',
                 'APCA-PAPER-SECRET',
                 'APCA-LIVE-KEY',
                 'APCA-LIVE-SECRET',
                 'POLYGON-KEY',
                 'APCA-LIVE',
                 'APCA-PRO')), ~stringr::str_which(.l, paste0(.x,"\\s")) %|z|% NA_real_)
  # Add/Update Values ----
  # Thu Mar 18 09:44:37 2021
  purrr::iwalk(keys, ~{
    browser()
    do.call(Sys.setenv, rlang::list2(!!.y := .x))
    .entry_idx <- .lns[.y]
    .line <- glue::glue("{.y} = '{.x}'")
    if (is.na(.entry_idx)) {
      write(paste0(.line, collapse = "\n") , file = .re, append = TRUE)
    } else {
      .l[.entry_idx] <- .line
      write(.l, file = .re, sep = "\n")
    } 
  })

  
  # Print changes to console
  cli::cat_line(cli::col_blue(.re), "\n", cli::col_white(paste0(readLines(.re), collapse = "\n")))
}

