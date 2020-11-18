#' @title First Run Credential Setup
#' @description This function helps edit the .Renviron file to work with `AlpacaforR`. It's for first time users and only needs to be run once.
#' @param paper_api \code{(named character)} of length two: `key` and `secret` for the Alpaca paper API.
#' @param live_api \code{(named character)} of length two: `key` and `secret` for the Alpaca live API.
#' @param live \code{(logical)} of length one: the default option for the `live` argument in functions that use it.
#' @return Sets environment variables for this session and edits the .Renviron file in R's home directory `path.expand("~")` and adds the specified key/secret combinations for future sessions. If an .Renviron file does not exist, it will be created.
#' @importFrom purrr iwalk
#' @importFrom glue glue
#' @examples 
#' \dontrun{
#' firstrun(paper_api = c(key = "mypaperkey", secret = "mypapersecret"), live_api = c(key = "mylivekey", secret = "mylivesecret"), live = FALSE)
#' }

firstrun <- function(paper_api, live_api, live) {
  .re <- path.expand("~/.Renviron")
  .l <- readLines(.re)
  if (!missing(paper_api) || !missing(live_api)) {
    purrr::iwalk(list(PAPER = paper_api, LIVE = live_api), ~{
      if (is.list(.x)) {
        .k <- .x$key
        .s <- .x$secret
      } else {
        .k <- .x[grep("^k", names(.x), ignore.case = T)]
        .s <- .x[grep("^s", names(.x), ignore.case = T)]
      }
      .key_id <- glue::glue("APCA-{.y}-API-KEY-ID")
      .secret <- glue::glue("APCA-{.y}-API-SECRET-KEY")
      do.call(Sys.setenv, rlang::list2(!!.key_id := .k))
      do.call(Sys.setenv, rlang::list2(!!.secret := .s))
      if (!file.exists(.re)) file.create(.re)
      
      .entry_idx <- which(stringr::str_detect(.l, glue::glue("^APCA-{.y}-API")))

      if (rlang::is_empty(.entry_idx)) {
        write(paste0(glue::glue("{.key_id} = '{.k}'"),"\n", glue::glue("{.secret} = '{.s}'")), file = .re, append = TRUE)
      } else {
        .l[.entry_idx] <- c(
          glue::glue("{.key_id} = '{.k}'"),
          glue::glue("{.secret} = '{.s}'")
        )
        write(.l, file = .re, sep = "\n")
      } 
    })
  }
  
  if (!missing(live)) {
    do.call(Sys.setenv, rlang::list2(!!glue::glue("APCA-LIVE") := live))
    .entry_idx <- which(stringr::str_detect(.l, glue::glue("^APCA-LIVE\\s")))
    if (rlang::is_empty(.entry_idx)) {
      write(glue::glue("APCA-LIVE = {live}"), file = path.expand("~/.Renviron"), append = T)
    } else {
      .l[.entry_idx] <- c(
        glue::glue("APCA-LIVE = {live}")
      )
      write(.l, file = .re, sep = "\n")
    } 
  }
  .nl <- readLines(.re)
  message("New Renviron:\n",paste0(.nl, collapse = "\n"))
}

