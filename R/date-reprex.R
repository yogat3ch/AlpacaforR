install.packages("httr")
install.packages("tidyverse")
headers <- httr::add_headers('APCA-API-KEY-ID' = "KEY", 
                  'APCA-API-SECRET-KEY' = "SECRET")

dat <- httr::GET("https://data.alpaca.markets/v1/bars/5Min?symbols=FB,NFLX,AMZN,MSFT&limit=1000&start=2020-02-05T09:30:00-0500&end=2020-02-24T14:45:00-0500", headers)

dat = httr::content(dat, as = "text", encoding = "UTF-8")
bars = jsonlite::fromJSON(dat)
bars <- purrr::imap(bars, ~{
  if (!any("data.frame" %in% class(.x))) {
    message(paste0("The symbol ", .y, " returned no data.")) 
    return(NULL)
  } 
  nms <- c(time = "t", open = "o", high = "h", low = "l", close = "c", volume = "v")
  out <- dplyr::mutate_at(.x, dplyr::vars("t"), ~lubridate::as_datetime(.,origin = lubridate::origin)) %>% dplyr::mutate_at(dplyr::vars(o,h,c,l,v),~as.numeric(.)) %>%
    dplyr::rename((!!nms))
})

range(bars$AMZN$time)