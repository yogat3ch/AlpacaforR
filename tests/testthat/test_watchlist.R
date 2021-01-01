#' @include Watchlist.R
#' @include internal.R
vcr::vcr_configure(dir = file.path(dirname(.log_path), "watchlist"))
# Remove watchlists with names used herein
purrr::walk(c("test", "test2", "FAANG", "FABANGG"), ~try(watchlist(.x, action = "d")))

vcr::use_cassette("watchlist_can_be_created_with_a_single_entry", {
test_that("A watchlist can be created with a single entry", {
  (wl <<- watchlist("test", symbols = "AAPL", action = "c")) 
  expect_length(wl, 10)
  expect_identical(wl$symbol, "AAPL")
  expect_identical(attr(wl, "info")$name, "test")
})
})

vcr::use_cassette("watchlist_appears_in_the_list_of_watchlists", {
test_that("The watchlist appears in the list of watchlists", {
  wls <- watchlist() 
  expect_true("test" %in% wls$name)
})
})

vcr::use_cassette("watchlist_is_returned_when_it_s_name_is_passed_as_an_arg", {
test_that("The watchlist is returned when it's name is passed as an arg", {
  (test <- watchlist("test"))
  expect_s3_class(wl, "watchlist")
  expect_equal(test,wl, ignore_attr = TRUE)
  expect_s3_class(attr(wl, "info")[["created_at"]], "POSIXct")
})
}) 

vcr::use_cassette("watchlist_renames_and_adds", {
test_that("Add FB, AMZN, NFLX, GOOG and update the watchlist name to FAANG", {
  (wl <- watchlist("test", new_name = "FAANG", symbols = c("FB", "AMZN", "NFLX", "GOOG")))
  expect_equal(wl$symbol, c("AAPL","FB", "AMZN", "NFLX", "GOOG"))
  expect_equal(attr(wl,"info")$name, "FAANG")
})
})

vcr::use_cassette("watchlist_add", {
test_that("watchlist: Add individual stocks", {
  # Add individual stocks by specifying symbols (action assumed to be add when symbols are present)
  (wl <- watchlist("FAANG", symbol = "GOOGL"))
  expect_length(wl, 10)
  expect_identical(wl$symbol, c("AAPL", "FB", "AMZN", "NFLX", "GOOG", "GOOGL"))
  expect_identical(attr(wl, "info")$name, "FAANG")
})
})

vcr::use_cassette("watchlist_delete_asset", {
test_that("watchlist: Delete individual items (or multiple)", {
  (wl <- watchlist("FAANG", action = "d", symbols = "GOOGL"))
  expect_identical(attr(wl, "info")$name, "FAANG")
  expect_identical(wl$symbol, c("AAPL","FB", "AMZN", "NFLX", "GOOG"))
})
})



vcr::use_cassette("watchlist_rename_replace_symbols_w_update", {
test_that("Rename watchlist & replace the symbols with action = update", {
  #  to match the name by specifying action = "update" explicitly.
  (wl <- watchlist("FAANG", new_name = "FANG", symbols = c("FB", "AAPL", "NFLX", "GOOG"), action = "u"))
  expect_identical(attr(wl, "info")$name, "FANG")
  expect_identical(wl$symbol, c("FB", "AAPL", "NFLX", "GOOG"))
})
})

vcr::use_cassette("watchlist_renamed_no_add", {
  test_that("Rename Watchlist and preserve existing", {
    (wl <- watchlist("FANG", new_name = "_FANG"))
    expect_identical(attr(wl, "info")$name, "_FANG")
    expect_identical(wl$symbol, c("FB", "AAPL", "NFLX", "GOOG"))
  })
})

vcr::use_cassette("watchlist_delete", {
test_that("watchlist delete", {
  # Delete it using a partial argument (may cause a warning)
  expect_message(watchlist("_FANG", a = "d"), regexp = "Watchlist deleted successfully")
  expect_true(!"FANG" %in% watchlist()$name)
})
})

