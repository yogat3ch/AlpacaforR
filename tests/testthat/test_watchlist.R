#' @include Watchlist.R
#' @include internal.R

# Remove watchlists with names used herein
purrr::walk(c("test", "test2", "FAANG", "FABANGG"), ~try(watchlist(.x, action = "d")))

vcr::use_cassette("watchlist_can_be_created_with_a_single_entry", {
test_that("A watchlist can be created with a single entry", {
  (wl <- watchlist(name = "test", tickers = "MSFT")) 
  expect_length(wl, 10)
  expect_identical(wl$symbol, "MSFT")
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
  (wl <- watchlist("test"))
  expect_length(wl, 10)
  expect_identical(wl$symbol, "MSFT")
  expect_identical(attr(wl, "info")$name, "test")
})
}) 

vcr::use_cassette("watchlist_can_be_deleted_successfully", {
test_that("The watchlist can be deleted successfully", {
  expect_message(wl <- watchlist("test", a = "d"), regexp = "Watchlist deleted successfully")
})
})

vcr::use_cassette("watchlist_replaces_and_renames_simultaneously", {
test_that("A watchlist replaces and renames simultaneously", {
  watchlist(name = "test", tickers = c("AAPL", "WMT"))
  .t <- c("FB", "AAPL", "AMZN", "NFLX", "GOOG", "WMT")
  (wl <- watchlist("test", name = "test2", tickers = .t, action = "r"))
  expect_length(wl, 10)
  expect_identical(wl$symbol, c("FB", "AAPL", "AMZN", "NFLX", "GOOG", "WMT"))
  expect_identical(attr(wl, "info")$name, "test2")
})
})

vcr::use_cassette("watchlist_can_be_renamed_and_have_a_single_asset_removed", {
test_that("A watchlist can be renamed and have a single asset removed", {
  (wl <- watchlist("test2", name = "FAANG", ticker = "WMT", action = "d")) 
  expect_identical(attr(wl, "info")$name, "FAANG")
  expect_identical(wl$symbol, c("FB", "AAPL", "AMZN", "NFLX", "GOOG"))
})
})

vcr::use_cassette("watchlist_can_be_renamed_and_have_assets_added", {
test_that("A watchlist can be renamed and have assets added", {
  (wl <- watchlist("FAANG", "FABANGG", ticker = c("BYND", "GOOGL"), action = "a"))
  expect_identical(attr(wl, "info")$name, "FABANGG")
  expect_identical(wl$symbol, c("BYND", "GOOGL", "FB", "AAPL", "AMZN", "NFLX", "GOOG"))
})
})

vcr::use_cassette("watchlist_can_have_assets_simply_removed", {
test_that("A watchlist can have assets simply removed", {
  (wl <- watchlist("FABANGG", tickers = c("BYND", "GOOGL"), action = "d")) 
  expect_identical(wl$symbol, c("FB", "AAPL", "AMZN", "NFLX", "GOOG"))
})
})

vcr::use_cassette("watchlist_can_be_just_renamed", {
test_that("A watchlist can be just renamed", {
  (wl <- watchlist("FABANGG", "FAANG"))
  expect_identical(attr(wl, "info")$name, "FAANG")
})
})

vcr::use_cassette("watchlist_can_be_deleted", {
test_that("The watchlist can be deleted", {
  expect_length(watchlist("FAANG", a = "d"), 0) 
})
})
# remove it (with partial action argument)
