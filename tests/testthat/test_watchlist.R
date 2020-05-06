#' @include Watchlist.R
#' @include internal.R

context("Watchlist family of functions works as intended.")

test_that("A watchlist can be created with a single entry", {
  (wl <- watchlist(name = "test", tickers = "MSFT")) 
  expect_length(wl, 10)
  expect_identical(wl$symbol, "MSFT")
  expect_identical(attr(wl, "info")$name, "test")
})

test_that("The watchlist appears in the list of watchlists", {
 wls <- watchlist() 
 expect_true("test" %in% wls$name)
})

test_that("The watchlist is returned when it's name is passed as an arg", {
  (wl <- watchlist("test"))
  expect_length(wl, 10)
  expect_identical(wl$symbol, "MSFT")
  expect_identical(attr(wl, "info")$name, "test")
}) 
 
test_that("The watchlist can be deleted successfully", {
  expect_message(wl <- watchlist("test", a = "d"), regexp = "Watchlist deleted successfully")
})
 
test_that("A watchlist can have it's values replaced and renamed simultaneously", {
  watchlist(name = "test", tickers = c("AAPL", "WMT"))
  .t <- c("FB", "AAPL", "AMZN", "NFLX", "GOOG", "WMT")
  (wl <- watchlist("test", name = "test2", tickers = .t, action = "r"))
  expect_length(wl, 10)
  expect_identical(wl$symbol, .t)
  expect_identical(attr(wl, "info")$name, "test2")
})

test_that("A watchlist can be renamed and have a single asset removed", {
  (wl <- watchlist("test2", name = "FAANG", ticker = "WMT", action = "d")) 
  expect_identical(attr(wl, "info")$name, "FAANG")
  expect_identical(wl$symbol, .t[-length(.t)])
})
 
test_that("A watchlist can be renamed and have assets added", {
  (wl <- watchlist("FAANG", "FABANGG", ticker = c("BYND", "GOOGL"), action = "a"))
  expect_identical(attr(wl, "info")$name, "FABANGG")
  expect_identical(wl$symbol, c("BYND", "GOOGL", .t[-length(.t)]))
})

test_that("A watchlist can have assets simply removed", {
  (wl <- watchlist("FABANGG", tickers = c("BYND", "GOOGL"), action = "d")) 
  expect_identical(wl$symbol, .t[-length(.t)])
})

test_that("A watchlist can be just renamed", {
  (wl <- watchlist("FABANGG", "FAANG"))
  expect_identical(attr(wl, "info")$name, "FAANG")
})
 
test_that("The watchlist can be deleted", {
  expect_length(watchlist("FAANG", a = "d"), 0) 
})
 # remove it (with partial action argument)
 