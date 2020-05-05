#' @import Positions.R
#' @import internal.R

library(testthat)
context("Test that the Positions family of functions works properly")

test_that("Positions returns the appropriate data", {
  .p <- positions()
  .op <- tryCatch(isTRUE(nrow(.p) > 0), error = function(e) F)
  if (.op) {
    expect_length(.p, 16)
    expect_named(.p, c("asset_id", "symbol", "exchange", "asset_class", "qty", "avg_entry_price", "side", "market_value", "cost_basis", "unrealized_pl", "unrealized_plpc", "unrealized_intraday_pl", "unrealized_intraday_plpc", "current_price", "lastday_price", "change_today"))
  } else {
    expect_message(positions(), regexp = "No positions")
  }
})
if (.op) {
  test_that("Positions cancels a single order", {
    expect_message(positions(.p$symbol[1], action = "c"), regexp = paste0(.p$symbol[1], " closed successfully"))
  })
  if (nrow(.p) > 1) {
    expect_message(.cp <- positions("close_all"), regexp = "All positions closed successfully")
  }
}
