#' @include  Positions.R
#' @include internal.R

context("Test that the Positions family of functions works properly")

vcr::use_cassette("Positions_returns_the_appropriate_data", {
test_that("Positions returns the appropriate data", {
  .p <<- positions()
  .op <<- tryCatch(isTRUE(nrow(.p) > 0), error = function(e) F)
  if (.op) {
    expect_length(.p, 16)
    expect_named(.p, c("asset_id", "symbol", "exchange", "asset_class", "qty", "avg_entry_price", "side", "market_value", "cost_basis", "unrealized_pl", "unrealized_plpc", "unrealized_intraday_pl", "unrealized_intraday_plpc", "current_price", "lastday_price", "change_today"))
  } else {
    expect_message(positions(), regexp = "No positions")
  }
})
})
vcr::use_cassette("positions_clock_is_open", {
.open <- clock()$is_open
})
if (.op && .open) {
  
vcr::use_cassette("Positions_cancels_a_single_order_when_market_is_open", {
  test_that("Positions cancels a single order when market is open", {
    expect_message(positions(.p$symbol[1], action = "c"), regexp = paste0(.p$symbol[1], " closed successfully"))
})
  })
  
  if (nrow(.p) > 1) {
    
vcr::use_cassette("Positions_cancels_all_orders_when_market_is_open", {
    test_that("Positions_cancels_all_orders_when_market_is_open", {
      expect_message({.cp <- positions(a = "close_all")}, regexp = "All positions closed successfully")
})
    })
    
  }
} else  if (.op) {
vcr::use_cassette("Positions_closes_a_single_position_when_market_is_closed", {
  test_that("Positions closes a single position when market is closed", {
    expect_warning(positions(.p$symbol[1], action = "c"))
})
  })
  
  if (nrow(.p) > 1) {
vcr::use_cassette("Positions_cancels_all_orders_when_market_is_closed", {
    test_that("Positions_cancels_all_orders_when_market_is_closed", {
      expect_warning(expect_message({.cp <- positions(a = "close_all")}, regexp = "Related orders prevent positions"))
})
    })
  }
}

if (.open) {
  vcr::use_cassette("positions_amzn", match_requests_on = "path", {
    .lq <<- polygon("lq", symbol = "AMZN")
  })
  vcr::use_cassette("positions_order_submit_amzn", {
  order_submit("AMZN", qty = 1, order_class = "b", take_profit = list(l = .lq$askprice * 1.05), stop_loss = list(l = .lq$askprice * .95, s = .lq$askprice * .96))
  })

  vcr::use_cassette("positions_bynd", match_requests_on = "path", {
    .lq <<- polygon("lq", symbol = "BYND")
  })
  
  
  vcr::use_cassette("positions_order_submit_bynd", {
  order_submit("BYND", qty = 1, order_class = "b", take_profit = list(l = .lq$askprice * 1.05), stop_loss = list(l = .lq$askprice * .95, s = .lq$askprice * .96))
  })
vcr::use_cassette("Positions_cancels_complex_orders_correctly", {
  test_that("Positions cancels complex orders correctly", {
    expect_warning(expect_message({.cp <- positions(a = "close_all")}, regexp = "(?:Order canceled successfully)|(?:closed successfully)"), regexp = "Canceled order")
})
  })
  
}

# need to determine if canceling single positions from complex orders is possible with changes
