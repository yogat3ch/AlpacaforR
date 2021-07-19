#' @include  Positions.R
#' @include internal.R

vcr::vcr_configure(dir = file.path(dirname(.log_path), "positions"))

vcr::use_cassette("Positions_returns_the_appropriate_data", {
test_that("Positions returns the appropriate data", {
  order_submit("AMZN", qty = 1, side = "buy", type = "market")
  Sys.sleep(5)
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

if (.op && market_open) {
  
  vcr::use_cassette("Positions_cancels_a_single_order_when_market_is_open", {
    test_that("Positions cancels a single order when market is open", {
      .pc <- positions(.p$symbol[1], action = "c")
      expect_true(.pc$qty == .p$qty && .pc$side == "sell")
    })
  })
  
  if (nrow(.p) > 1) {
    
vcr::use_cassette("Positions_cancels_all_orders_when_market_is_open", {
    test_that("Positions_cancels_all_orders_when_market_is_open", {
      .cp <- positions(a = "close_all")
      
})
    })
    
  }
}

if (market_open) {
  vcr::use_cassette("positions_amzn", match_requests_on = "path", {
    .lq <<- market_data(timeframe = "lt", symbol = "AMZN")
  })
  vcr::use_cassette("positions_order_submit_amzn", {
  order_submit("AMZN", qty = 1, order_class = "b", take_profit = list(l = .lq$p * 1.05), stop_loss = list(l = .lq$p * .95, s = .lq$p * .96))
  })
  Sys.sleep(1)
  vcr::use_cassette("Positions_cancels_complex_orders_correctly", match_requests_on = "path", {
    test_that("Positions cancels complex orders correctly", {
      .p <- positions("AMZN")
      expect_warning(.pc <- positions(a = "close_all"))
      expect_equal(.pc$legs[[1]][, c("order_type", "side")], data.frame(order_type = c("limit", "stop_limit"), side = rep("sell", 2)), ignore_attr = TRUE)
    })
  })
}

