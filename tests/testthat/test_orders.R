#' @import internal.R
#' @import Orders.R

context("Test that family: orders throws errors/messages as expected.")
vcr::use_cassette("clock_is_open", {
.open <<- clock()$is_open
})
vcr::use_cassette("get_orders", {
.ao <<- orders()
})
vcr::use_cassette("cancel_all_initial", {
if (isTRUE(inherits(.ao, "tbl") && try(nrow(.ao) > 0))) {
  order_submit(a = "cancel_all")
}
})
vcr::use_cassette("get_orders2", {
.ao <- orders()
})
if (.open) {
vcr::use_cassette("orders_returns_no_orders_properly", {
  test_that("orders returns no orders properly.", {
    #assumes no open orders
    expect_type(expect_message(orders(), regexp = "No orders for the selected query/filter criteria. 
Check `ticker_id` or set status = 'all' to see all orders."),"list")
})
  })
} else if (isTRUE(try(nrow(.ao)))) {
  vcr::use_cassette("order_status", {
  expect_true(any(orders()$status %in% "pending_cancel"))
  })
}


vcr::use_cassette("order_submit_errors_if_type_limit_and_limit_is_not_set", {
test_that("order_submit errors if type = limit and limit is not set", {
  expect_error(order_submit("AMZN", side = "sell", type = "limit", qty = 1), "Please set limit price.")
})
})

vcr::use_cassette("order_submit_errors_if_type_stop_and_stop_is_not_set", {
test_that("order_submit errors if type = stop and stop is not set", {
  expect_error(order_submit("AMZN", side = "sell", type = "stop", qty = 1), "Please set stop price.")
})
})

vcr::use_cassette("order_submit_errors_if_type_stop_limit_and_stop_is_not_set", {
test_that("order_submit errors if type = stop_limit and stop is not set", {
  expect_error(order_submit("AMZN", side = "sell", type = "stop_limit", qty = 1, limit = 2400), "stop must be set.")
})
})

vcr::use_cassette("order_submit_errors_if_type_stop_limit_and_limit_is_not_set", {
test_that("order_submit errors if type = stop_limit and limit is not set", {
  expect_error(order_submit("AMZN", side = "sell", type = "stop_limit", qty = 1, stop = 2400), "limit must be set.")
})
})

vcr::use_cassette("order_submit_errors_if_qty_is_not_set", {
test_that("order_submit errors if qty is not set", {
  expect_error(order_submit("AMZN", side = "sell", type = "stop", stop = 2400), "qty must be set.")
})
})

vcr::use_cassette("order_submit_errors_if_qty_is_not_set", {
test_that("order_submit errors if qty is not set", {
  expect_error(order_submit("AMZN", side = "sell", type = "stop", stop = 2400), "qty must be set.")
})
})

context("Test that family: orders functions effectively.")
vcr::use_cassette("order_submit_properly_places_a_simple_buy_order", {
test_that("order_submit properly places a simple buy order", {
  .o <<- order_submit("AMZN", qty = 2, side = "b", type = "m")
  .dts <- purrr::map_lgl(dplyr::select(.o, dplyr::ends_with("at")), lubridate::is.POSIXct)
  expect_equal(sum(.dts), 8)
  expect_equal(dim(.o), c(1,28))
})
})

vcr::use_cassette("orders_properly_detects_the_order", {
test_that("orders properly detects the order", {
  .oo <- orders(s = "all")
  expect_identical(.oo$id[1], .o$id)
  expect_equal(dim(.oo[1,]), c(1,28))
})
})
vcr::use_cassette("polygon_last_quote", {
.lq <<- polygon("lq", symbol = "AMZN")
})
if (.open) {
vcr::use_cassette("An_expedited_stop_can_be_placed_on_the_order_with_appropriate_messages", {
  test_that("An expedited stop can be placed on the order with appropriate messages", {
    expect_message({.so <<- order_submit(.o$id, stop = .lq$askprice * .95, client = T)}, regexp = "side|qty|ticker_id|client_order_id|type", all = T)
    expect_identical(.so$client_order_id, .o$id)
})
  })
} else if (isTRUE(suppressWarnings(positions(.o$symbol)$qty) < .o$qty)){
vcr::use_cassette("An_expedited_stop_warns_if_the_buy_order_did_not_fill_and_there_is_an_insufficient_quantity_available", {
  test_that("An expedited stop warns if the buy order did not fill and there is an insufficient quantity available", {
    expect_warning(expect_message({.so <<- order_submit(.o$id, stop = .lq$askprice * .95, client = T)}, regexp = "side|qty|ticker_id|client_order_id|type", all = T), regexp = "insufficient qty available for order")
})
  })
} else if (!.open){
  vcr::use_cassette("market_closed_expedited_stop", {
  test_that("market is closed and expedited stop is warns properly", {
    expect_warning(expect_message({.so <<- order_submit(.o$id, stop = .lq$askprice * .95, client = T)}, regexp = "side|qty|ticker_id|client_order_id|type", all = T), "cannot open a short sell while a long buy order is open")
  })
  })
}

if (.open && exists(".so")) {
vcr::use_cassette("order_submit_properly_modifies_the_simple_sell_order", {
  test_that("order_submit properly modifies the simple sell order", {
    .r <<- order_submit(.so$id, action = "r", qty = 1, stop = .lq$askprice * .95)
    # when the market is open
    expect_identical(.r$replaces, .so$id)
    expect_identical(.r$qty, 1)
})
  })
} else if (exists(".so")) {
vcr::use_cassette("order_submit_errors_if_outside_of_market_hours", {
  test_that("order_submit errors if outside of market hours", {
    .m <- "(?:unable to replace order, order is not open)|(?:unable to replace order, order isn't sent to exchange yet)|(?:Not Found)"
    expect_warning({.r <<- order_submit(.so$id, action = "r", qty = 1, stop = .lq$askprice * .95)}, regexp = .m)
    expect_true(stringr::str_detect(.r, .m))
})
  })
}

vcr::use_cassette("order_submit_properly_places_a_bracket_order_class", {
test_that("order_submit properly places a bracket order_class", {
  expect_message(.bo <<- order_submit("AMZN", order_class = "bracket", qty = 2, take_profit = list(l = .lq$askprice * 1.03), stop_loss = list(l = .lq$askprice * .94, s = .lq$askprice * .95)), regexp = "buy|market", all = T)
  expect_equal(dim(.bo), c(2, 28))
  expect_equal(dim(.bo$legs), c(2, 28))
  .dts <- unlist(purrr::map_depth(.bo, -1, .ragged = T, lubridate::is.POSIXct))
  expect_equal(sum(.dts), 24)
})
})


vcr::use_cassette("order_submit_properly_cancels_the_bracket_order", {
test_that("order_submit properly cancels the bracket order", {
  expect_message({.co <<- order_submit(.bo$id, "c")}, "Order canceled successfully")
  expect_true(is.list(.co))
  expect_equal(length(.co), 0)
  
})
})

if (.open) {
vcr::use_cassette("order_submit_properly_cancels_the_simple_sell_replacement_order", {
  test_that("order_submit properly cancels the simple sell replacement order", {
    expect_message({.co <- order_submit(.r$id, "c")}, "Order canceled successfully")
    expect_true(is.list(.co))
    expect_equal(length(.co), 0)
})
  })
} 


if (.open) {
vcr::use_cassette("order_submit_properly_places_an_oco_order_class", {
  test_that("order_submit properly places an oco order_class", {
    .o <- order_submit("AMZN", qty = 2, side = "b", type = "m")
    expect_message(.oco <- order_submit("AMZN", order_class = "oco", qty = 2, take_profit = list(l = .lq$askprice * 1.03), stop_loss = list(s = .lq$askprice * .96)), regexp = "sell|limit", all = T)
})
  })
} else {
  suppressWarnings(.op <- positions("AMZN"))
  if (isTRUE(suppressWarnings(.op$qty) < 2)) {
vcr::use_cassette("order_submit_warns_appropriately_when_market_is_closed", {
    test_that("order_submit warns appropriately when market is closed", {
      expect_warning(expect_message(.oco <- order_submit("AMZN", order_class = "oco", qty = 2, take_profit = list(l = .lq$askprice * 1.03), stop_loss = list(s = .lq$askprice * .96)), regexp = "sell|limit", all = T), regexp = "insufficient qty available for order") 
})
    })
  }
}

vcr::use_cassette("order_submit_properly_places_an_oto_order_class", {
test_that("order_submit properly places an oto order_class", {
  .lq <- polygon("lq", symbol = "BYND")
  expect_message(.oto <- order_submit("BYND", order_class = "oto", qty = 2, stop_loss = list(s = .lq$askprice * .96)), regexp = "oto", all = T)
})
})
vcr::use_cassette("orders_final", {
.oo <- orders()
})
vcr::use_cassette("cancel_all_final", {
.ca <- order_submit(a = "cancel_all")  
})
if (.open && length(.oo) > 0) {
vcr::use_cassette("order_submit_properly_cancels_all_open_orders_when_market_is_open_and_there_are_orders_to_cancel", {
  test_that("order_submit properly cancels all open orders when market is open and there are orders to cancel",{
    expect_true(any(.ca$status %in% c("pending_cancel")))
    expect_true(nrow(.ca) > 1)
    expect_s3_class(do.call(c, dplyr::select(.ca, dplyr::ends_with("at"))), "POSIXct")
    expect_message(orders(), regexp = "(?:No orders for the selected query\\/filter criteria)|(?: Check `ticker_id` or set status \\= \\'all\\' to see all orders.)")
})
  })
} else if (length(.oo) > 0) {
vcr::use_cassette("order_submit_properly_cancels_all_oepn_order_when_there_are_no_open_orders_to_cancel", {
  test_that("order_submit properly cancels all oepn order when there are no open orders to cancel", {
    expect_message(orders(), regexp = "(?:No orders for the selected query\\/filter criteria)|(?: Check `ticker_id` or set status \\= \\'all\\' to see all orders.)")
})
  })
  
}
