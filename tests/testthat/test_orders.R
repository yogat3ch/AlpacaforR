#' @include Orders.R
# reset VCR
# purrr::walk(list.files("tests/testthat/vcr", full.names = T) %>% {.[stringr::str_detect(.,"^orders_")]}, file.remove)
vcr::vcr_configure(dir = file.path(dirname(.log_path), "orders"))

# Errors ----
# Wed Dec 30 11:02:16 2020

test_that("order_submit errors if type = limit and limit is not set", {
  expect_error(order_submit("AMZN", side = "sell", type = "limit", qty = 1), "Please set limit price.")
})


test_that("order_submit errors if type = stop and stop is not set", {
  expect_error(order_submit("AMZN", side = "sell", type = "stop", qty = 1), regexp = "`stop`")
})



test_that("order_submit errors if type = stop_limit and stop is not set", {
  expect_error(order_submit("AMZN", side = "sell", type = "stop_limit", qty = 1, limit = 2400), "stop must be set.")
})



test_that("order_submit errors if type = stop_limit and limit is not set", {
  expect_error(order_submit("AMZN", side = "sell", type = "stop_limit", qty = 1, stop = 2400), "limit must be set.")
})



test_that("order_submit errors if qty is not set", {
  expect_error(order_submit("AMZN", side = "sell", type = "stop", stop = 2400), "qty must be set.")
})



test_that("order_submit errors if qty is not set", {
  expect_error(order_submit("AMZN", side = "sell", type = "stop", stop = 2400), "qty must be set.")
})


# Order placement ----
# Wed Dec 30 11:02:24 2020

vcr::use_cassette("prep_for_orders", record = "new_episodes", {
  .open <<- clock()$is_open
  .ao <<- orders()
  if (isTRUE(inherits(.ao, "tbl") && try(nrow(.ao) > 0))) {
    order_submit("cancel_all")
  }
})

if (.open) {
  vcr::use_cassette("returns_no_orders_properly", {
    test_that("orders returns no orders properly.", {
      #assumes no open orders
      expect_type(expect_message(orders(), regexp = "No orders for the selected query/filter criteria. 
Check `symbol_id` or set status = 'all' to see all orders."),"list")
    })
  })
  
  vcr::use_cassette("order_submit_properly_places_a_simple_buy_order", {
    test_that("order_submit properly places a simple buy order", {
      .o <<- order_submit("AMZN", qty = 2, side = "b", type = "m")
      .dts <- purrr::map_lgl(dplyr::select(.o, dplyr::ends_with("at")), lubridate::is.POSIXct)
      expect_equal(sum(.dts), 8)
      expect_equal(dim(.o), c(1,31))
    })
  })
  
  vcr::use_cassette("properly_detects_the_order", {
    test_that("orders properly detects the order", {
      .oo <<- orders(status = "all")
      expect_identical(.oo$id[1], .o$id)
    })
  })
  
  vcr::use_cassette("polygon_last_quote", match_requests_on = "uri", {
    .lq <<- market_data("lq", symbol = "AMZN")
  })
  
  if (isTRUE(suppressWarnings(positions(.o$symbol)$qty) < .o$qty)){
    vcr::use_cassette("An_expedited_stop_warns_if_insufficient_quantity_available", {
      test_that("An expedited stop warns if insufficient quantity available", {
        expect_warning(expect_message({.so <<- order_submit(.o, stop = .lq$askprice * .95, client = T)}, regexp = "side|qty|symbol_id|client_order_id|type"), regexp = "insufficient qty available for order")
      })
    })
  }
  
  vcr::use_cassette("An_expedited_stop_can_be_placed_with_appropriate_messages", {
    test_that("An_expedited_stop_can_be_placed_with_appropriate_messages", {
      expect_message({.so <<- order_submit(.o, stop = .lq$askprice * .95, client_order_id = T)}, regexp = "side|qty|symbol_id|client_order_id|type")
      expect_identical(.so$client_order_id, .o$id)
    })
  })
  
  if (!is.null(get0(".so", ifnotfound = list(id = NULL))$id)) {
    vcr::use_cassette("order_submit_properly_modifies_the_simple_sell_order", {
      test_that("order_submit properly modifies the simple sell order", {
        .r <<- order_submit(.so, action = "r", qty = 1, stop = .lq$askprice * .95)
        # when the market is open
        expect_identical(.r$replaces, .so$id)
        expect_identical(.r$qty, 1)
      })
    })
  }
  
  vcr::use_cassette("order_submit_properly_places_a_bracket_order_class", {
    test_that("order_submit properly places a bracket order_class", {
      expect_message(.bo <<- order_submit("AMZN", order_class = "bracket", qty = 2, take_profit = list(l = .lq$askprice * 1.05), stop_loss = list(l = .lq$askprice * .94, s = .lq$askprice * .95)), regexp = "buy|market")
      expect_equal(dim(.bo), c(2, 31))
      expect_equal(dim(.bo$legs), c(2, 31))
      .dts <- unlist(purrr::map_depth(.bo, -1, .ragged = T, lubridate::is.POSIXct))
      expect_equal(sum(.dts), 24)
    })
  })
  
  vcr::use_cassette("order_submit_properly_cancels_the_bracket_order", {
    test_that("order_submit properly cancels the bracket order", {
      expect_message({.co <<- order_submit(.bo, "c")}, "Order canceled successfully")
      expect_true(is.list(.co))
      expect_equal(length(.co), 0)
      
    })
  })
  
  vcr::use_cassette("order_submit_properly_cancels_the_simple_sell_replacement_order", {
    test_that("order_submit properly cancels the simple sell replacement order", {
      expect_message({.co <- order_submit(.r, "c")}, "Order canceled successfully")
      expect_true(is.list(.co))
      expect_equal(length(.co), 0)
    })
  })
  
  vcr::use_cassette("order_submit_properly_places_an_oco_order_class", record = "new_episodes", {
    test_that("order_submit properly places an oco order_class", {
      .o <- order_submit("AMZN", qty = 3, side = "b", type = "m")
      expect_message(.oco <- order_submit("AMZN", order_class = "oco", qty = 1, take_profit = list(l = .lq$askprice * 1.03), stop_loss = list(s = .lq$askprice * .96)), regexp = "sell|limit", all = T)
    })
  })
  
  # trailing stops
  vcr::use_cassette("trailing_stop_works_with_a_stop_value", {
    test_that("trailing_stop works with a stop value of 10", {
      .ts <<- order_submit("AMZN", qty = 1, side = "sell", type = "trailing_stop", trail_price = 10)
      expect_equal(.ts$trail_price, 10)
      expect_equal(dim(.ts), c(1,31))
    })
  })
  
  vcr::use_cassette("trailing_stop_works_with_a_stop_percent", record = "new_episodes", {
    test_that("trailing_stop works with a stop percent of 5", {
      .ts <<- order_submit("AMZN", qty = 1, side = "sell", type = "trailing_stop", trail_percent = 5)
      expect_equal(.ts$trail_percent, 5)
      expect_equal(dim(.ts), c(1,31))
    })
  })
  
  
  vcr::use_cassette("warn_for_short_sell", match_requests_on = "uri", record = "new_episodes", {
    test_that("warn_for_short_sell", {
      suppressWarnings({
        order_submit(.ts, action = "cancel")
        positions("LSF", "close")
      })
      expect_warning(.ts <<- order_submit("LSF", side = "sell", qty = 1, trail_price = 5), regexp = "This order will be a short sell")
    })
  })
  
  vcr::use_cassette("warn_for_buy_stop", match_requests_on = "uri", record =  "new_episodes",{
    test_that("warn_for_buy_stop", {
      suppressWarnings({
        order_submit(.ts, action = "cancel")
        positions("LSF", "close")
      })
      testthat::expect_warning(.ts <- order_submit("LSF", side = "buy", qty = 1, trail_price = 5))
    })
  })
  
  
  vcr::use_cassette("lq_bynd", match_requests_on = "path", {
    .lq <<- market_data("lq", symbol = "BYND")
  })
  vcr::use_cassette("order_submit_properly_places_an_oto_order_class", {
    test_that("order_submit properly places an oto order_class", {
      expect_message(.oto <- order_submit("BYND", order_class = "oto", qty = 2, stop_loss = list(s = .lq$askprice * .96)), regexp = "oto", all = T)
    })
  })
  
  
  
# Market closed ----
# Wed Dec 30 11:11:37 2020  
} else if (!.open){
  vcr::use_cassette("market_closed_expedited_stop", {
    test_that("market is closed and expedited stop is warns properly", {
      expect_warning(expect_warning(expect_message({.so <<- order_submit(.o, stop = .lq$askprice * .95, client = T)}, regexp = "side|qty|symbol_id|client_order_id|type"), "cannot open a short sell while a long buy order is open"), regexp = "short sell")
    })
  })
  
  if (!is.null(get0(".so", envir = .GlobalEnv, ifnotfound = list(id = NULL))$id)) {
    vcr::use_cassette("order_submit_errors_if_outside_of_market_hours", {
      test_that("order_submit errors if outside of market hours", {
        .m <- "(?:unable to replace order, order is not open)|(?:unable to replace order, order isn't sent to exchange yet)|(?:Not Found)|(?:`symbol_id` is not an Order ID.)"
        expect_warning({.r <<- order_submit(.so, action = "r", qty = 1, stop = .lq$askprice * .95)}, regexp = .m)
        expect_true(stringr::str_detect(.r, .m))
      })
    })
  }
  
  suppressWarnings(.op <- positions("AMZN"))
  if (isTRUE(suppressWarnings(.op$qty) < 2)) {
    vcr::use_cassette("order_submit_warns_appropriately_when_market_is_closed", {
      test_that("order_submit warns appropriately when market is closed", {
        expect_warning(expect_message(.oco <- order_submit("AMZN", order_class = "oco", qty = 3, take_profit = list(l = .lq$askprice * 1.03), stop_loss = list(s = .lq$askprice * .96)), regexp = "sell|limit", all = T), regexp = "insufficient qty available for order") 
      })
    })
  }
}

vcr::use_cassette("cancel_all_final", {
  order_submit("cancel_all")  
})
 


