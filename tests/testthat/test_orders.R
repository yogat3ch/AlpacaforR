#' @import internal.R
#' @import Orders.R

context("Test that family: orders throws errors/messages as expected.")
order_submit(a = "cancel_all")
test_that("orders returns no orders properly.", {
  #assumes no open orders
  expect_type(expect_message(orders(), regexp = "No orders for the selected query/filter criteria. 
Check `ticker_id` or set status = 'all' to see all orders."),"list")
})

test_that("order_submit errors if type = limit and limit is not set", {
  expect_error(order_submit("AMZN", side = "sell", type = "limit", qty = 1), "Please set limit price.")
})

test_that("order_submit errors if type = stop and stop is not set", {
  expect_error(order_submit("AMZN", side = "sell", type = "stop", qty = 1), "Please set stop price.")
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

context("Test that family: orders functions effectively.")
test_that("order_submit properly places a simple buy order", {
  .o <<- order_submit("AMZN", qty = 2, side = "b", type = "m")
  .dts <- purrr::map_lgl(dplyr::select(.o, dplyr::ends_with("at")), lubridate::is.POSIXct)
  expect_equal(sum(.dts), 8)
  expect_equal(dim(.o), c(1,28))
})

test_that("orders properly detects the order", {
  .oo <- orders()
  expect_identical(.oo$id, .o$id)
  expect_equal(dim(.oo), c(1,28))
})
.lq <<- polygon("lq", symbol = "AMZN")
test_that("An expedited stop can be placed on the order with appropriate messages", {
  
  expect_message(.so <<- order_submit(.o$id, stop = .lq$askprice * .95, client = T), regexp = "side|qty|ticker_id|client_order_id|type", all = T)
  expect_identical(.so$client_order_id, .o$id)
})
.ms_closed <- polygon("ms")$market == "closed"
test_that("order_submit properly modifies the simple sell order", {
  .m <- "(?:unable to replace order, order is not open)|(?:unable to replace order, order isn't sent to exchange yet)"
  expect_warning({.r <<- order_submit(.so$id, action = "r", qty = 1, stop = .lq$askprice * .95)}, regexp = .m)
  if (.ms_closed) {
    expect_true(stringr::str_detect(.r$message, .m))
  } else {
    # when the market is open
    expect_identical(.r$replaces, .so$id)
    expect_identical(.r$qty, 1)
  }
  
})

test_that("order_submit properly places a bracket order_class", {
  expect_message(.bo <<- order_submit("AMZN", order_class = "bracket", qty = 2, take_profit = list(l = .lq$askprice * 1.03), stop_loss = list(l = .lq$askprice * .94, s = .lq$askprice * .95)), regexp = "buy|market", all = T)
  expect_equal(dim(.bo), c(2, 28))
  expect_equal(dim(.bo$legs), c(2, 28))
  .dts <- unlist(purrr::map_depth(.bo, -1, .ragged = T, lubridate::is.POSIXct))
  expect_equal(sum(.dts), 16)
})


test_that("order_submit properly cancels the bracket order", {
    expect_message({.co <<- order_submit(.bo$id, "c")}, "Order canceled successfully")
    expect_true(is.list(.co))
    expect_equal(length(.co), 0)
  
})

test_that("order_submit properly cancels the simple sell replacement order", {
  if (.ms_closed) {
    expect_error({.co <- order_submit(.r$id, "c")}, regexp = "`ticker_id` is NULL, the order may not have been placed successfully?")
  } else {
    expect_message({.co <- order_submit(.r$id, "c")}, "Order canceled successfully")
    expect_true(is.list(.co))
    expect_equal(length(.co), 0)
  }
  
})

test_that("order_submit properly places an oco order_class", {
  .o <- order_submit("AMZN", qty = 2, side = "b", type = "m")
  expect_message(.oco <- order_submit("AMZN", order_class = "oco", qty = 2, take_profit = list(l = .lq$askprice * 1.03), stop_loss = list(s = .lq$askprice * .96)), regexp = "sell|limit", all = T)
})


test_that("order_submit properly places an oto order_class", {
  .lq <- polygon("lq", symbol = "BYND")
  expect_message(.oto <- order_submit("BYND", order_class = "oto", qty = 2, stop_loss = list(s = .lq$askprice * .96)), regexp = "oto", all = T)
})

test_that("order_submit properly cancels all open orders",{
  .ca <- order_submit(a = "cancel_all")  
expect_true(all(.ca$status %in% "pending_cancel"))
expect_true(nrow(.ca) > 1)
expect_s3_class(do.call(c, dplyr::select(.ca, dplyr::ends_with("at"))), "POSIXct")
  })
