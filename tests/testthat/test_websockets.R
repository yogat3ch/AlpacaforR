#' @include Websockets.R
#' @include internal.R

library(websocket)

context("Websocket tests")

test_that("ws_create returns the appropriate websocket object", {
  .ws <- ws_create("Alpaca", logfile = F)
  expect_identical(class(.ws$ws), c("WebSocket", "R6"))
  expect_identical(attr(.ws$ws, "api"), "a")
  expect_equal(.ws$ws$readyState()[1], 1)
  .ws <- ws_create("Polygon", logfile = F)
  expect_identical(class(.ws$ws), c("WebSocket", "R6"))
  expect_identical(attr(.ws$ws, "api"), "p")
  expect_equal(.ws$ws$readyState()[1], 1)
})

