#' @include Websockets.R
#' @include internal.R

context("Websocket tests")
if (rlang::is_interactive()) {
  test_that("ws_create returns the appropriate websocket object", {
    .ws <- ws_create("Alpaca", log_msgs = F)
    expect_identical(class(.ws$ws), c("WebSocket", "R6"))
    expect_identical(attr(.ws, "api"), "a")
    .rs <- .ws$ws$readyState()[1]
    expect_equal(.rs, 0)
    .ws$ws$close()
    .ws <- ws_create("Polygon", log_msgs = F)
    expect_identical(class(.ws$ws), c("WebSocket", "R6"))
    expect_identical(attr(.ws, "api"), "p")
    expect_equal(.ws$ws$readyState()[1], 0)
    .ws$ws$close()
})
}
