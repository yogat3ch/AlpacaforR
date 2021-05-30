#' @include Assets.R
vcr::vcr_configure(dir = file.path(dirname(.log_path), "assets"))
vcr::use_cassette("assets_returns_a_well_known_ticker_symbol", {
test_that("assets returns a well-known ticker symbol", {
  .a <- assets("TSLA")
  expect_length(.a, 11)
  expect_identical(.a$symbol, "TSLA")
})
})

vcr::use_cassette("assets_warns_when_symbol_is_unknown", {
test_that("assets warns when symbol is unknown", {
  expect_warning(.a <- assets("blah"), "code: 404")
  expect_identical(.a, data.frame(symbol = "BLAH", status = "Not Found"))
})
})
