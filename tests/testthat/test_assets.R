#' @include Assets.R
context("Test that Assets family of functions works properly")

vcr::use_cassette("assets returns a well-known ticker symbol", {
test_that("assets returns a well-known ticker symbol", {
  .a <- assets("TSLA")
  expect_length(.a, 10)
  expect_identical(.a$symbol, "TSLA")
})
})

vcr::use_cassette("assets warns when symbol is unknown", {
test_that("assets warns when symbol is unknown", {
  expect_warning(.a <- assets("blah"), "asset not found")
})
})
