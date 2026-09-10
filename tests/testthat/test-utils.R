test_that("betwixt version works", {
  expect_true(is.character(betwixt_version()))
  expect_true(grepl(pattern = "0.0", x = betwixt_version()))
})
