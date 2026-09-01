test_that("subject definitions identify resolved entities", {
  context <- prepare_review_context(betwixt::delini)

  expect_equal(
    context$rows[[1]]$assertions[[1]]$definition,
    "https://reprexbase.eu/fu/Item:Q7328"
  )

  expect_true(is.na(
    context$rows[[2]]$assertions[[1]]$definition
  ))
})
