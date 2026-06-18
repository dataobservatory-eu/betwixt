test_that("claim_context returns a list", {

  x <- claim(
    scope = "country=AD;year=2023",
    subject = "country",
    predicate = "GDP",
    value = "3.73 billion EUR"
  )

  context <- claim_context(x)

  expect_type(context, "list")
  expect_named(context, "claims")
})

test_that("claim_context contains one claim", {

  x <- claim(
    scope = "country=AD;year=2023",
    subject = "country",
    predicate = "GDP",
    value = "3.73 billion EUR"
  )

  context <- claim_context(x)

  expect_length(context$claims, 1)
})

test_that("claim_context preserves claim values", {

  x <- claim(
    scope = "country=AD;year=2023",
    subject = "country",
    predicate = "GDP",
    value = "3.73 billion EUR"
  )

  context <- claim_context(x)

  expect_equal(
    context$claims[[1]]$scope,
    "country=AD;year=2023"
  )

  expect_equal(
    context$claims[[1]]$subject,
    "country"
  )

  expect_equal(
    context$claims[[1]]$predicate,
    "GDP"
  )

  expect_equal(
    context$claims[[1]]$value,
    "3.73 billion EUR"
  )
})

test_that("claim_context rejects non claim_df objects", {

  expect_error(
    claim_context(data.frame(x = 1)),
    "claim_df"
  )
})
