test_that("wide assertion uses value range", {
  result <- assertion_range_type(
    column = "instance_of",
    template = "wide_review"
  )

  # Wide assertion columns contain predicate values.
  expect_identical(
    result,
    "value"
  )
})

test_that("long subject uses subject range", {
  result <- assertion_range_type(
    column = "subject",
    template = "long_review"
  )

  # The explicit subject component uses the subject range.
  expect_identical(
    result,
    "subject"
  )
})

test_that("long predicate uses predicate range", {
  result <- assertion_range_type(
    column = "predicate",
    template = "long_review"
  )

  # The explicit predicate component uses the predicate range.
  expect_identical(
    result,
    "predicate"
  )
})

test_that("long value uses value range", {
  result <- assertion_range_type(
    column = "value",
    template = "long_review"
  )

  # The elementary value component uses the value range.
  expect_identical(
    result,
    "value"
  )
})

test_that("long numbered values use value range", {
  result1 <- assertion_range_type(
    column = "value1",
    template = "long_review"
  )

  result2 <- assertion_range_type(
    column = "value2",
    template = "long_review"
  )

  # Numbered value columns retain value semantics.
  expect_identical(
    result1,
    "value"
  )

  expect_identical(
    result2,
    "value"
  )
})

test_that("unknown long assertion has no range type", {
  result <- assertion_range_type(
    column = "certainty",
    template = "long_review"
  )

  # Arbitrary column names are not assigned assertion semantics.
  expect_null(result)
})
