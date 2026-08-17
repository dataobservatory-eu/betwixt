# Wide projection ---------------------------------------------------------

test_that("wide assertion columns use value ranges", {
  expect_identical(
    assertion_range_type(
      column = "instance_of",
      template = "wide_review"
    ),
    "value"
  )

  expect_identical(
    assertion_range_type(
      column = "inventory_number",
      template = "wide_review"
    ),
    "value"
  )
})


# Long projection ---------------------------------------------------------

test_that("long subject column uses subject range", {
  expect_identical(
    assertion_range_type(
      column = "subject",
      template = "long_review"
    ),
    "subject"
  )
})


test_that("long predicate column uses predicate range", {
  expect_identical(
    assertion_range_type(
      column = "predicate",
      template = "long_review"
    ),
    "predicate"
  )
})


test_that("long value column uses value range", {
  expect_identical(
    assertion_range_type(
      column = "value",
      template = "long_review"
    ),
    "value"
  )
})


test_that("long numbered value columns use value range", {
  expect_identical(
    assertion_range_type(
      column = "value1",
      template = "long_review"
    ),
    "value"
  )

  expect_identical(
    assertion_range_type(
      column = "value2",
      template = "long_review"
    ),
    "value"
  )

  expect_identical(
    assertion_range_type(
      column = "value10",
      template = "long_review"
    ),
    "value"
  )
})


test_that("long unrelated assertion column has no range type", {
  expect_null(
    assertion_range_type(
      column = "certainty",
      template = "long_review"
    )
  )
})
