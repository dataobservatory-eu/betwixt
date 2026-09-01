test_that("candidate_range() creates a canonical range", {
  expect_equal(
    candidate_range("depicts", "documents"),
    "depicts|documents"
  )

  expect_equal(
    candidate_range(c("depicts", "documents")),
    "depicts|documents"
  )
})


test_that("candidate_range() accepts pipe-delimited input", {
  expect_equal(
    candidate_range("depicts|documents"),
    "depicts|documents"
  )

  expect_equal(
    candidate_range("depicts | documents"),
    "depicts|documents"
  )

  expect_equal(
    candidate_range(" depicts | documents "),
    "depicts|documents"
  )
})


test_that("candidate_range() preserves spaces within values", {
  expect_equal(
    candidate_range(
      "depicts",
      "is evidence for",
      "Other…"
    ),
    "depicts|is evidence for|Other…"
  )
})


test_that("candidate_range() accepts mixed input", {
  expect_equal(
    candidate_range(
      "depicts | documents",
      "is evidence for",
      "Other…"
    ),
    "depicts|documents|is evidence for|Other…"
  )
})


test_that("candidate_range() handles missing values", {
  expect_equal(
    candidate_range(),
    NA_character_
  )

  expect_equal(
    candidate_range(NA_character_),
    NA_character_
  )

  expect_equal(
    candidate_range("depicts", NA_character_, "documents"),
    "depicts|documents"
  )
})


test_that("candidate_range() ignores empty values", {
  expect_equal(
    candidate_range("", "depicts", "", "documents"),
    "depicts|documents"
  )

  expect_equal(
    candidate_range("depicts||documents"),
    "depicts|documents"
  )
})
