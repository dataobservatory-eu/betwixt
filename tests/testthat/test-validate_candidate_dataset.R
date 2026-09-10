# Valid candidate datasets -----------------------------------------------

test_that("valid candidate template passes validation", {
  x <- create_candidate_template()

  expect_invisible(validate_candidate_dataset(x))
})

test_that("candidate and context columns may have arbitrary atomic types", {
  x <- data.frame(
    row_number = 1L,
    subject = "Q121",
    year = 1897L,
    verified = TRUE
  )

  expect_invisible(validate_candidate_dataset(x))
})


# Mandatory columns ------------------------------------------------------

test_that("row_number is required", {
  x <- create_candidate_template()
  x$row_number <- NULL

  expect_error(
    validate_candidate_dataset(x),
    "Missing required column.*row_number"
  )
})

test_that("subject is required", {
  x <- create_candidate_template()
  x$subject <- NULL

  expect_error(
    validate_candidate_dataset(x),
    "Missing required column.*subject"
  )
})


# Structural types -------------------------------------------------------

test_that("row_number must be integer", {
  x <- create_candidate_template()
  x$row_number <- numeric()

  expect_error(
    validate_candidate_dataset(x),
    "`row_number` must be integer"
  )
})

test_that("standard descriptive columns must be character", {
  x <- create_candidate_template()
  x$label <- integer()

  expect_error(
    validate_candidate_dataset(x),
    "Column.*must be character.*label"
  )
})

test_that("subject must be character", {
  x <- create_candidate_template()
  x$subject <- integer()

  expect_error(
    validate_candidate_dataset(x),
    "Column.*must be character.*subject"
  )
})


# Candidate metadata -----------------------------------------------------

test_that("range requires its candidate column", {
  x <- create_candidate_template()
  x$instance_of_range <- character()

  expect_error(
    validate_candidate_dataset(x),
    "Candidate metadata without candidate column.*instance_of"
  )
})

test_that("definition requires its candidate column", {
  x <- create_candidate_template()
  x$instance_of_definition <- character()

  expect_error(
    validate_candidate_dataset(x),
    "Candidate metadata without candidate column.*instance_of"
  )
})

test_that("candidate metadata passes with its candidate column", {
  x <- create_candidate_template(columns = "instance_of")

  expect_invisible(validate_candidate_dataset(x))
})



test_that("row_number cannot contain missing values", {
  x <- data.frame(
    row_number = c(1L, NA_integer_),
    subject = c("Q1", "Q2")
  )

  expect_error(
    validate_candidate_dataset(x),
    "`row_number` cannot contain missing values"
  )
})

test_that("row_number must be unique", {
  x <- data.frame(
    row_number = c(1L, 1L),
    subject = c("Q1", "Q2")
  )

  expect_error(
    validate_candidate_dataset(x),
    "`row_number` must be unique"
  )
})

test_that("row_number must be positive", {
  x <- data.frame(
    row_number = c(0L, 1L),
    subject = c("Q1", "Q2")
  )

  expect_error(
    validate_candidate_dataset(x),
    "`row_number` must contain positive integers"
  )
})

# URLs -------------------------------------------------------------------

# URLs -------------------------------------------------------------------

test_that("pipe-separated evidence URLs are accepted", {
  x <- data.frame(
    row_number = 1L,
    subject = "Q121",
    evidence_url = "https://example.org/a| https://example.org/b"
  )

  expect_invisible(validate_candidate_dataset(x))
})

test_that("invalid evidence URLs are rejected", {
  x <- data.frame(
    row_number = 1L,
    subject = "Q121",
    evidence_url = "https://example.org/a | not-a-url"
  )

  expect_error(
    validate_candidate_dataset(x),
    "Invalid URL in `evidence_url`"
  )
})


test_that("invalid evidence media URLs are rejected", {
  x <- data.frame(
    row_number = 1L,
    subject = "Q121",
    evidence_media_url = "not-a-url"
  )

  expect_error(
    validate_candidate_dataset(x),
    "Invalid URL in `evidence_media_url`"
  )
})
