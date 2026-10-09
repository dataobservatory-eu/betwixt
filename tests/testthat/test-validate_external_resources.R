# URLs -------------------------------------------------------------------

test_that("pipe-separated input URLs are accepted", {
  x <- data.frame(
    row_id = 1L,
    subject = "Q121",
    input_url = "https://example.org/a| https://example.org/b"
  )

  expect_invisible(validate_external_resources(x))
})


test_that("invalid input media URLs are rejected", {
  x <- data.frame(
    row_id = 1L,
    subject = "Q121",
    input_media_url = "not-a-url"
  )

  expect_error(
    validate_external_resources(x),
    "Invalid URL in `input_media_url`"
  )
})


test_that("invalid input URLs are rejected", {
  x <- data.frame(
    row_id = 1L,
    subject = "Q121",
    input_url = "https://example.org/a | not-a-url"
  )

  expect_error(
    validate_external_resources(x),
    "Invalid URL in `input_url`"
  )
})


test_that("valid HTTP and HTTPS resources are accepted", {
  x <- create_candidate_template()
  x <- x[1L, , drop = FALSE]
  x$input_url <- "http://example.org/object"
  x$input_media_url <- "https://example.org/image.jpg"

  expect_invisible(validate_external_resources(x))
})

test_that("relative local paths are rejected", {
  x <- create_candidate_template()
  x <- x[1L, , drop = FALSE]
  x$input_media_url <- "images/photo_tier2.jpg"

  expect_error(
    validate_external_resources(x),
    "Invalid URL in `input_media_url`"
  )
})

test_that("file URLs are rejected", {
  x <- create_candidate_template()
  x <- x[1L, , drop = FALSE]
  x$input_media_url <- "file:///D:/images/photo.jpg"

  expect_error(
    validate_external_resources(x),
    "Invalid URL in `input_media_url`"
  )
})

test_that("missing resource references are accepted", {
  x <- create_candidate_template()
  x <- x[1L, , drop = FALSE]
  x$input_url <- NA_character_
  x$input_media_url <- NA_character_

  expect_invisible(validate_external_resources(x))
})
