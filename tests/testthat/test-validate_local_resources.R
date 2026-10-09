test_that("existing relative resources are accepted", {
  path <- tempfile()
  dir.create(path)
  on.exit(unlink(path, recursive = TRUE), add = TRUE)

  file.create(file.path(path, "photo.jpg"))

  x <- data.frame(
    input_media_url = "photo.jpg"
  )

  expect_invisible(validate_local_resources(x, path))
})

test_that("resources in subdirectories are accepted", {
  path <- tempfile()
  dir.create(file.path(path, "images"), recursive = TRUE)
  on.exit(unlink(path, recursive = TRUE), add = TRUE)

  file.create(file.path(path, "images", "photo.jpg"))

  x <- data.frame(
    input_media_url = "images/photo.jpg"
  )

  expect_invisible(validate_local_resources(x, path))
})

test_that("pipe-separated resources are accepted", {
  path <- tempfile()
  dir.create(path)
  on.exit(unlink(path, recursive = TRUE), add = TRUE)

  file.create(file.path(path, c("a.jpg", "b.jpg")))

  x <- data.frame(
    input_media_url = "a.jpg | b.jpg"
  )

  expect_invisible(validate_local_resources(x, path))
})

test_that("absolute paths are accepted", {
  path <- tempfile()
  dir.create(path)
  on.exit(unlink(path, recursive = TRUE), add = TRUE)

  resource <- file.path(path, "photo.jpg")
  file.create(resource)

  x <- data.frame(input_media_url = resource)

  expect_invisible(validate_local_resources(x, path))
})

test_that("missing resources are rejected", {
  path <- tempfile()
  dir.create(path)
  on.exit(unlink(path, recursive = TRUE), add = TRUE)

  x <- data.frame(
    input_media_url = "missing.jpg"
  )

  expect_error(
    validate_local_resources(x, path),
    "Missing local resource"
  )
})

test_that("missing resources in a list are rejected", {
  path <- tempfile()
  dir.create(path)
  on.exit(unlink(path, recursive = TRUE), add = TRUE)

  file.create(file.path(path, "a.jpg"))

  x <- data.frame(
    input_media_url = "a.jpg | missing.jpg"
  )

  expect_error(
    validate_local_resources(x, path),
    "missing.jpg"
  )
})

test_that("missing review directory is rejected", {
  path <- tempfile()

  x <- data.frame(
    input_media_url = "photo.jpg"
  )

  expect_error(
    validate_local_resources(x, path),
    "Review directory does not exist"
  )
})

test_that("missing resource references are accepted", {
  path <- tempfile()
  dir.create(path)
  on.exit(unlink(path, recursive = TRUE), add = TRUE)

  x <- data.frame(
    input_url = NA_character_,
    input_media_url = NA_character_
  )

  expect_invisible(validate_local_resources(x, path))
})

test_that("input_url is validated independently", {
  path <- tempfile()
  dir.create(path)
  on.exit(unlink(path, recursive = TRUE), add = TRUE)

  x <- data.frame(
    input_url = "missing.arw",
    input_media_url = NA_character_
  )

  expect_error(
    validate_local_resources(x, path),
    "Missing local resource.*input_url"
  )
})

test_that("empty resource columns are accepted", {
  path <- tempfile()
  dir.create(path)
  on.exit(unlink(path, recursive = TRUE), add = TRUE)

  x <- data.frame(
    input_url = character(),
    input_media_url = character()
  )

  expect_invisible(validate_local_resources(x, path))
})
