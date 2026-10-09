# Local HTML review fixtures ---------------------------------------------

local_review_fixture <- function() {
  path <- tempfile("betwixt-local-")
  dir.create(file.path(path, "media"), recursive = TRUE)

  file.create(file.path(path, "media", "photo.jpg"))
  file.create(file.path(path, "media", "record.jpg"))

  candidate <- create_candidate_dataset(
    subject = c("urn:test:artefact:1", "urn:test:record:1"),
    input_media_url = c("media/photo.jpg", "media/record.jpg"),
    label = c("Delini sash", "Delini floor plan"),
    description = c("A woven sash", "A farmhouse floor plan")
  )

  list(path = path, candidate = candidate)
}

test_that("local HTML review is written to the output directory", {
  fixture <- local_review_fixture()
  on.exit(unlink(fixture$path, recursive = TRUE), add = TRUE)

  expect_message(
    html <- render_local_html_review(
      fixture$candidate,
      path = fixture$path
    ),
    "Review rendered:"
  )

  output <- file.path(fixture$path, "betwixt-review.html")

  expect_true(file.exists(output))
  expect_type(html, "character")
  expect_true(nzchar(html))
})

test_that("local HTML preserves relative media references", {
  fixture <- local_review_fixture()
  on.exit(unlink(fixture$path, recursive = TRUE), add = TRUE)

  suppressMessages(render_local_html_review(
    fixture$candidate,
    path = fixture$path
  ))

  html <- paste(
    readLines(file.path(fixture$path, "betwixt-review.html")),
    collapse = "\n"
  )

  expect_match(html, "media/photo.jpg", fixed = TRUE)
  expect_match(html, "media/record.jpg", fixed = TRUE)
})

test_that("local HTML contains candidate labels", {
  fixture <- local_review_fixture()
  on.exit(unlink(fixture$path, recursive = TRUE), add = TRUE)

  html <- suppressMessages(render_local_html_review(
    fixture$candidate,
    path = fixture$path
  ))

  expect_match(html, "Delini sash", fixed = TRUE)
  expect_match(html, "Delini floor plan", fixed = TRUE)
})

test_that("local HTML respects filename and sequence", {
  fixture <- local_review_fixture()
  on.exit(unlink(fixture$path, recursive = TRUE), add = TRUE)

  suppressMessages(render_local_html_review(
    fixture$candidate,
    filename_stem = "delini-review",
    sequence = 2L,
    path = fixture$path
  ))

  expect_true(file.exists(
    file.path(fixture$path, "delini-review_2.html")
  ))
})

test_that("local HTML rejects missing media resources", {
  fixture <- local_review_fixture()
  on.exit(unlink(fixture$path, recursive = TRUE), add = TRUE)

  fixture$candidate$input_media_url[1] <- "media/missing.jpg"

  expect_error(
    render_local_html_review(
      fixture$candidate,
      path = fixture$path
    ),
    "Missing local resource"
  )
})

test_that("local HTML rejects nonexistent output directories", {
  fixture <- local_review_fixture()
  on.exit(unlink(fixture$path, recursive = TRUE), add = TRUE)

  expect_error(
    render_local_html_review(
      fixture$candidate,
      path = file.path(fixture$path, "missing")
    ),
    "Review directory does not exist"
  )
})

test_that("local HTML rejects invalid review sequences", {
  fixture <- local_review_fixture()
  on.exit(unlink(fixture$path, recursive = TRUE), add = TRUE)

  expect_error(
    render_local_html_review(
      fixture$candidate,
      sequence = -1L,
      path = fixture$path
    ),
    "sequence must be"
  )
})

test_that("render_review dispatches to local HTML", {
  fixture <- local_review_fixture()
  on.exit(unlink(fixture$path, recursive = TRUE), add = TRUE)

  suppressMessages(render_review(
    fixture$candidate,
    type = "local_html",
    filename_stem = "dispatch-test",
    path = fixture$path
  ))

  expect_true(file.exists(
    file.path(fixture$path, "dispatch-test.html")
  ))
})
