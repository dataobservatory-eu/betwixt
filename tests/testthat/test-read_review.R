## Candidate state tests ------------------------------------------------------

test_that("read_review() reads initial review metadata", {
  path <- tempfile(fileext = ".html")

  betwixt_render(
    delini,
    title = "Test review",
    description = "Review these claims.",
    project_id = "test-project",
    filename_stem = "test-review",
    sequence = 0L,
    path = dirname(path)
  )

  actual_path <- file.path(dirname(path), "test-review.html")

  review <- read_review(actual_path)

  expect_equal(review$metadata$title, "Test review")
  expect_equal(review$metadata$description, "Review these claims.")
  expect_equal(review$metadata$project_id, "test-project")
  expect_identical(review$metadata$sequence, 0L)
  expect_equal(review$metadata$status, "in-progress")

  expect_equal(review$provenance$original_filename, "test-review.html")
  expect_equal(review$provenance$filename, "test-review.html")

  expect_match(
    review$provenance$original_created_at,
    "^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}Z$"
  )

  expect_true(is.na(review$provenance$reviewer))
  expect_true(is.na(review$provenance$reviewer_email))
  expect_true(is.na(review$provenance$started_at))
  expect_true(is.na(review$provenance$saved_at))
  expect_true(is.na(review$provenance$ended_at))
})

# Validation tests ------------------------------------------------------------

test_that("read_review() validates its input", {
  expect_error(
    read_review("does-not-exist.html"),
    "Review file does not exist"
  )

  path <- tempfile(fileext = ".html")
  writeLines("<html><body>Not Betwixt</body></html>", path)

  expect_error(
    read_review(path),
    "does not contain a Betwixt review table"
  )
})

test_that("read_review() recognises an empty review comment", {
  betwixt_render(delini,
    review_comment = TRUE,
    filename_stem = "test-review-comment", path = tempdir()
  )

  review <- read_review(
    file.path(tempdir(), "test-review-comment.html")
  )

  expect_true(is.na(review$provenance$review_comment))
})

test_that("read_review() reads a review without evidence", {
  path <- tempdir()

  x <- candidate_dataset(
    subject = c("House", "Sash", "Bed")
  )

  betwixt_render(
    x,
    filename_stem = "no-evidence",
    path = path
  )

  review <- read_review(
    file.path(path, "no-evidence.html")
  )

  expect_identical(review$candidate$row_number, 1:3)
  expect_identical(review$reviewed$row_number, 1:3)

  expect_true(all(is.na(review$candidate$evidence_url)))
  expect_true(all(is.na(review$candidate$evidence_media_url)))
  expect_true(all(is.na(review$candidate$evidence_text)))

  expect_identical(
    review$candidate$col_1,
    c("House", "Sash", "Bed")
  )
})

# Candidate data retention ---------------------------------------------------

test_that("read_review() reconstructs candidate data", {
  betwixt_render(
    delini,
    filename_stem = "test-candidate", path = tempdir()
  )
  review <- read_review(file.path(tempdir(), "test-candidate.html"))

  expect_s3_class(review$candidate, "data.frame")
  expect_equal(nrow(review$candidate), nrow(delini))
  expect_identical(review$candidate$row_number, delini$row_number)
  expect_equal(review$candidate$evidence_url, delini$evidence_url)
  expect_equal(review$candidate$evidence_media_url, delini$evidence_media_url)
  expect_equal(review$candidate$evidence_text, delini$evidence_text)
  expect_equal(review$candidate$label, delini$label)
  expect_equal(review$candidate$description, delini$description)
  expect_equal(review$candidate$col_1, delini$col_1)
  expect_equal(review$candidate$col_2, delini$col_2)
})

# Reviewed data retention ----------------------------------------------------

test_that("read_review() reconstructs initial reviewed data", {
  betwixt_render(
    delini,
    row_comment = TRUE, filename_stem = "test-reviewed",
    path = tempdir()
  )
  review <- read_review(file.path(tempdir(), "test-reviewed.html"))

  expect_s3_class(review$reviewed, "data.frame")
  expect_equal(nrow(review$reviewed), nrow(delini))
  expect_identical(review$reviewed$row_number, delini$row_number)
  expect_equal(review$reviewed$evidence_url, delini$evidence_url)
  expect_equal(review$reviewed$evidence_media_url, delini$evidence_media_url)
  expect_equal(review$reviewed$evidence_text, delini$evidence_text)
  expect_equal(review$reviewed$label, delini$label)
  expect_equal(review$reviewed$description, delini$description)
  expect_equal(review$reviewed$col_1, delini$col_1)
  expect_equal(review$reviewed$col_2, delini$col_2)
  expect_true(all(is.na(review$reviewed$row_comment)))
})


# Persisted reviewed state ---------------------------------------------------

test_that("read_review() reconstructs reviewed decisions", {
  path <- test_path(
    "fixtures", "muis-garments-review_1-finalised.html"
  )
  review <- read_review(path)

  expect_identical(review$reviewed$finalised, c(TRUE, TRUE, TRUE))
  expect_equal(review$reviewed$outcome, c("accept", "defer", "reject"))
  expect_equal(review$reviewed$row_comment, c("ok", "maybe", "rejected"))

  expect_equal(
    review$reviewed$col_3_qualification,
    c("none", "none", "reject")
  )
  expect_equal(
    review$reviewed$col_4_qualification,
    c("none", "defer", "none")
  )
  expect_equal(review$reviewed$col_1[1], "fuds:QNEW1")
})

# Persisted draft state ------------------------------------------------------

test_that("read_review() reconstructs draft review state", {
  path <- test_path(
    "fixtures", "muis-garments-review_1-draft.html"
  )
  review <- read_review(path)

  expect_identical(review$metadata$sequence, 1L)
  expect_equal(review$metadata$status, "in-progress")
  expect_equal(review$provenance$reviewer, "Daniel")
  expect_equal(review$provenance$reviewer_email, "daniel@example.com")
  expect_true(is.na(review$provenance$ended_at))

  expect_identical(review$reviewed$finalised, c(TRUE, FALSE, FALSE))
  expect_equal(review$reviewed$outcome, c("accept", "defer", "accept"))
  expect_equal(review$reviewed$col_1[1], "fuds:QNEW1")
  expect_equal(
    review$reviewed$col_4_qualification,
    c("none", "defer", "none")
  )
})

test_that("read_review() separates candidate and edited values", {
  path <- test_path("fixtures", "muis-garments-review_1-edited.html")
  review <- read_review(path)

  expect_equal(review$candidate$label[1], "sweater, women's")
  expect_equal(review$reviewed$label[1], "Women's sweater")
  expect_equal(review$candidate$col_1[1], "[image shown]")
  expect_equal(review$reviewed$col_1[1], "fuds:QNEW1")
})
