# Fixtures ------------------------------------------------------------------

initial_path <- test_path("fixtures", "muis-garments-review.html")
draft_path <- test_path("fixtures", "muis-garments-review_1-draft.html")
final_path <- test_path("fixtures", "muis-garments-review_1-finalised.html")


# -------------------------------------------------------------------------
# Initial review
# -------------------------------------------------------------------------

test_that("read_review() reconstructs initial candidate state", {
  review <- read_review(initial_path)

  expect_equal(nrow(review$candidate), 3L)
  expect_equal(
    review$candidate$depicts,
    c("300209900", "300148821", "300212499")
  )
  expect_equal(
    review$candidate$context_held_by,
    rep("Estonian National Museum", 3L)
  )

  expect_equal(review$metadata$status, "in-progress")
  expect_equal(review$metadata$sequence, 0L)
})


# -------------------------------------------------------------------------
# Draft review
# -------------------------------------------------------------------------

test_that("read_review() reconstructs draft review state", {
  review <- read_review(draft_path)

  expect_equal(review$provenance$reviewer, "Daniel Antal")
  expect_equal(review$provenance$reviewer_email, "daniel@example.net")
  expect_equal(review$metadata$status, "in-progress")
  expect_equal(review$metadata$sequence, 1L)

  expect_identical(review$reviewed$finalised, c(FALSE, TRUE, TRUE))
  expect_equal(
    review$reviewed$depicts_qualification,
    c("defer", "reject", "none")
  )
  expect_equal(
    review$reviewed$outcome,
    c("defer", "reject", "accept")
  )

  expect_equal(
    review$reviewed$row_comment,
    c("I am not sure of this", NA, NA)
  )

  expect_equal(
    review$reviewed$context_held_by,
    rep("Estonian National Museum", 3L)
  )
})


# -------------------------------------------------------------------------
# Finalised review
# -------------------------------------------------------------------------

test_that("read_review() reconstructs finalised review state", {
  review <- read_review(final_path)

  expect_equal(review$metadata$status, "finalised")
  expect_equal(review$metadata$sequence, 1L)
  expect_true(all(review$reviewed$finalised))

  expect_equal(
    review$reviewed$outcome,
    rep("accept", 3L)
  )
  expect_equal(
    review$reviewed$depicts_qualification,
    rep("none", 3L)
  )

  expect_equal(review$provenance$review_comment, "Final version")
})


# -------------------------------------------------------------------------
# Candidate and reviewed values
# -------------------------------------------------------------------------

test_that("read_review() separates candidate and corrected values", {
  review <- read_review(final_path)

  expect_equal(review$candidate$alternative_label[1], "női kardigán")
  expect_equal(review$reviewed$alternative_label[1], "női pulóver")

  expect_equal(review$candidate$depicts[1], "300209900")
  expect_equal(review$reviewed$depicts[1], "300209900")
})

# -------------------------------------------------------------------------
# Context parser
# -------------------------------------------------------------------------


test_that("read_review() preserves context without qualifications", {
  review <- read_review(draft_path)

  expect_equal(
    review$candidate$context_held_by,
    rep("Estonian National Museum", 3L)
  )
  expect_equal(
    review$reviewed$context_held_by,
    rep("Estonian National Museum", 3L)
  )

  expect_false("context_held_by_qualification" %in%
    names(review$reviewed))
})

# -------------------------------------------------------------------------
# Provenance
# -------------------------------------------------------------------------


test_that("read_review() normalises missing candidate provenance", {
  path <- test_path(
    "fixtures",
    "small_countries_dataset_extended_1-draft.html"
  )

  review <- read_review(path)

  expect_false(is.na(review$provenance$generated_at))
  expect_equal(review$provenance$software_agent, "Betwixt")
  expect_equal(review$provenance$software_version, "0.0.7")
})
