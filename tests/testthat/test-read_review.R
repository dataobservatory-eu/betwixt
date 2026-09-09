## Fixtures ------------------------------------------------------------------

initial_path <- test_path("fixtures", "muis-garments-review.html")
draft_path <- test_path("fixtures", "muis-garments-review_1-draft.html")
final_path <- test_path("fixtures", "muis-garments-review_1-finalised.html")

## Candidate state -----------------------------------------------------------

test_that("read_review() reconstructs initial candidate state", {
  review <- read_review(initial_path)

  expect_identical(review$metadata$sequence, 0L)
  expect_equal(review$metadata$status, "in-progress")
  expect_equal(nrow(review$candidate), 3L)

  expect_equal(
    review$candidate$label,
    c("sweater, women's", "gloves", "shirt, women's")
  )
  expect_equal(
    review$candidate$alternative_label,
    c("női kardigán", "kesztyű", "női ing")
  )
  expect_equal(review$candidate$predicate, rep("depicts", 3L))
  expect_equal(
    review$candidate$value,
    c("300209900", "300148821", "300212499")
  )

  expect_true(all(review$reviewed$finalised == FALSE))
  expect_true(all(review$reviewed$outcome == "accept"))
})

## Draft state ---------------------------------------------------------------

test_that("read_review() reconstructs draft review state", {
  review <- read_review(draft_path)

  expect_identical(review$metadata$sequence, 1L)
  expect_equal(review$metadata$status, "in-progress")
  expect_equal(review$provenance$reviewer, "Daniel Antal")
  expect_equal(review$provenance$reviewer_email, "daniel@example.com")
  expect_true(is.na(review$provenance$ended_at))

  expect_identical(review$reviewed$finalised, c(FALSE, FALSE, TRUE))
  expect_equal(review$reviewed$outcome, c("defer", "reject", "accept"))
  expect_equal(
    review$reviewed$value_qualification,
    c("defer", "none", "none")
  )
  expect_equal(
    review$reviewed$predicate_qualification,
    c("none", "reject", "none")
  )
  expect_equal(
    review$reviewed$row_comment,
    c("the translation needs fixing", NA, "This is correct")
  )
})

## Finalised state -----------------------------------------------------------

test_that("read_review() reconstructs finalised review state", {
  review <- read_review(final_path)

  expect_identical(review$metadata$sequence, 1L)
  expect_equal(review$metadata$status, "finalised")
  expect_false(is.na(review$provenance$ended_at))

  expect_identical(review$reviewed$finalised, rep(TRUE, 3L))
  expect_equal(review$reviewed$outcome, c("defer", "accept", "accept"))
  expect_equal(
    review$reviewed$value_qualification,
    c("defer", "none", "none")
  )
  expect_equal(
    review$reviewed$predicate_qualification,
    rep("none", 3L)
  )
})

## Candidate and reviewed values --------------------------------------------

test_that("read_review() separates candidate and corrected values", {
  review <- read_review(final_path)

  expect_equal(review$candidate$alternative_label[1], "női kardigán")
  expect_equal(review$reviewed$alternative_label[1], "női pulóver")
  expect_equal(review$candidate$value[1], "300209900")
  expect_equal(review$reviewed$value[1], "300209900")
})
