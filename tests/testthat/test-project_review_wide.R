## Structure -----------------------------------------------------------------

test_that("project_review_wide() creates three aligned planes", {
  path <- test_path("fixtures", "muis-garments-review_1-finalised.html")
  review <- read_review(path)
  x <- project_review_wide(review)

  expect_s3_class(x, "data.frame")
  expect_equal(nrow(x), 3 * nrow(review$candidate))
  expect_equal(x$plane, rep(c("candidate", "reviewed", "status"), each = 3))
  expect_equal(x$row_number, rep(review$candidate$row_number, 3))
  expect_identical(names(x)[1:2], c("row_number", "plane"))
  expect_identical(
    names(x),
    append(names(review$candidate), "plane", after = 1)
  )
})

## Initial review ------------------------------------------------------------

test_that("project_review_wide() keeps initial assertions pending", {
  path <- test_path("fixtures", "muis-garments-review.html")
  x <- project_review_wide(read_review(path))
  status <- x[x$plane == "status", ]

  expect_equal(status$label, rep("pending", 3))
  expect_equal(status$alternative_label, rep("pending", 3))
  expect_equal(status$subject, rep("pending", 3))
  expect_equal(status$predicate, rep("pending", 3))
  expect_equal(status$value, rep("pending", 3))
})

## Draft review --------------------------------------------------------------

test_that("project_review_wide() preserves explicit qualifications", {
  path <- test_path("fixtures", "muis-garments-review_1-draft.html")
  x <- project_review_wide(read_review(path))
  status <- x[x$plane == "status", ]

  expect_equal(status$value, c("deferred", "pending", "corroborated"))
  expect_equal(status$predicate, c("pending", "rejected", "corroborated"))
})

## Finalised review ----------------------------------------------------------

test_that("project_review_wide() derives finalised review states", {
  path <- test_path("fixtures", "muis-garments-review_1-finalised.html")
  x <- project_review_wide(read_review(path))
  status <- x[x$plane == "status", ]

  expect_equal(
    status$alternative_label,
    c("corrected", "corroborated", "corroborated")
  )
  expect_equal(
    status$value,
    c("deferred", "corroborated", "corroborated")
  )
  expect_equal(status$predicate, rep("corroborated", 3))

  candidate <- x[x$plane == "candidate", ]
  reviewed <- x[x$plane == "reviewed", ]
  status <- x[x$plane == "status", ]

  expect_equal(candidate$alternative_label[1], "női kardigán")
  expect_equal(reviewed$alternative_label[1], "női pulóver")
  expect_equal(status$alternative_label[1], "corrected")
  expect_equal(status$value[1], "deferred")
})

## Descriptive assertions ----------------------------------------------------

test_that("project_review_wide() derives descriptive review states", {
  review <- list(
    candidate = data.frame(
      row_number = 1:4,
      description = c(NA, "Delete", "Change", "Keep")
    ),
    reviewed = data.frame(
      row_number = 1:4,
      description = c(NA, "", "Changed", "Keep"),
      finalised = rep(TRUE, 4)
    )
  )

  x <- project_review_wide(review)
  status <- x[x$plane == "status", ]

  expect_equal(
    status$description,
    c("deferred", "rejected", "corrected", "corroborated")
  )
})
