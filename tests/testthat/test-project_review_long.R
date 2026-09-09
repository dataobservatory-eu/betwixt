## Structure -----------------------------------------------------------------

test_that("project_review_long() creates atomic assertions", {
  path <- test_path("fixtures", "muis-garments-review_1-finalised.html")
  x <- project_review_long(read_review(path))

  expect_s3_class(x, "data.frame")
  expect_identical(
    names(x),
    c(
      "assertion_number", "row_number", "subject", "predicate",
      "value", "status", "reviewer", "generated_at"
    )
  )
  expect_equal(nrow(x), 15)
  expect_equal(x$assertion_number, 1:15)
  expect_equal(x$row_number, rep(1:3, each = 5))
})

## Descriptive assertions ----------------------------------------------------

test_that("project_review_long() creates descriptive assertions", {
  path <- test_path("fixtures", "muis-garments-review_1-finalised.html")
  x <- project_review_long(read_review(path))
  x <- x[x$row_number == 1, ]

  expect_equal(
    x$predicate,
    c(
      "label", "description", "alternative_label",
      "alternative_description", "depicts"
    )
  )
  expect_equal(x$value[1], "sweater, women's")
  expect_equal(x$value[3], "női pulóver")
  expect_equal(x$status[3], "corrected")
})

## Semantic assertions -------------------------------------------------------

test_that("project_review_long() combines the semantic triplet", {
  path <- test_path("fixtures", "muis-garments-review_1-finalised.html")
  x <- project_review_long(read_review(path))

  assertion <- x[x$row_number == 1 & x$predicate == "depicts", ]

  expect_equal(nrow(assertion), 1)
  expect_equal(assertion$value, "300209900")
  expect_equal(assertion$status, "deferred")
})

## Subjects ------------------------------------------------------------------

test_that("project_review_long() uses the reviewed subject", {
  path <- test_path("fixtures", "muis-garments-review_1-finalised.html")
  review <- read_review(path)
  x <- project_review_long(review)

  row <- x[x$row_number == 1, ]

  expect_equal(nrow(row), 5)
  expect_true(all(row$subject == review$reviewed$subject[1]))
})

## Review provenance ---------------------------------------------------------

test_that("project_review_long() adds review provenance", {
  path <- test_path("fixtures", "muis-garments-review_1-finalised.html")
  review <- read_review(path)
  x <- project_review_long(review)

  expect_true(all(x$reviewer == review$provenance$reviewer))
  expect_true(all(x$generated_at == review$provenance$ended_at))
})

## Review states -------------------------------------------------------------

test_that("project_review_long() preserves assertion review states", {
  path <- test_path("fixtures", "muis-garments-review_1-finalised.html")
  x <- project_review_long(read_review(path))
  row <- x[x$row_number == 1, ]

  expect_equal(
    row$status,
    c(
      "corroborated", "corroborated", "corrected",
      "corroborated", "deferred"
    )
  )
})

## Row correspondence --------------------------------------------------------

test_that("project_review_long() preserves Betwixt row coordinates", {
  path <- test_path("fixtures", "muis-garments-review_1-finalised.html")
  x <- project_review_long(read_review(path))

  expect_equal(as.integer(table(x$row_number)), rep(5L, 3))
  expect_equal(length(unique(x$assertion_number)), nrow(x))
})
