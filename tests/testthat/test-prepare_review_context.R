# Basic context preparation ------------------------------------------------

test_that("prepare_review_context() prepares delini", {
  context <- prepare_review_context(delini)

  expect_equal(
    context$candidate_columns,
    c("subject", "instance_of", "heritage_of")
  )
  expect_equal(context$context_columns, "context_1")
  expect_false(context$has_evidence_relation)
  expect_length(context$rows, nrow(delini))

  row <- context$rows[[1]]

  expect_equal(row$row_number, delini$row_number[1])
  expect_equal(row$evidence_media_url, delini$evidence_media_url[1])
  expect_length(row$evidence_media_url, 1)
  expect_length(row$evidence_url, 0)
  expect_equal(row$evidence_text, delini$evidence_text[1])
  expect_equal(row$label, delini$label[1])
  expect_equal(row$description, delini$description[1])
  expect_true(is.na(row$alternative_label))
  expect_true(is.na(row$alternative_description))
  expect_null(row$evidence_relation)
  expect_null(row$evidence_relation_range)
})


# Candidate assertions -----------------------------------------------------

test_that("prepare_review_context() prepares assertions", {
  assertions <- prepare_review_context(delini)$rows[[1]]$assertions

  expect_length(assertions, 3)
  expect_equal(assertions[[1]]$name, "subject")
  expect_equal(assertions[[1]]$value, delini$subject[1])
  expect_equal(assertions[[2]]$value, delini$instance_of[1])
  expect_equal(assertions[[3]]$value, delini$heritage_of[1])

  expect_equal(
    assertions[[2]]$range,
    c("farmhouse", "tablet-woven sash", "bed", "record", "Other…")
  )

  expect_equal(
    assertions[[2]]$definition,
    delini$instance_of_definition[1]
  )
})


test_that("candidate definition works without a range", {
  candidate <- delini[, c(
    "row_number", "subject", "subject_definition",
    "instance_of", "instance_of_definition"
  )]

  context <- prepare_review_context(candidate)

  expect_equal(context$candidate_columns, c("subject", "instance_of"))
  expect_length(context$rows[[1]]$assertions[[2]]$range, 0)
})


test_that("candidate range works without a definition", {
  candidate <- delini[, c(
    "row_number", "subject", "subject_range",
    "instance_of", "instance_of_range"
  )]

  context <- prepare_review_context(candidate)

  expect_equal(context$candidate_columns, c("subject", "instance_of"))
  expect_true(is.na(context$rows[[1]]$assertions[[2]]$definition))
})


# Context columns ----------------------------------------------------------

test_that("prepare_review_context() prepares context columns", {
  row_context <- prepare_review_context(delini)$rows[[1]]$context

  expect_length(row_context, 1)
  expect_equal(row_context[[1]]$name, "context_1")
  expect_equal(row_context[[1]]$value, delini$context_1[1])
})


test_that("named context columns are recognised", {
  candidate <- delini
  candidate$context_year <- 2024L
  candidate$context_unit <- "CP_MEUR"

  context <- prepare_review_context(candidate)

  expect_equal(
    context$context_columns,
    c("context_1", "context_year", "context_unit")
  )
})


# Optional descriptive columns --------------------------------------------

test_that("optional descriptive columns may be absent", {
  candidate <- delini[, c(
    "row_number", "subject", "subject_range", "subject_definition"
  )]

  row <- prepare_review_context(candidate)$rows[[1]]

  expect_length(row$evidence_url, 0)
  expect_length(row$evidence_media_url, 0)
  expect_true(is.na(row$evidence_text))
  expect_true(is.na(row$label))
  expect_true(is.na(row$description))
  expect_true(is.na(row$alternative_label))
  expect_true(is.na(row$alternative_description))
})


test_that("alternative descriptive information is preserved", {
  candidate <- create_candidate_dataset(
    evidence_media_url = "https://example.com/001.jpg",
    evidence_text = "Example evidence",
    label = "Tablet-woven sash",
    description = "A tablet-woven textile object.",
    alternative_label = "Tablet-woven belt",
    alternative_description =
      "A visitor-facing description of the textile object.",
    subject = "example:Q1"
  )

  row <- prepare_review_context(candidate)$rows[[1]]

  expect_equal(row$alternative_label, "Tablet-woven belt")
  expect_equal(
    row$alternative_description,
    "A visitor-facing description of the textile object."
  )
})


# Evidence relations -------------------------------------------------------

test_that("prepare_review_context() supports evidence relations", {
  dual_delini <- delini %>%
    dplyr::mutate(
      evidence_relation = "depicts",
      evidence_relation_range = add_candidate_range("depicts", "documents")
    )

  context <- prepare_review_context(dual_delini)

  expect_true(context$has_evidence_relation)
  expect_length(context$rows, nrow(dual_delini))
  expect_equal(context$rows[[1]]$evidence_relation, "depicts")
  expect_equal(
    context$rows[[1]]$evidence_relation_range,
    c("depicts", "documents")
  )

  relations <- vapply(
    context$rows,
    function(row) row$evidence_relation,
    character(1)
  )
  expect_true(all(relations == "depicts"))

  ranges <- lapply(context$rows, function(row) row$evidence_relation_range)
  expected <- c("depicts", "documents")
  expect_true(all(vapply(ranges, identical, logical(1), expected)))
})


# Pipe-separated evidence --------------------------------------------------

test_that("prepare_review_context() parses multiple evidence URLs", {
  candidate <- create_candidate_dataset(
    evidence_media_url =
      "https://example.com/001.jpg | https://example.com/002.jpg",
    evidence_url =
      "https://example.com/001.html | https://example.com/002.html",
    evidence_text = "Multiple evidence resources",
    label = "Example subject",
    description = "An example subject",
    subject = "example:Q1"
  )

  row <- prepare_review_context(candidate)$rows[[1]]

  expect_equal(
    row$evidence_media_url,
    c("https://example.com/001.jpg", "https://example.com/002.jpg")
  )
  expect_equal(
    row$evidence_url,
    c("https://example.com/001.html", "https://example.com/002.html")
  )
})


test_that("pipe parsing tolerates surrounding whitespace", {
  candidate <- create_candidate_dataset(
    evidence_url = "https://example.com/a|  https://example.com/b",
    subject = "example:Q1"
  )

  expect_equal(
    prepare_review_context(candidate)$rows[[1]]$evidence_url,
    c("https://example.com/a", "https://example.com/b")
  )
})


# Reviewer comments --------------------------------------------------------

test_that("draft review preserves reviewer comments", {
  # Fixture with both row-level and overall reviewer comments.
  html <- render_review(
    small_countries_dataset,
    row_comment = TRUE,
    review_comment = TRUE
  )

  expect_match(html, "row-comment", fixed = TRUE)
  expect_match(html, "review-comment", fixed = TRUE)
})


test_that("finalised review preserves reviewer comments", {
  html <- render_review(
    small_countries_dataset,
    row_comment = TRUE,
    review_comment = TRUE
  )

  expect_match(html, "row-comment", fixed = TRUE)
  expect_match(html, "review-comment", fixed = TRUE)
})
