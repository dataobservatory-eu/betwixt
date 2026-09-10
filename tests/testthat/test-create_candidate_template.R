# Default template --------------------------------------------------------

test_that("create_candidate_template() creates the default schema", {
  x <- create_candidate_template()

  expect_s3_class(x, "data.frame")
  expect_equal(nrow(x), 0L)
  expect_true(is.integer(x$row_number))
  expect_named(x, c(
    "row_number", "evidence_url", "evidence_media_url", "evidence_text",
    "label", "description", "subject", "subject_range", "subject_definition"
  ))
})


# Optional standard columns ----------------------------------------------

test_that("optional standard columns can be omitted", {
  x <- create_candidate_template(
    evidence_url = FALSE,
    evidence_media_url = FALSE,
    evidence_text = FALSE,
    label = FALSE,
    description = FALSE,
    subject_range = FALSE,
    subject_definition = FALSE
  )

  expect_named(x, c("row_number", "subject"))
})

test_that("alternative descriptive columns can be included", {
  x <- create_candidate_template(
    alternative_label = TRUE,
    alternative_description = TRUE
  )

  expect_true("alternative_label" %in% names(x))
  expect_true("alternative_description" %in% names(x))
})


# Evidence relation ------------------------------------------------------

test_that("evidence relation adds its candidate columns", {
  x <- create_candidate_template(evidence_relation = TRUE)

  expect_true("evidence_relation" %in% names(x))
  expect_true("evidence_relation_range" %in% names(x))
})


# Candidate columns ------------------------------------------------------

test_that("candidate columns expand to candidate triplets", {
  x <- create_candidate_template(
    columns = c("instance_of", "heritage_of")
  )

  expect_true(all(c(
    "instance_of", "instance_of_range", "instance_of_definition",
    "heritage_of", "heritage_of_range", "heritage_of_definition"
  ) %in% names(x)))
})

test_that("a single candidate column is supported", {
  x <- create_candidate_template(columns = "instance_of")

  expect_true(all(c(
    "instance_of", "instance_of_range", "instance_of_definition"
  ) %in% names(x)))
})


# Context columns --------------------------------------------------------

test_that("context columns are added without candidate metadata", {
  x <- create_candidate_template(context = c("held_by", "collection"))

  expect_true(all(c("held_by", "collection") %in% names(x)))
  expect_false("held_by_range" %in% names(x))
  expect_false("held_by_definition" %in% names(x))
})


# Combined template ------------------------------------------------------

test_that("candidate and context columns can be combined", {
  x <- create_candidate_template(
    columns = c("instance_of", "heritage_of"),
    context = "held_by"
  )

  expect_true(all(c(
    "instance_of", "instance_of_range", "instance_of_definition",
    "heritage_of", "heritage_of_range", "heritage_of_definition",
    "held_by"
  ) %in% names(x)))
})
