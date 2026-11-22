# Default template --------------------------------------------------------

test_that("create_candidate_template() creates the default schema", {
  x <- create_candidate_template()

  expect_s3_class(x, "data.frame")
  expect_equal(nrow(x), 0L)
  expect_true(is.integer(x$row_id))
  expect_named(x, c(
    "row_id", "input_url", "input_media_url",
    "input_label", "input_description",
    "label", "description",
    "subject", "subject_range", "subject_definition"
  ))
})


# Optional standard columns ----------------------------------------------

test_that("optional standard columns can be omitted", {
  x <- create_candidate_template(
    input_url = FALSE,
    input_media_url = FALSE,
    input_label = FALSE,
    input_description = FALSE,
    label = FALSE,
    description = FALSE,
    subject_range = FALSE,
    subject_definition = FALSE
  )

  expect_named(x, c("row_id", "subject"))
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
  x <- create_candidate_template(input_predicate = TRUE)

  expect_true("input_predicate" %in% names(x))
  expect_true("input_predicate_range" %in% names(x))
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


# Row count ---------------------------------------------------------------

test_that("omitted n creates a header-only template", {
  x <- create_candidate_template()

  expect_equal(nrow(x), 0L)
  expect_identical(x$row_id, integer())
})

test_that("NULL and zero create header-only templates", {
  for (n in list(NULL, 0L, 0)) {
    x <- create_candidate_template(n = n)

    expect_equal(nrow(x), 0L)
    expect_identical(x$row_id, integer())
  }
})

test_that("positive n creates sequentially numbered rows", {
  for (n in c(1L, 3L, 70L)) {
    x <- create_candidate_template(n = n)

    expect_equal(nrow(x), n)
    expect_identical(x$row_id, seq_len(n))
    expect_true(is.integer(x$row_id))
  }
})

test_that("positive n preserves the template schema", {
  empty <- create_candidate_template(
    columns = "instance_of",
    context = "held_by"
  )

  populated <- create_candidate_template(
    n = 3L,
    columns = "instance_of",
    context = "held_by"
  )

  expect_named(populated, names(empty))
  expect_equal(nrow(populated), 3L)

  expect_true(all(vapply(
    populated[-1],
    \(x) is.character(x) && all(is.na(x)),
    logical(1)
  )))
})

test_that("invalid n values produce informative errors", {
  invalid_n <- list(
    -1L,
    1.5,
    NA_integer_,
    NaN,
    Inf,
    -Inf,
    "3",
    TRUE,
    c(1L, 2L),
    numeric()
  )

  for (n in invalid_n) {
    expect_error(
      create_candidate_template(n = n),
      "`n` must be a single, non-negative, finite whole number",
      fixed = TRUE
    )
  }
})
