test_that("prepare_review_context() prepares delini", {
  context <- prepare_review_context(delini)

  # Check the discovered candidate and context columns.
  expect_equal(context$candidate_columns, c("col_1", "col_2", "col_3"))
  expect_equal(context$context_columns, "context_1")

  # Check that ordinary Delini has no evidence relation.
  expect_false(context$has_evidence_relation)
  expect_null(context$rows[[1]]$evidence_relation)
  expect_null(context$rows[[1]]$evidence_relation_range)

  # Check the number of prepared rows.
  expect_length(context$rows, nrow(delini))

  # Check the common contents of the first row.
  expect_equal(context$rows[[1]]$row_number, delini$row_number[1])
  expect_equal(context$rows[[1]]$evidence_url, delini$evidence_url[1])
  expect_equal(context$rows[[1]]$evidence_text, delini$evidence_text[1])
  expect_equal(context$rows[[1]]$label, delini$label[1])
  expect_equal(context$rows[[1]]$description, delini$description[1])
})


test_that("prepare_review_context() prepares candidate assertions", {
  context <- prepare_review_context(delini)
  assertions <- context$rows[[1]]$assertions

  # Check that all candidate columns become assertions.
  expect_length(assertions, 3)
  expect_equal(assertions[[1]]$name, "col_1")
  expect_equal(assertions[[1]]$value, delini$col_1[1])
  expect_equal(assertions[[2]]$value, delini$col_2[1])
  expect_equal(assertions[[3]]$value, delini$col_3[1])

  # Check that candidate ranges are parsed for rendering.
  expect_equal(
    assertions[[2]]$range,
    c("farmhouse", "tablet-woven sash", "bed", "record", "Other…")
  )

  # Check that semantic definitions are preserved.
  expect_equal(assertions[[2]]$definition, delini$col_2_definition[1])
})


test_that("prepare_review_context() prepares context columns", {
  context <- prepare_review_context(delini)
  row_context <- context$rows[[1]]$context

  # Check that contextual information is preserved.
  expect_length(row_context, 1)
  expect_equal(row_context[[1]]$name, "context_1")
  expect_equal(row_context[[1]]$value, delini$context_1[1])
})


test_that("prepare_review_context() supports evidence relations", {
  # Add an optional evidence relation to the Delini candidate dataset.
  dual_delini <- delini %>%
    dplyr::mutate(
      evidence_relation = "depicts",
      evidence_relation_range = candidate_range("depicts", "documents")
    )

  context <- prepare_review_context(dual_delini)

  # Check that the optional evidence relation is detected.
  expect_true(context$has_evidence_relation)
  expect_length(context$rows, nrow(dual_delini))

  # Check that the relation and its parsed range are prepared for rendering.
  expect_equal(context$rows[[1]]$evidence_relation, "depicts")
  expect_equal(
    context$rows[[1]]$evidence_relation_range,
    c("depicts", "documents")
  )

  # Check that the evidence relation is preserved for every row.
  relations <- vapply(
    context$rows, function(row) row$evidence_relation, character(1)
  )
  expect_true(all(relations == "depicts"))

  # Check that the evidence relation range is preserved for every row.
  ranges <- lapply(
    context$rows, function(row) row$evidence_relation_range
  )
  expect_true(all(vapply(
    ranges, identical, logical(1), c("depicts", "documents")
  )))
})
