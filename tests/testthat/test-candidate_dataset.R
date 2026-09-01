test_that("candidate_dataset() constructs the expected base structure", {
  result <- candidate_dataset(
    evidence_url = delini$evidence_url,
    evidence_text = delini$evidence_text,
    evidence_relation = rep("depicts", nrow(delini)),
    evidence_relation_range = rep(
      "depicts | documents | is evidence for | Other...",
      nrow(delini)
    ),
    label = delini$label,
    description = delini$description,
    subject = delini$col_1,
    subject_range = delini$col_1_range,
    subject_definition = delini$col_1_definition
  )

  expect_s3_class(result, "data.frame")

  expect_named(
    result,
    c(
      "row_number",
      "evidence_url",
      "evidence_text",
      "evidence_relation",
      "evidence_relation_range",
      "label",
      "description",
      "col_1",
      "col_1_range",
      "col_1_definition"
    )
  )

  expect_equal(nrow(result), nrow(delini))
})


test_that("candidate_dataset() preserves Delini input values", {
  relation <- rep("depicts", nrow(delini))

  relation_range <- rep(
    "depicts | documents | is evidence for | Other...",
    nrow(delini)
  )

  result <- candidate_dataset(
    evidence_url = delini$evidence_url,
    evidence_text = delini$evidence_text,
    evidence_relation = relation,
    evidence_relation_range = relation_range,
    label = delini$label,
    description = delini$description,
    subject = delini$col_1,
    subject_range = delini$col_1_range,
    subject_definition = delini$col_1_definition
  )

  expect_equal(result$evidence_url, delini$evidence_url)
  expect_equal(result$evidence_text, delini$evidence_text)
  expect_equal(result$evidence_relation, relation)
  expect_equal(result$evidence_relation_range, relation_range)
  expect_equal(result$label, delini$label)
  expect_equal(result$description, delini$description)

  expect_equal(result$col_1, delini$col_1)
  expect_equal(result$col_1_range, delini$col_1_range)
  expect_equal(result$col_1_definition, delini$col_1_definition)
})


test_that("candidate_dataset() creates integer row numbers in input order", {
  result <- candidate_dataset(
    evidence_url = delini$evidence_url,
    evidence_text = delini$evidence_text,
    evidence_relation = rep("depicts", nrow(delini)),
    label = delini$label,
    description = delini$description,
    subject = delini$col_1
  )

  expect_type(result$row_number, "integer")

  expect_identical(
    result$row_number,
    seq_len(nrow(delini))
  )

  expect_equal(
    result$evidence_text,
    delini$evidence_text
  )
})


test_that("candidate_dataset() uses NA defaults for optional subject metadata", {
  result <- candidate_dataset(
    evidence_url = rep(
      "https://www.w3.org/TR/vocab-data-cube/",
      nrow(w3c_life_expectancy)
    ),
    evidence_text = rep(
      "W3C RDF Data Cube Vocabulary",
      nrow(w3c_life_expectancy)
    ),
    evidence_relation = rep(
      "documents",
      nrow(w3c_life_expectancy)
    ),
    label = w3c_life_expectancy$observation,
    description = paste(
      "Life expectancy observation for",
      w3c_life_expectancy$area
    ),
    subject = w3c_life_expectancy$observation
  )

  expect_true(all(is.na(result$evidence_relation_range)))
  expect_true(all(is.na(result$col_1_range)))
  expect_true(all(is.na(result$col_1_definition)))

  expect_type(result$evidence_relation_range, "character")
  expect_type(result$col_1_range, "character")
  expect_type(result$col_1_definition, "character")
})


test_that("candidate_dataset() works with statistical source data", {
  result <- candidate_dataset(
    evidence_url = rep(
      "https://www.w3.org/TR/vocab-data-cube/",
      nrow(w3c_life_expectancy)
    ),
    evidence_text = rep(
      "W3C RDF Data Cube Vocabulary",
      nrow(w3c_life_expectancy)
    ),
    evidence_relation = rep(
      "documents",
      nrow(w3c_life_expectancy)
    ),
    label = w3c_life_expectancy$observation,
    description = paste(
      "Life expectancy observation for",
      w3c_life_expectancy$area
    ),
    subject = w3c_life_expectancy$observation
  )

  expect_equal(nrow(result), 4L)
  expect_equal(result$col_1, w3c_life_expectancy$observation)
  expect_equal(result$label, w3c_life_expectancy$observation)
})


test_that("candidate_dataset() integrates with add_candidate_column()", {
  result <- candidate_dataset(
    evidence_url = rep(
      "https://www.w3.org/TR/vocab-data-cube/",
      nrow(w3c_life_expectancy)
    ),
    evidence_text = rep(
      "W3C RDF Data Cube Vocabulary",
      nrow(w3c_life_expectancy)
    ),
    evidence_relation = rep(
      "documents",
      nrow(w3c_life_expectancy)
    ),
    label = w3c_life_expectancy$observation,
    description = paste(
      "Life expectancy observation for",
      w3c_life_expectancy$area
    ),
    subject = w3c_life_expectancy$observation
  ) |>
    add_candidate_column(
      value = w3c_life_expectancy$area
    ) |>
    add_candidate_column(
      value = w3c_life_expectancy$period
    ) |>
    add_candidate_column(
      value = w3c_life_expectancy$sex
    ) |>
    add_candidate_column(
      value = w3c_life_expectancy$life_expectancy
    )

  expect_equal(result$col_1, w3c_life_expectancy$observation)
  expect_equal(result$col_2, w3c_life_expectancy$area)
  expect_equal(result$col_3, w3c_life_expectancy$period)
  expect_equal(result$col_4, w3c_life_expectancy$sex)
  expect_equal(result$col_5, w3c_life_expectancy$life_expectancy)

  expect_equal(nrow(result), nrow(w3c_life_expectancy))
})


test_that("candidate_dataset() omits optional evidence relation columns", {
  result <- candidate_dataset(
    evidence_url = "https://example.org/evidence/1",
    evidence_text = "Evidence 1",
    label = "Example subject",
    description = "An example subject",
    subject = "example:Q1"
  )

  expect_false("evidence_relation" %in% names(result))
  expect_false("evidence_relation_range" %in% names(result))

  expect_equal(
    names(result),
    c(
      "row_number",
      "evidence_url",
      "evidence_text",
      "label",
      "description",
      "col_1",
      "col_1_range",
      "col_1_definition"
    )
  )
})


test_that("evidence relation range requires an evidence relation", {
  expect_error(
    candidate_dataset(
      evidence_url = "https://example.org/evidence/1",
      evidence_text = "Evidence 1",
      label = "Example subject",
      description = "An example subject",
      subject = "example:Q1",
      evidence_relation_range = candidate_range("depicts", "documents")
    ),
    "evidence_relation_range requires evidence_relation."
  )
})
