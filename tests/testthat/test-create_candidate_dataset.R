test_that("create_candidate_dataset() constructs the expected base structure", {
  result <- create_candidate_dataset(
    evidence_media_url = delini$evidence_media_url,
    evidence_text = delini$evidence_text,
    evidence_relation = rep("depicts", nrow(delini)),
    evidence_relation_range = rep(
      "depicts | documents | is evidence for | Other...",
      nrow(delini)
    ),
    label = delini$label,
    description = delini$description,
    subject = delini$subject,
    subject_range = delini$subject_range,
    subject_definition = delini$subject_definition
  )

  expect_s3_class(result, "data.frame")

  expect_named(
    result,
    c(
      "row_number",
      "evidence_url",
      "evidence_media_url",
      "evidence_text",
      "evidence_relation",
      "evidence_relation_range",
      "label",
      "description",
      "alternative_label",
      "alternative_description",
      "subject",
      "subject_range",
      "subject_definition"
    )
  )

  expect_equal(nrow(result), nrow(delini))
})


test_that("create_candidate_dataset() preserves Delini input values", {
  relation <- rep("depicts", nrow(delini))

  relation_range <- rep(
    "depicts | documents | is evidence for | Other...",
    nrow(delini)
  )

  result <- create_candidate_dataset(
    evidence_media_url = delini$evidence_media_url,
    evidence_text = delini$evidence_text,
    evidence_relation = relation,
    evidence_relation_range = relation_range,
    label = delini$label,
    description = delini$description,
    subject = delini$subject,
    subject_range = delini$subject_range,
    subject_definition = delini$subject_definition
  )

  expect_equal(result$evidence_media_url, delini$evidence_media_url)
  expect_true(all(is.na(result$evidence_url)))
  expect_equal(result$evidence_text, delini$evidence_text)
  expect_equal(result$evidence_relation, relation)
  expect_equal(result$evidence_relation_range, relation_range)
  expect_equal(result$label, delini$label)
  expect_equal(result$description, delini$description)

  expect_true(all(is.na(result$alternative_label)))
  expect_true(all(is.na(result$alternative_description)))

  expect_equal(result$subject, delini$subject)
  expect_equal(result$subject_range, delini$subject_range)
  expect_equal(result$subject_definition, delini$subject_definition)
})


test_that("create_candidate_dataset() creates integer row numbers in input order", {
  result <- create_candidate_dataset(
    evidence_media_url = delini$evidence_media_url,
    evidence_text = delini$evidence_text,
    evidence_relation = rep("depicts", nrow(delini)),
    label = delini$label,
    description = delini$description,
    subject = delini$subject
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


test_that("create_candidate_dataset() uses NA defaults for optional subject metadata", {
  result <- create_candidate_dataset(
    evidence_media_url = rep(
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

  # test default behavior
  expect_true(all(is.na(result$evidence_url)))
  expect_true(all(is.na(result$evidence_relation_range)))
  expect_true(all(is.na(result$subject_range)))
  expect_true(all(is.na(result$subject_definition)))

  # test types
  expect_type(result$evidence_url, "character")
  expect_type(result$subject_range, "character")
  expect_type(result$subject_definition, "character")
  expect_type(result$alternative_label, "character")
  expect_type(result$alternative_description, "character")
})


test_that("create_candidate_dataset() works with statistical source data", {
  result <- create_candidate_dataset(
    evidence_media_url = rep(
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
  expect_equal(result$subject, w3c_life_expectancy$observation)
  expect_equal(result$label, w3c_life_expectancy$observation)
})


test_that("create_candidate_dataset() integrates with add_candidate_column()", {
  result <- create_candidate_dataset(
    evidence_media_url = rep(
      "https://www.w3.org/TR/vocab-data-cube/",
      nrow(w3c_life_expectancy)
    ),
    evidence_text = rep(
      "W3C RDF Data Cube Vocabulary",
      nrow(w3c_life_expectancy)
    ),
    evidence_relation = rep("documents", nrow(w3c_life_expectancy)),
    label = w3c_life_expectancy$observation,
    description = paste(
      "Life expectancy observation for",
      w3c_life_expectancy$area
    ),
    subject = w3c_life_expectancy$observation
  ) |>
    add_candidate_column("area", w3c_life_expectancy$area) |>
    add_candidate_column("period", w3c_life_expectancy$period) |>
    add_candidate_column("sex", w3c_life_expectancy$sex) |>
    add_candidate_column(
      "life_expectancy",
      w3c_life_expectancy$life_expectancy
    )

  expect_equal(result$subject, w3c_life_expectancy$observation)
  expect_equal(result$area, w3c_life_expectancy$area)
  expect_equal(result$period, w3c_life_expectancy$period)
  expect_equal(result$sex, w3c_life_expectancy$sex)
  expect_equal(result$life_expectancy, w3c_life_expectancy$life_expectancy)
  expect_equal(nrow(result), nrow(w3c_life_expectancy))
})


test_that("create_candidate_dataset() omits optional evidence relation columns", {
  result <- create_candidate_dataset(
    evidence_media_url = "https://example.org/evidence/1",
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
      "evidence_media_url",
      "evidence_text",
      "label",
      "description",
      "alternative_label",
      "alternative_description",
      "subject",
      "subject_range",
      "subject_definition"
    )
  )
})


test_that("evidence relation range requires an evidence relation", {
  expect_error(
    create_candidate_dataset(
      evidence_media_url = "https://example.org/evidence/1",
      evidence_text = "Evidence 1",
      label = "Example subject",
      description = "An example subject",
      subject = "example:Q1",
      evidence_relation_range = add_candidate_range("depicts", "documents")
    ),
    "evidence_relation_range requires evidence_relation."
  )
})


test_that("create_candidate_dataset() accepts evidence_url without evidence_media_url", {
  result <- create_candidate_dataset(
    evidence_url = "https://example.org/evidence/1",
    evidence_text = "Evidence 1",
    label = "Example subject",
    description = "An example subject",
    subject = "example:Q1"
  )

  expect_true(is.na(result$evidence_media_url))
  expect_equal(
    result$evidence_url,
    "https://example.org/evidence/1"
  )
})

test_that("create_candidate_dataset() preserves alternative labels and descriptions", {
  result <- create_candidate_dataset(
    evidence_media_url = "https://example.org/evidence/1",
    evidence_text = "Evidence 1",
    label = "Tablet-woven sash",
    description = "A tablet-woven textile object.",
    alternative_label = "Tablet-woven belt",
    alternative_description = paste(
      "A visitor-facing description of the textile object."
    ),
    subject = "example:Q1"
  )

  expect_equal(
    result$alternative_label,
    "Tablet-woven belt"
  )

  expect_equal(
    result$alternative_description,
    "A visitor-facing description of the textile object."
  )
})


# -------------------------------------------------------------------------
# Provenance
# -------------------------------------------------------------------------

test_that("candidate dataset records provenance", {
  x <- create_candidate_dataset(
    subject = "Example",
    evidence_url = "https://example.org",
    data_manager_name = "Daniel Antal",
    data_manager_iri = "https://orcid.org/0000-0001-7513-6760",
    data_manager_email = "daniel@example.org",
    project_id = "example-project"
  )

  p <- attr(x, "provenance")

  expect_equal(p$data_manager, "Daniel Antal")
  expect_equal(p$data_manager_iri, "https://orcid.org/0000-0001-7513-6760")
  expect_equal(p$data_manager_email, "daniel@example.org")
  expect_equal(p$project_id, "example-project")
})

test_that("candidate provenance records generation metadata", {
  x <- create_candidate_dataset(
    subject = "Example",
    evidence_url = "https://example.org"
  )

  p <- attr(x, "provenance")

  expect_match(p$generated_at, "^\\d{4}-\\d{2}-\\d{2}T")
  expect_equal(p$software_agent, "Betwixt")
  expect_equal(
    p$software_version,
    as.character(utils::packageVersion("betwixt"))
  )
})
