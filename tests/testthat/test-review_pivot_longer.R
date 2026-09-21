final_path <- test_path(
  "fixtures",
  "muis-garments-review_1-finalised.html"
)

## Subjects -----------------------------------------------------------------

test_that("review_pivot_longer() preserves candidate subjects", {
  x <- create_candidate_dataset(
    input_media_url = delini$input_media_url,
    input_predicate = rep("depicts", nrow(delini)),
    label = delini$label,
    description = delini$description,
    subject = delini$subject
  )

  result <- review_pivot_longer(x)
  domain <- result[result$component == "domain", ]

  expect_equal(
    unique(domain$subject),
    delini$subject
  )
})
test_that("review_pivot_longer() preserves MuIS subjects", {
  review <- read_review(final_path)

  result <- review |>
    project_review_wide() |>
    review_pivot_longer()

  domain <- result[result$component == "domain", ]

  expect_equal(
    unique(domain$subject),
    review$candidate$subject
  )
})

test_that("review_pivot_longer() pivots input assertions", {
  x <- create_candidate_dataset(
    input_media_url = delini$input_media_url,
    input_description = delini$input_description,
    input_predicate = rep("depicts", nrow(delini)),
    input_predicate_range = rep(
      "depicts | documents | is evidence for | Other...",
      nrow(delini)
    ),
    label = delini$label,
    description = delini$description,
    subject = delini$subject,
    subject_range = delini$subject_range,
    subject_definition = delini$subject_definition,
    project_id = "test"
  )

  result <- review_pivot_longer(x)
  input <- result[result$component == "input", ]

  expect_equal(nrow(input), nrow(delini))
  expect_equal(input$row_id, seq_len(nrow(delini)))
  expect_equal(input$subject, delini$subject)
  expect_equal(input$predicate, rep("depicts", nrow(delini)))
  expect_equal(input$value, delini$input_media_url)

  expect_false(any(result$predicate == "input_description"))
  expect_false(any(result$predicate == "input_predicate_range"))
  expect_false(any(result$predicate == "subject_range"))
  expect_false(any(result$predicate == "subject_definition"))
})


test_that("review_pivot_longer() pivots added candidate columns", {
  x <- create_candidate_dataset(
    input_media_url = rep(
      "https://www.w3.org/TR/vocab-data-cube/",
      nrow(w3c_life_expectancy)
    ),
    input_description = rep(
      "W3C RDF Data Cube Vocabulary",
      nrow(w3c_life_expectancy)
    ),
    input_predicate = rep("documents", nrow(w3c_life_expectancy)),
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

  result <- review_pivot_longer(x)
  domain <- result[result$component == "domain", ]

  expect_equal(
    unique(domain$predicate),
    c(
      "label", "description", "area", "period",
      "sex", "life_expectancy"
    )
  )

  expect_equal(
    domain$value[domain$predicate == "area"],
    as.character(w3c_life_expectancy$area)
  )
  expect_equal(
    domain$value[domain$predicate == "period"],
    as.character(w3c_life_expectancy$period)
  )
  expect_equal(
    domain$value[domain$predicate == "sex"],
    as.character(w3c_life_expectancy$sex)
  )
  expect_equal(
    domain$value[domain$predicate == "life_expectancy"],
    as.character(w3c_life_expectancy$life_expectancy)
  )
})


test_that("review_pivot_longer() pivots alternative descriptions", {
  x <- create_candidate_dataset(
    input_media_url = "https://example.org/evidence/1",
    input_description = "Evidence 1",
    label = "Tablet-woven sash",
    description = "A tablet-woven textile object.",
    alternative_label = "Tablet-woven belt",
    alternative_description =
      "A visitor-facing description of the textile object.",
    subject = "example:Q1"
  )

  result <- review_pivot_longer(x)
  domain <- result[result$component == "domain", ]

  expect_equal(
    domain$predicate,
    c(
      "label", "description",
      "alternative_label", "alternative_description"
    )
  )

  expect_equal(
    domain$value,
    c(
      "Tablet-woven sash",
      "A tablet-woven textile object.",
      "Tablet-woven belt",
      "A visitor-facing description of the textile object."
    )
  )

  expect_true(all(domain$subject == "example:Q1"))
})


test_that("review_pivot_longer() pivots context from a review projection", {
  review <- read_review(final_path)

  result <- review |>
    project_review_wide() |>
    review_pivot_longer()

  context <- result[result$component == "context", ]

  expect_equal(nrow(context), 3L)
  expect_equal(context$row_id, c(1L, 2L, 3L))
  expect_equal(
    context$subject,
    review$candidate$subject
  )
  expect_equal(
    context$predicate,
    rep("context_held_by", 3L)
  )
  expect_equal(
    context$value,
    rep("Estonian National Museum", 3L)
  )
})


test_that("review_pivot_longer() preserves reviewed assertion provenance", {
  review <- read_review(final_path)

  result <- review |>
    project_review_wide() |>
    review_pivot_longer()

  provenance <- result[
    result$component == "assertion_provenance",
  ]

  expect_true(nrow(provenance) > 0L)

  corrected <- provenance[
    provenance$row_id == 1L &
      provenance$predicate == "alternative_label",
  ]

  expect_equal(nrow(corrected), 1L)
  expect_true(grepl("női kardigán", corrected$value, fixed = TRUE))
  expect_true(grepl("női pulóver", corrected$value, fixed = TRUE))
})


test_that("review_pivot_longer() returns canonical columns", {
  review <- read_review(final_path)

  result <- review |>
    project_review_wide() |>
    review_pivot_longer()

  expect_identical(
    names(result),
    c(
      "row_id", "assertion_id", "component",
      "subject", "predicate", "value"
    )
  )

  expect_identical(
    result$assertion_id,
    seq_len(nrow(result))
  )
})

test_that("review_pivot_longer() rejects invalid representations", {
  x <- data.frame(foo = "bar")

  expect_error(
    review_pivot_longer(x),
    "Invalid Betwixt representation."
  )
})


test_that("review_pivot_longer() projects dataset identity", {
  x <- create_candidate_dataset(
    input_media_url = "https://example.org/input/1",
    label = "Example",
    subject = "example:Q1",
    project_id = "test-project"
  )

  result <- review_pivot_longer(x)
  dataset <- result[result$component == "dataset_identity", ]

  expect_true(any(
    dataset$predicate == "type" &
      dataset$value == "schema:Dataset"
  ))

  expect_true(any(
    dataset$predicate == "project_id" &
      dataset$value == "test-project"
  ))

  expect_true(all(dataset$row_id == 0L))
})


test_that("review_pivot_longer() atomises pipe-separated domain values", {
  x <- create_candidate_dataset(
    input_media_url = "https://example.org/input/1",
    label = "First | Second",
    subject = "example:Q1"
  )

  result <- review_pivot_longer(x)
  label <- result[
    result$component == "domain" &
      result$predicate == "label",
  ]

  expect_equal(label$subject, rep("example:Q1", 2L))
  expect_equal(label$value, c("First", "Second"))
})


test_that("review_pivot_longer() atomises pipe-separated input values", {
  x <- create_candidate_dataset(
    input_media_url = "https://example.org/1 | https://example.org/2",
    input_predicate = "documents",
    label = "Example",
    subject = "example:Q1"
  )

  result <- review_pivot_longer(x)
  input <- result[result$component == "input", ]

  expect_equal(input$subject, rep("example:Q1", 2L))
  expect_equal(input$predicate, rep("documents", 2L))
  expect_equal(
    input$value,
    c("https://example.org/1", "https://example.org/2")
  )
})


test_that("review_pivot_longer() supplies the default input predicate", {
  x <- create_candidate_dataset(
    input_media_url = "https://example.org/input/1",
    label = "Example",
    subject = "example:Q1"
  )

  result <- review_pivot_longer(x)
  input <- result[result$component == "input", ]

  expect_equal(input$subject, "example:Q1")
  expect_equal(input$predicate, "review_input")
  expect_equal(input$value, "https://example.org/input/1")
})




test_that("review_pivot_longer() projects the dataset review comment", {
  review <- read_review(final_path)

  result <- review |>
    project_review_wide() |>
    review_pivot_longer()

  comment <- result[result$component == "dataset_comment", ]

  expect_equal(nrow(comment), 1L)
  expect_equal(comment$row_id, 0L)
  expect_equal(comment$predicate, "review_comment")
  expect_equal(comment$value, "Final version")
})


test_that("review_pivot_longer() projects dataset provenance", {
  review <- read_review(final_path)

  result <- review |>
    project_review_wide() |>
    review_pivot_longer()

  provenance <- result[result$component == "dataset_provenance", ]

  expect_true(nrow(provenance) > 0L)
  expect_true(all(provenance$row_id == 0L))

  expect_true(any(
    provenance$predicate == "software_agent" &
      provenance$value == "Betwixt"
  ))

  expect_true(any(
    provenance$predicate == "reviewer" &
      provenance$value == "Daniel Antal"
  ))

  expect_false(any(
    provenance$predicate %in%
      c("project_id", "table_id", "review_comment")
  ))
})


test_that("review_pivot_longer() handles a minimal URL-only candidate", {
  x <- create_candidate_dataset(
    input_url = "https://example.org/source/1",
    subject = "example:Q1"
  )

  result <- review_pivot_longer(x)

  expect_equal(
    unique(result$row_id[result$component == "domain"]),
    integer()
  )

  expect_false(any(result$component == "input"))
})


test_that("review_pivot_longer() handles statistical candidate data", {
  x <- create_candidate_dataset(
    input_media_url = rep(
      "https://www.w3.org/TR/vocab-data-cube/",
      nrow(w3c_life_expectancy)
    ),
    input_description = rep(
      "W3C RDF Data Cube Vocabulary",
      nrow(w3c_life_expectancy)
    ),
    input_predicate = rep(
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
    add_candidate_column("area", w3c_life_expectancy$area) |>
    add_candidate_column("period", w3c_life_expectancy$period) |>
    add_candidate_column("sex", w3c_life_expectancy$sex) |>
    add_candidate_column(
      "life_expectancy",
      w3c_life_expectancy$life_expectancy
    )

  result <- review_pivot_longer(x)
  domain <- result[result$component == "domain", ]

  expect_equal(
    domain$subject[domain$predicate == "life_expectancy"],
    w3c_life_expectancy$observation
  )

  expect_equal(
    domain$value[domain$predicate == "life_expectancy"],
    as.character(w3c_life_expectancy$life_expectancy)
  )

  expect_equal(
    domain$value[domain$predicate == "period"],
    as.character(w3c_life_expectancy$period)
  )
})


test_that("review_pivot_longer() handles descriptive heritage data", {
  x <- create_candidate_dataset(
    input_media_url = c(
      "https://example.org/image/1",
      "https://example.org/image/2"
    ),
    input_description = c(
      "Front of textile",
      "Reverse of vessel"
    ),
    input_predicate = c(
      "depicts",
      "documents"
    ),
    label = c(
      "Tablet-woven sash",
      "Ceramic vessel"
    ),
    description = c(
      "A tablet-woven textile object.",
      "A decorated ceramic vessel."
    ),
    alternative_label = c(
      "Tablet-woven belt",
      NA
    ),
    alternative_description = c(
      "A visitor-facing description of the textile.",
      "A vessel with painted decoration."
    ),
    subject = c(
      "example:textile-1",
      "example:vessel-1"
    )
  )

  result <- review_pivot_longer(x)

  input <- result[result$component == "input", ]
  domain <- result[result$component == "domain", ]

  expect_equal(
    input$subject,
    c("example:textile-1", "example:vessel-1")
  )

  expect_equal(
    input$predicate,
    c("depicts", "documents")
  )

  expect_equal(
    input$value,
    c(
      "https://example.org/image/1",
      "https://example.org/image/2"
    )
  )

  expect_equal(
    domain$value[domain$predicate == "alternative_label"],
    "Tablet-woven belt"
  )

  expect_equal(
    domain$value[domain$predicate == "alternative_description"],
    c(
      "A visitor-facing description of the textile.",
      "A vessel with painted decoration."
    )
  )
})


test_that("review_pivot_longer() preserves candidate dataset identity and provenance", {
  x <- create_candidate_dataset(
    subject = "example:Q1",
    input_url = "https://example.org/source/1",
    label = "Example object",
    data_manager_name = "Daniel Antal",
    data_manager_iri = "https://orcid.org/0000-0001-7513-6760",
    data_manager_email = "daniel@example.org",
    table_id = "example-table",
    project_id = "example-project"
  )

  result <- review_pivot_longer(x)

  identity <- result[result$component == "dataset_identity", ]
  provenance <- result[result$component == "dataset_provenance", ]

  expect_true(any(
    identity$subject == "example-table" &
      identity$predicate == "type" &
      identity$value == "schema:Dataset"
  ))

  expect_true(any(
    identity$subject == "example-table" &
      identity$predicate == "project_id" &
      identity$value == "example-project"
  ))

  expect_true(any(
    provenance$predicate == "data_manager" &
      provenance$value == "Daniel Antal"
  ))

  expect_true(any(
    provenance$predicate == "data_manager_iri" &
      provenance$value ==
      "https://orcid.org/0000-0001-7513-6760"
  ))

  expect_true(any(
    provenance$predicate == "data_manager_email" &
      provenance$value == "daniel@example.org"
  ))
})


test_that("review_pivot_longer() preserves concatenated value order", {
  x <- data.frame(
    row_id = c(1L, 2L),
    subject = c("Q1", "Q2"),
    instance_of = c("human | composer", "building"),
    occupation = c("pianist | composer", NA_character_)
  )

  result <- review_pivot_longer(x)
  domain <- result[result$component == "domain", ]

  expect_equal(
    domain$predicate,
    c(
      "instance_of", "instance_of",
      "occupation", "occupation",
      "instance_of"
    )
  )

  expect_equal(
    domain$value,
    c("human", "composer", "pianist", "composer", "building")
    )

  expect_equal(
    domain$subject,
    c("Q1", "Q1", "Q1", "Q1", "Q2")
  )
})
