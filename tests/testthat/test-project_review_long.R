# Fixtures ------------------------------------------------------------------

muis_final_path <- test_path(
  "fixtures",
  "muis-garments-review_1-finalised.html"
)

countries_final_path <- test_path(
  "fixtures",
  "small_countries_dataset_extended_1-finalised.html"
)


# Structure -----------------------------------------------------------------

test_that("long projection creates atomic assertions in both states", {
  x <- project_review_long(read_review(muis_final_path))

  expect_s3_class(x, "data.frame")
  expect_identical(
    names(x),
    c(
      "assertion_number", "row_number", "context_held_by",
      "plane", "subject", "predicate", "value", "status",
      "reviewer", "reviewer_email", "reviewer_iri",
      "started_at", "saved_at", "ended_at",
      "data_manager", "data_manager_email", "data_manager_iri",
      "candidate_generated_at", "software_agent", "software_version",
      "project_id"
    )
  )

  expect_equal(nrow(x), 30L)
  expect_equal(x$assertion_number, seq_len(nrow(x)))
  expect_setequal(unique(x$plane), c("candidate", "reviewed"))
})

test_that("long projection preserves Betwixt row coordinates", {
  x <- project_review_long(read_review(muis_final_path))

  expect_equal(as.integer(table(x$row_number)), rep(10L, 3))
  expect_equal(as.integer(table(x$plane)), c(15L, 15L))
  expect_equal(length(unique(x$assertion_number)), nrow(x))
})


# Candidate columns ---------------------------------------------------------

test_that("long projection projects named candidate columns", {
  x <- project_review_long(read_review(countries_final_path))

  expect_true(all(c("country_code", "gdp") %in% x$predicate))
  expect_setequal(unique(x$plane), c("candidate", "reviewed"))

  semantic <- x[x$predicate %in% c("country_code", "gdp"), ]
  expect_true(all(semantic$plane %in% c("candidate", "reviewed")))
})


# Descriptive assertions ----------------------------------------------------

test_that("long projection preserves descriptive assertions", {
  x <- project_review_long(read_review(muis_final_path))

  assertion <- x[
    x$row_number == 1 & x$predicate == "alternative_label",
  ]

  expect_equal(nrow(assertion), 2L)
  expect_equal(assertion$plane, c("candidate", "reviewed"))
  expect_equal(assertion$value, c("női kardigán", "női pulóver"))
  expect_equal(assertion$status, rep("corrected", 2L))
})


# Semantic assertions -------------------------------------------------------

test_that("long projection preserves semantic assertions", {
  x <- project_review_long(read_review(muis_final_path))

  assertion <- x[x$row_number == 1 & x$predicate == "depicts", ]

  expect_equal(nrow(assertion), 2L)
  expect_equal(assertion$plane, c("candidate", "reviewed"))
  expect_equal(assertion$value, rep("300209900", 2L))
  expect_equal(assertion$status, rep("corroborated", 2L))
})

test_that("long projection inherits explicit rejection", {
  x <- project_review_long(read_review(countries_final_path))

  assertion <- x[x$row_number == 1 & x$predicate == "gdp", ]

  expect_equal(nrow(assertion), 2L)
  expect_equal(assertion$status, rep("rejected", 2L))
})


# Subjects ------------------------------------------------------------------

test_that("long projection preserves subjects by plane", {
  review <- read_review(muis_final_path)
  x <- project_review_long(review)

  candidate <- x[x$row_number == 1 & x$plane == "candidate", ]
  reviewed <- x[x$row_number == 1 & x$plane == "reviewed", ]

  expect_true(all(candidate$subject == review$candidate$subject[1]))
  expect_true(all(reviewed$subject == review$reviewed$subject[1]))
})


# Context -------------------------------------------------------------------

test_that("long projection preserves row context", {
  x <- project_review_long(read_review(countries_final_path))
  row <- x[x$row_number == 1, ]

  expect_true(all(row$context_year == "2023"))
  expect_true(all(row$context_unit == "CP_MEUR"))
})

test_that("context does not become an assertion", {
  x <- project_review_long(read_review(countries_final_path))

  expect_false(any(grepl("^context_", x$predicate)))
})


# Review provenance ---------------------------------------------------------

test_that("long projection materialises reviewer provenance", {
  review <- read_review(muis_final_path)
  x <- project_review_long(review)
  p <- review$provenance

  expect_true(all(x$reviewer == p$reviewer))
  expect_true(all(x$reviewer_email == p$reviewer_email))
  expect_true(all(x$reviewer_iri == p$reviewer_iri))
  expect_true(all(x$started_at == p$started_at))
  expect_true(all(x$saved_at == p$saved_at))
  expect_true(all(x$ended_at == p$ended_at))
})

test_that("long projection materialises candidate provenance", {
  review <- read_review(countries_final_path)
  x <- project_review_long(review)
  p <- review$provenance

  expect_equal(unique(x$data_manager), p$data_manager)
  expect_equal(unique(x$data_manager_email), p$data_manager_email)
  expect_equal(unique(x$data_manager_iri), p$data_manager_iri)
  expect_equal(unique(x$candidate_generated_at), p$generated_at)
  expect_equal(unique(x$software_agent), p$software_agent)
  expect_equal(unique(x$software_version), p$software_version)
  expect_equal(unique(x$project_id), p$project_id)
})


# Review states -------------------------------------------------------------

test_that("long projection inherits states from the wide projection", {
  review <- read_review(muis_final_path)
  long <- project_review_long(review)
  wide <- project_review_wide(review)

  status <- wide[wide$plane == "status", ]

  for (predicate in unique(long$predicate)) {
    expected <- status[[predicate]]

    assertions <- long[long$predicate == predicate, ]
    actual <- assertions$status[
      match(status$row_number, assertions$row_number)
    ]

    expect_equal(actual, expected)
  }
})
