# Complete review -------------------------------------------------------------

test_that("serialise_review() creates candidate and reviewed datasets", {
  review <- read_review(
    "fixtures/small_countries_dataset_extended_1-finalised.html"
  )
  ttl <- serialise_review(
    review,
    "https://usebetwixt.com/examples/",
    "small-countries-review.ttl"
  )

  expect_match(ttl, "/small-countries-review/candidate>", fixed = TRUE)
  expect_match(ttl, "/small-countries-review/reviewed-1>", fixed = TRUE)
  expect_match(ttl, "prov:wasDerivedFrom", fixed = TRUE)
  expect_match(ttl, "a prov:Activity", fixed = TRUE)
  expect_match(ttl, "a prov:Agent", fixed = TRUE)
  expect_match(ttl, "a btx:Assertion", fixed = TRUE)
})

test_that("serialise_review() separates candidate and reviewed assertions", {
  review <- read_review(
    "fixtures/small_countries_dataset_extended_1-finalised.html"
  )
  x <- project_review_long(review)
  ttl <- serialise_review(
    review,
    "https://usebetwixt.com/examples/",
    "small-countries-review.ttl"
  )

  n_candidate <- sum(x$plane == "candidate")
  n_reviewed <- sum(x$plane == "reviewed")

  expect_match(ttl, "/candidate/assertion/1>", fixed = TRUE)
  expect_match(ttl, "/reviewed-1/assertion/1>", fixed = TRUE)
  expect_match(
    ttl,
    paste0("/candidate/assertion/", n_candidate, ">"),
    fixed = TRUE
  )
  expect_match(
    ttl,
    paste0("/reviewed-1/assertion/", n_reviewed, ">"),
    fixed = TRUE
  )
})

test_that("review status belongs only to reviewed assertions", {
  review <- read_review(
    "fixtures/small_countries_dataset_extended_1-finalised.html"
  )
  x <- project_review_long(review)

  candidate <- x[x$plane == "candidate", ][1, ]
  reviewed <- x[x$plane == "reviewed", ][1, ]

  candidate_ttl <- serialise_assertion(
    candidate, "https://example.org/", "review.ttl", "candidate", 1L
  )
  reviewed_ttl <- serialise_assertion(
    reviewed, "https://example.org/", "review.ttl", "reviewed-1", 1L
  )

  expect_false(grepl("btx:status", candidate_ttl, fixed = TRUE))
  expect_match(reviewed_ttl, "btx:status btx:Corroborated", fixed = TRUE)
})

test_that("serialise_review() produces valid Turtle", {
  skip_if_not_installed("rdflib")

  review <- read_review(
    "fixtures/small_countries_dataset_extended_1-finalised.html"
  )
  ttl <- serialise_review(
    review,
    "https://usebetwixt.com/examples/",
    "small-countries-review.ttl"
  )

  path <- tempfile(fileext = ".ttl")
  writeLines(ttl, path, useBytes = TRUE)

  expect_no_error(rdflib::rdf_parse(path, format = "turtle"))
})

# Prefix serialisation --------------------------------------------------------

test_that("serialise_prefixes() creates Turtle prefixes", {
  x <- serialise_prefixes()

  expect_match(x, "@prefix btx: <https://usebetwixt.com/ns/> .", fixed = TRUE)
  expect_match(x, "@prefix schema: <https://schema.org/> .", fixed = TRUE)
  expect_match(x, "@prefix prov: <http://www.w3.org/ns/prov#> .", fixed = TRUE)
  expect_match(x, "@prefix rdfs:", fixed = TRUE)
  expect_match(x, "@prefix xsd:", fixed = TRUE)
})

# Candidate provenance -------------------------------------------------------

test_that("serialise_candidate_activity() preserves candidate provenance", {
  review <- read_review(
    "fixtures/small_countries_dataset_extended_1-finalised.html"
  )
  x <- serialise_candidate_activity(
    review,
    "https://usebetwixt.com/examples/",
    "small-countries-review.ttl"
  )

  expect_match(x, "/candidate-activity>", fixed = TRUE)
  expect_match(x, "a prov:Activity", fixed = TRUE)
  expect_match(x, "prov:wasAssociatedWith", fixed = TRUE)
  expect_match(x, "prov:endedAtTime", fixed = TRUE)
})

test_that("serialise_data_manager() preserves the data manager", {
  review <- read_review(
    "fixtures/small_countries_dataset_extended_1-finalised.html"
  )
  x <- serialise_data_manager(
    review,
    "https://usebetwixt.com/examples/",
    "small-countries-review.ttl"
  )

  expect_match(x, "<https://orcid.org/0000-0001-7513-6760>", fixed = TRUE)
  expect_match(x, 'rdfs:label "Daniel Antal"', fixed = TRUE)
})

test_that("review activity links to candidate preparation", {
  review <- read_review(
    "fixtures/small_countries_dataset_extended_1-finalised.html"
  )
  x <- serialise_activity(
    review,
    "https://usebetwixt.com/examples/",
    "small-countries-review.ttl"
  )

  expect_match(x, "prov:wasInformedBy", fixed = TRUE)
  expect_match(x, "/candidate-activity>", fixed = TRUE)
})

# Dataset serialisation -------------------------------------------------------

test_that("serialise_dataset() creates a candidate dataset", {
  x <- data.frame(row_number = 1:2)

  ttl <- serialise_dataset(
    x,
    "https://usebetwixt.com/examples/",
    "small-countries-review.ttl",
    "candidate",
    activity = "candidate-activity"
  )

  expect_match(ttl, "/small-countries-review/candidate>", fixed = TRUE)
  expect_match(ttl, "a schema:Dataset", fixed = TRUE)
  expect_match(ttl, "/candidate-activity>", fixed = TRUE)
  expect_false(grepl("prov:wasDerivedFrom", ttl, fixed = TRUE))
})

test_that("serialise_dataset() creates a derived reviewed dataset", {
  x <- data.frame(row_number = 1:2)

  ttl <- serialise_dataset(
    x,
    "https://usebetwixt.com/examples/",
    "small-countries-review.ttl",
    "reviewed-1",
    activity = "activity",
    derived_from = "candidate"
  )

  expect_match(ttl, "/small-countries-review/reviewed-1>", fixed = TRUE)
  expect_match(ttl, "/activity>", fixed = TRUE)
  expect_match(ttl, "prov:wasDerivedFrom", fixed = TRUE)
  expect_match(ttl, "/small-countries-review/candidate>", fixed = TRUE)
})

test_that("serialise_dataset() adds descriptive metadata", {
  x <- data.frame(row_number = 1L)

  ttl <- serialise_dataset(
    x,
    "https://usebetwixt.com/examples/",
    "small-countries-review.ttl",
    "reviewed-1",
    title = "Small countries GDP review",
    description = "Reviewed GDP assertions for selected small countries.",
    activity = "activity"
  )

  expect_match(ttl, 'schema:name "Small countries GDP review"', fixed = TRUE)
  expect_match(ttl, 'schema:description "Reviewed GDP assertions', fixed = TRUE)
})

test_that("serialise_dataset() escapes literals", {
  x <- data.frame(row_number = 1L)

  ttl <- serialise_dataset(
    x,
    "https://usebetwixt.com/examples/",
    "test.ttl",
    "candidate",
    title = 'Women"s clothing',
    description = "Line one\nLine two",
    activity = "candidate-activity"
  )

  expect_match(ttl, 'schema:name "Women\\"s clothing"', fixed = TRUE)
  expect_match(ttl, 'description "Line one\\nLine two"', fixed = TRUE)
})

test_that("serialise_dataset() links only its own assertions", {
  x <- data.frame(row_number = 1:2)

  ttl <- serialise_dataset(
    x,
    "https://usebetwixt.com/examples/",
    "review.ttl",
    "candidate",
    activity = "candidate-activity"
  )

  expect_match(ttl, "/review/candidate/assertion/1>", fixed = TRUE)
  expect_match(ttl, "/review/candidate/assertion/2>", fixed = TRUE)
  expect_false(grepl("/reviewed-1/assertion/", ttl, fixed = TRUE))
})

# Activity serialisation ------------------------------------------------------

test_that("serialise_activity() creates a review activity", {
  review <- read_review(
    "fixtures/small_countries_dataset_extended_1-finalised.html"
  )
  x <- serialise_activity(
    review,
    "https://usebetwixt.com/examples/",
    "small-countries-review.ttl"
  )

  expect_match(x, "/small-countries-review/activity>", fixed = TRUE)
  expect_match(x, "a prov:Activity", fixed = TRUE)
  expect_match(x, "prov:wasAssociatedWith", fixed = TRUE)
  expect_match(x, "prov:startedAtTime", fixed = TRUE)
  expect_match(x, "prov:endedAtTime", fixed = TRUE)
})

test_that("serialise_activity() validates the reviewer IRI", {
  review <- read_review(
    "fixtures/small_countries_dataset_extended_1-finalised.html"
  )

  expect_error(
    serialise_activity(review, "https://example.org/", "test.ttl", "not-an-iri"),
    "absolute"
  )
})

# Reviewer serialisation ------------------------------------------------------

test_that("serialise_reviewer() creates a reviewer agent", {
  review <- read_review(
    "fixtures/small_countries_dataset_extended_1-finalised.html"
  )
  x <- serialise_reviewer(
    review,
    "https://usebetwixt.com/examples/",
    "small-countries-review.ttl"
  )

  expect_match(x, "<https://orcid.org/0000-0002-1825-0097>", fixed = TRUE)
  expect_match(x, "a prov:Agent", fixed = TRUE)
  expect_match(x, 'rdfs:label "Jane Doe"', fixed = TRUE)
})

test_that("serialise_reviewer() accepts a reviewer IRI override", {
  review <- read_review(
    "fixtures/small_countries_dataset_extended_1-finalised.html"
  )
  x <- serialise_reviewer(
    review,
    "https://usebetwixt.com/examples/",
    "small-countries-review.ttl",
    reviewer_iri = "https://orcid.org/0000-0000-0000-0000"
  )

  expect_match(x, "<https://orcid.org/0000-0000-0000-0000>", fixed = TRUE)
  expect_match(x, 'rdfs:label "Jane Doe"', fixed = TRUE)
})

test_that("serialise_reviewer() escapes the reviewer name", {
  review <- read_review(
    "fixtures/small_countries_dataset_extended_1-finalised.html"
  )
  review$provenance$reviewer <- 'Jane "Test" Doe'

  x <- serialise_reviewer(
    review,
    "https://usebetwixt.com/examples/",
    "small-countries-review.ttl"
  )

  expect_match(x, 'rdfs:label "Jane \\"Test\\" Doe"', fixed = TRUE)
})

# Dataset serialisation -------------------------------------------------------

test_that("serialise_dataset() creates a candidate dataset", {
  x <- data.frame(row_number = 1:2)

  ttl <- serialise_dataset(
    x,
    "https://usebetwixt.com/examples/",
    "small-countries-review.ttl",
    "candidate",
    activity = "candidate-activity"
  )

  expect_match(ttl, "/small-countries-review/candidate>", fixed = TRUE)
  expect_match(ttl, "a schema:Dataset", fixed = TRUE)
  expect_match(ttl, "/candidate-activity>", fixed = TRUE)
  expect_false(grepl("prov:wasDerivedFrom", ttl, fixed = TRUE))
})

test_that("serialise_dataset() creates a derived reviewed dataset", {
  x <- data.frame(row_number = 1:2)

  ttl <- serialise_dataset(
    x,
    "https://usebetwixt.com/examples/",
    "small-countries-review.ttl",
    "reviewed-1",
    activity = "activity",
    derived_from = "candidate"
  )

  expect_match(ttl, "/small-countries-review/reviewed-1>", fixed = TRUE)
  expect_match(ttl, "/activity>", fixed = TRUE)
  expect_match(ttl, "prov:wasDerivedFrom", fixed = TRUE)
  expect_match(ttl, "/small-countries-review/candidate>", fixed = TRUE)
})

test_that("serialise_dataset() adds descriptive metadata", {
  x <- data.frame(row_number = 1L)

  ttl <- serialise_dataset(
    x,
    "https://usebetwixt.com/examples/",
    "small-countries-review.ttl",
    "reviewed-1",
    title = "Small countries GDP review",
    description = "Reviewed GDP assertions for selected small countries.",
    activity = "activity"
  )

  expect_match(ttl, 'schema:name "Small countries GDP review"', fixed = TRUE)
  expect_match(ttl, 'schema:description "Reviewed GDP assertions', fixed = TRUE)
})

test_that("serialise_dataset() escapes literals", {
  x <- data.frame(row_number = 1L)

  ttl <- serialise_dataset(
    x,
    "https://usebetwixt.com/examples/",
    "test.ttl",
    "candidate",
    title = 'Women"s clothing',
    description = "Line one\nLine two",
    activity = "candidate-activity"
  )

  expect_match(ttl, 'schema:name "Women\\"s clothing"', fixed = TRUE)
  expect_match(ttl, 'description "Line one\\nLine two"', fixed = TRUE)
})

test_that("serialise_dataset() links only its own assertions", {
  x <- data.frame(row_number = 1:2)

  ttl <- serialise_dataset(
    x,
    "https://usebetwixt.com/examples/",
    "review.ttl",
    "candidate",
    activity = "candidate-activity"
  )

  expect_match(ttl, "/review/candidate/assertion/1>", fixed = TRUE)
  expect_match(ttl, "/review/candidate/assertion/2>", fixed = TRUE)
  expect_false(grepl("/reviewed-1/assertion/", ttl, fixed = TRUE))
})


# Turtle literals -------------------------------------------------------------

test_that("turtle_literal() escapes Turtle strings", {
  expect_equal(turtle_literal("cardigan"), '"cardigan"')
  expect_equal(turtle_literal('women"s'), '"women\\"s"')
  expect_equal(turtle_literal("a\\b"), '"a\\\\b"')
  expect_equal(turtle_literal("a\nb"), '"a\\nb"')
  expect_equal(turtle_literal("női pulóver"), '"női pulóver"')
})

# Turtle IRIs -----------------------------------------------------------------

test_that("turtle_iri() constructs Turtle IRIs", {
  expect_equal(turtle_iri("https://example.org/Q1"), "<https://example.org/Q1>")
  expect_equal(turtle_iri("urn:isbn:9780140328721"), "<urn:isbn:9780140328721>")
})

test_that("turtle_iri() rejects invalid IRIs", {
  expect_error(turtle_iri(""), "non-empty")
  expect_error(turtle_iri(NA_character_), "non-empty")
  expect_error(turtle_iri("Q223"), "absolute")
  expect_error(turtle_iri("https://example.org/a b"), "not permitted")
  expect_error(turtle_iri("https://example.org/<Q1>"), "not permitted")
})
