# Complete review -------------------------------------------------------------

test_that("serialise_review() creates a complete RDF review", {
  review <- read_review("fixtures/muis-garments-review_1-finalised.html")
  ttl <- serialise_review(
    review,
    "https://usebetwixt.com/examples/",
    "muis-garments-review.ttl"
  )

  expect_match(ttl, "a schema:Dataset", fixed = TRUE)
  expect_match(ttl, "a prov:Activity", fixed = TRUE)
  expect_match(ttl, "a prov:Agent", fixed = TRUE)
  expect_match(ttl, "a btx:Assertion", fixed = TRUE)
})

test_that("serialise_review() produces valid Turtle", {
  skip_if_not_installed("rdflib")

  review <- read_review("fixtures/muis-garments-review_1-finalised.html")
  ttl <- serialise_review(
    review,
    "https://usebetwixt.com/examples/",
    "muis-garments-review.ttl"
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

# Dataset serialisation -------------------------------------------------------

test_that("serialise_dataset() creates a dataset", {
  x <- serialise_dataset(
    prefix = "https://usebetwixt.com/examples/",
    filename = "muis-garments-review.ttl"
  )

  expect_match(x, "<https://usebetwixt.com/examples/muis-garments-review.ttl>",
    fixed = TRUE
  )
  expect_match(x, "a schema:Dataset", fixed = TRUE)
  expect_match(x, "prov:wasGeneratedBy", fixed = TRUE)
})


test_that("serialise_dataset() adds descriptive metadata", {
  x <- serialise_dataset(
    prefix = "https://usebetwixt.com/examples/",
    filename = "muis-garments-review.ttl",
    title = "MuIS garment terminology review",
    description = "Reviewed terminology assertions for three MuIS garments."
  )

  expect_match(x, 'schema:name "MuIS garment terminology review"', fixed = TRUE)
  expect_match(x, 'schema:description "Reviewed terminology assertions',
    fixed = TRUE
  )
})

test_that("serialise_dataset() escapes literals", {
  x <- serialise_dataset(
    "https://usebetwixt.com/examples/",
    "test.ttl",
    title = 'Women"s clothing',
    description = "Line one\nLine two"
  )

  expect_match(x, 'schema:name "Women\\"s clothing"', fixed = TRUE)
  expect_match(x, 'description "Line one\\nLine two"', fixed = TRUE)
})

test_that("serialise_dataset() links its assertions", {
  x <- serialise_dataset(
    "https://usebetwixt.com/examples/",
    "review.ttl",
    n_assertions = 2L
  )

  expect_match(x, "btx:assertion <https://usebetwixt.com/examples/review/assertion/1>",
    fixed = TRUE
  )
  expect_match(x, "<https://usebetwixt.com/examples/review/assertion/2>", fixed = TRUE)
})

# Activity serialisation ------------------------------------------------------

test_that("serialise_activity() creates a review activity", {
  review <- read_review("fixtures/muis-garments-review_1-finalised.html")
  x <- serialise_activity(
    review,
    "https://usebetwixt.com/examples/",
    "muis-garments-review.ttl"
  )

  expect_match(x, "/muis-garments-review/activity>", fixed = TRUE)
  expect_match(x, "a prov:Activity", fixed = TRUE)
  expect_match(x, "prov:wasAssociatedWith", fixed = TRUE)
  expect_match(x, "prov:startedAtTime", fixed = TRUE)
  expect_match(x, "prov:endedAtTime", fixed = TRUE)
})

test_that("serialise_activity() validates the reviewer IRI", {
  review <- read_review("fixtures/muis-garments-review_1-finalised.html")

  expect_error(
    serialise_activity(review, "https://example.org/", "test.ttl", "not-an-iri"),
    "absolute"
  )
})

# Reviewer serialisation ------------------------------------------------------

test_that("serialise_reviewer() creates a reviewer agent", {
  review <- read_review("fixtures/muis-garments-review_1-finalised.html")
  x <- serialise_reviewer(
    review,
    "https://usebetwixt.com/examples/",
    "muis-garments-review.ttl"
  )

  expect_match(x, "/muis-garments-review/reviewer>", fixed = TRUE)
  expect_match(x, "a prov:Agent", fixed = TRUE)
  expect_match(x, 'rdfs:label "Daniel Antal"', fixed = TRUE)
})

test_that("serialise_reviewer() uses a persistent reviewer IRI", {
  review <- read_review("fixtures/muis-garments-review_1-finalised.html")
  x <- serialise_reviewer(
    review,
    "https://usebetwixt.com/examples/",
    "muis-garments-review.ttl",
    reviewer_iri = "https://orcid.org/0000-0000-0000-0000"
  )

  expect_match(x, "<https://orcid.org/0000-0000-0000-0000>", fixed = TRUE)
  expect_match(x, 'rdfs:label "Daniel Antal"', fixed = TRUE)
})

test_that("serialise_reviewer() escapes the reviewer name", {
  review <- read_review("fixtures/muis-garments-review_1-finalised.html")
  review$provenance$reviewer <- 'Daniel "Test" Antal'

  x <- serialise_reviewer(
    review,
    "https://usebetwixt.com/examples/",
    "muis-garments-review.ttl"
  )

  expect_match(x, 'rdfs:label "Daniel \\"Test\\" Antal"', fixed = TRUE)
})

# Assertion serialisation -----------------------------------------------------

test_that("serialise_assertion() creates one Betwixt assertion", {
  review <- read_review("fixtures/muis-garments-review_1-finalised.html")
  x <- project_review_long(review)[1, ]
  ttl <- serialise_assertion(
    x,
    "https://usebetwixt.com/examples/",
    "muis-garments-review.ttl"
  )

  expect_match(ttl, "/muis-garments-review/assertion/1>", fixed = TRUE)
  expect_match(ttl, "a btx:Assertion", fixed = TRUE)
  expect_match(ttl, "btx:rowNumber 1", fixed = TRUE)
  expect_match(ttl, 'btx:predicate "label"', fixed = TRUE)
  expect_match(ttl, "btx:status btx:Corroborated", fixed = TRUE)
})


test_that("serialise_assertion() escapes assertion literals", {
  x <- data.frame(
    assertion_number = 1L,
    row_number = 1L,
    subject = "Q223",
    predicate = 'label "en"',
    value = "women's\ncardigan",
    status = "corrected"
  )

  ttl <- serialise_assertion(x, "https://example.org/", "review.ttl")

  expect_match(ttl, 'btx:predicate "label \\"en\\""', fixed = TRUE)
  expect_match(ttl, 'btx:value "women\'s\\ncardigan"', fixed = TRUE)
})


# Assertion collection --------------------------------------------------------

test_that("serialise_assertions() serialises all assertions", {
  review <- read_review("fixtures/muis-garments-review_1-finalised.html")
  x <- project_review_long(review)
  ttl <- serialise_assertions(
    x,
    "https://usebetwixt.com/examples/",
    "muis-garments-review.ttl"
  )

  expect_equal(length(gregexpr("a btx:Assertion", ttl, fixed = TRUE)[[1]]), 15)
  expect_match(ttl, "/assertion/1>", fixed = TRUE)
  expect_match(ttl, "/assertion/15>", fixed = TRUE)
})


# Turtle literals -------------------------------------------------------------
#' @keywords internal
#' @noRd
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
