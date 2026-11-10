# Fixtures ------------------------------------------------------------------

initial_path <- test_path("fixtures", "muis-garments-review.html")
draft_path <- test_path("fixtures", "muis-garments-review_1-draft.html")
final_path <- test_path("fixtures", "muis-garments-review_1-finalised.html")


# -------------------------------------------------------------------------
# Initial review
# -------------------------------------------------------------------------

test_that("read_review() reconstructs initial candidate state", {
  review <- read_review(initial_path)

  expect_equal(nrow(review$candidate), 3L)
  expect_equal(
    review$candidate$depicts,
    c("300209900", "300148821", "300212499")
  )
  expect_equal(
    review$candidate$context_held_by,
    rep("Estonian National Museum", 3L)
  )

  expect_equal(review$metadata$status, "in-progress")
  expect_equal(review$metadata$sequence, 0L)
})


# -------------------------------------------------------------------------
# Draft review
# -------------------------------------------------------------------------

test_that("read_review() reconstructs draft review state", {
  review <- read_review(draft_path)

  expect_equal(review$provenance$reviewer, "Daniel Antal")
  expect_equal(review$provenance$reviewer_email, "daniel@example.net")
  expect_equal(review$metadata$status, "in-progress")
  expect_equal(review$metadata$sequence, 1L)

  expect_identical(review$reviewed$finalised, c(FALSE, TRUE, TRUE))
  expect_equal(
    review$reviewed$depicts_qualification,
    c("defer", "reject", "none")
  )
  expect_equal(
    review$reviewed$outcome,
    c("defer", "reject", "accept")
  )

  expect_equal(
    review$reviewed$row_comment,
    c("I am not sure of this", NA, NA)
  )

  expect_equal(
    review$reviewed$context_held_by,
    rep("Estonian National Museum", 3L)
  )
})


# -------------------------------------------------------------------------
# Finalised review
# -------------------------------------------------------------------------

test_that("read_review() reconstructs finalised review state", {
  review <- read_review(final_path)

  expect_equal(review$metadata$status, "finalised")
  expect_equal(review$metadata$sequence, 1L)
  expect_true(all(review$reviewed$finalised))

  expect_equal(
    review$reviewed$outcome,
    rep("accept", 3L)
  )
  expect_equal(
    review$reviewed$depicts_qualification,
    rep("none", 3L)
  )

  expect_equal(review$provenance$review_comment, "Final version")
})


# -------------------------------------------------------------------------
# Candidate and reviewed values
# -------------------------------------------------------------------------

test_that("read_review() separates candidate and corrected values", {
  review <- read_review(final_path)

  expect_equal(review$candidate$alternative_label[1], "női kardigán")
  expect_equal(review$reviewed$alternative_label[1], "női pulóver")

  expect_equal(review$candidate$depicts[1], "300209900")
  expect_equal(review$reviewed$depicts[1], "300209900")
})

# -------------------------------------------------------------------------
# Context parser
# -------------------------------------------------------------------------


test_that("read_review() preserves context without qualifications", {
  review <- read_review(draft_path)

  expect_equal(
    review$candidate$context_held_by,
    rep("Estonian National Museum", 3L)
  )
  expect_equal(
    review$reviewed$context_held_by,
    rep("Estonian National Museum", 3L)
  )

  expect_false("context_held_by_qualification" %in%
    names(review$reviewed))
})

# -------------------------------------------------------------------------
# Provenance
# -------------------------------------------------------------------------


test_that("read_review() normalises missing candidate provenance", {
  path <- test_path(
    "fixtures",
    "small_countries_dataset_extended_1-draft.html"
  )

  review <- read_review(path)

  expect_false(is.na(review$provenance$generated_at))
  expect_equal(review$provenance$software_agent, "Betwixt")
  expect_equal(review$provenance$software_version, "0.0.81")
})


# Special candidate-range values -------------------------------------------

test_that("read_review() resolves special candidate values", {
  # Use an existing review fixture as the document template.
  document <- xml2::read_html(initial_path)
  row <- xml2::xml_find_first(
    document, '//*[@id="review-table"]//tbody/tr[1]'
  )

  assertion <- xml2::xml_find_first(
    row, ".//td[@data-column and @data-candidate][1]"
  )

  column <- xml2::xml_attr(assertion, "data-column")
  original <- xml2::xml_attr(assertion, "data-candidate")

  cases <- data.frame(
    option = c(
      "Other", "Other\u2026", "Other...", "<other>",
      "<other>", "<unknown>", "<no-value>"
    ),
    value = c(
      "Other", "__other__", "__other__", "__other__",
      "__other__", "__unknown__", "__no_value__"
    ),
    write_in = c(
      "", "", "", "", "Satirical commentary", "", ""
    ),
    expected = c(
      "Other", "Other\u2026", "Other...", "<other>",
      "Satirical commentary", "<unknown>", "<no-value>"
    )
  )

  for (i in seq_len(nrow(cases))) {
    # Reconstruct the review from a fresh copy of the fixture.
    document <- xml2::read_html(initial_path)

    assertion <- xml2::xml_find_first(
      document,
      '//*[@id="review-table"]//tbody/tr[1]//td[@data-column][1]'
    )

    # Replace the assertion's review control.
    children <- xml2::xml_children(assertion)
    xml2::xml_remove(children)

    select <- xml2::xml_add_child(
      assertion, "select",
      class = "candidate-select"
    )

    xml2::xml_add_child(
      select, "option",
      "Original",
      value = "Original"
    )

    xml2::xml_add_child(
      select, "option",
      cases$option[i],
      value = cases$value[i],
      selected = "selected"
    )

    xml2::xml_add_child(
      assertion, "input",
      class = "write-in",
      value = cases$write_in[i]
    )

    # Read the modified review through the public function.
    path <- tempfile(fileext = ".html")
    xml2::write_html(document, path)

    review <- read_review(path)

    expect_equal(
      review$candidate[[column]][1],
      original
    )

    expect_equal(
      review$reviewed[[column]][1],
      cases$expected[i],
      info = paste("Case:", cases$option[i])
    )

    unlink(path)
  }
})
