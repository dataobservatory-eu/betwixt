# render_review -------------------------------------------------------------

test_that("render_review() writes an initial Delini review", {
  path <- tempdir()

  render_review(
    delini,
    cols = c(
      subject = "Subject",
      instance_of = "instance of",
      heritage_of = "heritage of",
      context_1 = "held by"
    ),
    subheadings = c(
      instance_of = "wdt:P31",
      heritage_of = "controlled range"
    ),
    title = "Delini semantic review",
    description = "Review the proposed semantic assertions.",
    filename_stem = "delini_wide",
    project_id = "delini",
    sequence = 0L,
    path = path
  )

  output <- file.path(path, "delini_wide.html")
  expect_true(file.exists(output))

  html <- paste(readLines(output, warn = FALSE), collapse = "\n")

  expect_match(html, "Delini semantic review", fixed = TRUE)
  expect_match(html, 'value="delini"', fixed = TRUE)
  expect_match(html, 'value="delini_wide.html"', fixed = TRUE)
  expect_match(html, 'value="0"', fixed = TRUE)

  expect_match(html, 'data-column="subject"', fixed = TRUE)
  expect_match(html, 'data-column="instance_of"', fixed = TRUE)
  expect_match(html, 'data-column="heritage_of"', fixed = TRUE)
  expect_false(grepl('data-column="col_', html, fixed = TRUE))

  expect_match(html, 'id="review-table"', fixed = TRUE)
  expect_match(html, 'data-finalised="false"', fixed = TRUE)
  expect_match(html, 'data-qualification="none"', fixed = TRUE)
  expect_match(html, 'value="in-progress"', fixed = TRUE)

  expect_match(html, 'id="save-draft"', fixed = TRUE)
  expect_match(html, 'id="save-final"', fixed = TRUE)
})


# -------------------------------------------------------------------------
# Row identity
# -------------------------------------------------------------------------

test_that("render_review() preserves candidate row numbers", {
  html <- render_review(delini[1:3, ])

  expect_match(html, 'data-row="1"', fixed = TRUE)
  expect_match(html, 'data-row="2"', fixed = TRUE)
  expect_match(html, 'data-row="3"', fixed = TRUE)
})


# -------------------------------------------------------------------------
# Review sequence
# -------------------------------------------------------------------------

test_that("render_review() records a sequenced filename", {
  html <- render_review(
    delini,
    filename_stem = "delini_wide",
    sequence = 2L
  )

  expect_match(html, 'value="delini_wide_2.html"', fixed = TRUE)
  expect_match(html, 'id="review-sequence" type="number" value="2"', fixed = TRUE)
})


# -------------------------------------------------------------------------
# Optional comments
# -------------------------------------------------------------------------

test_that("comments are absent by default", {
  html <- render_review(delini)

  expect_false(grepl('<th class="row-comment">', html, fixed = TRUE))
  expect_false(grepl('id="review-comment"', html, fixed = TRUE))
})

test_that("row comments are rendered when requested", {
  html <- render_review(delini, row_comment = TRUE)

  expect_match(html, '<th class="row-comment">Reviewer comment</th>',
    fixed = TRUE
  )
  expect_match(html, '<td class="row-comment">', fixed = TRUE)
  expect_false(grepl('id="review-comment"', html, fixed = TRUE))
})

test_that("review comment is rendered when requested", {
  html <- render_review(delini, review_comment = TRUE)

  expect_match(html, 'id="review-comment"', fixed = TRUE)
  expect_false(grepl('<th class="row-comment">', html, fixed = TRUE))
})


# -------------------------------------------------------------------------
# Review provenance
# -------------------------------------------------------------------------

test_that("reviewer identity survives the HTML round trip", {
  path <- tempfile(fileext = ".html")
  stem <- tools::file_path_sans_ext(basename(path))

  render_review(
    delini,
    title = "Delini review",
    reviewer_name = "Daniel Antal",
    reviewer_iri = "https://example.org/d123",
    project_id = "delini",
    filename_stem = stem,
    path = dirname(path)
  )

  review <- read_review(path)

  expect_equal(review$metadata$title, "Delini review")
  expect_equal(review$metadata$project_id, "delini")
  expect_equal(review$provenance$reviewer, "Daniel Antal")
  expect_equal(review$provenance$reviewer_iri, "https://example.org/d123")
})


# -------------------------------------------------------------------------
# Candidate provenance
# -------------------------------------------------------------------------

test_that("candidate provenance survives rendering", {
  candidate <- create_candidate_dataset(
    subject = "Example",
    evidence_url = "https://example.org",
    data_manager_name = "Daniel Antal",
    data_manager_iri = "https://orcid.org/0000-0001-7513-6760",
    data_manager_email = "daniel@example.org",
    project_id = "example-project"
  )

  html <- render_review(candidate)

  expect_match(html, "Data manager: Daniel Antal", fixed = TRUE)
  expect_match(html, "Project: example-project", fixed = TRUE)
  expect_match(html, "Generated:", fixed = TRUE)
  expect_match(html, "Betwixt", fixed = TRUE)

  expect_match(html, 'data-provenance="data_manager"', fixed = TRUE)
  expect_match(html, 'value="Daniel Antal"', fixed = TRUE)
  expect_match(html, 'data-provenance="data_manager_iri"', fixed = TRUE)
  expect_match(html, 'data-provenance="data_manager_email"', fixed = TRUE)
  expect_match(html, 'data-provenance="project_id"', fixed = TRUE)
  expect_match(html, 'data-provenance="generated_at"', fixed = TRUE)
  expect_match(html, 'data-provenance="software_version"', fixed = TRUE)
})


# -------------------------------------------------------------------------
# Typed candidates and context
# -------------------------------------------------------------------------

test_that("render_review() renders numeric candidates and context", {
  html <- render_review(small_countries_dataset)

  expect_match(html, 'data-column="gdp"', fixed = TRUE)
  expect_match(html, 'data-context="context_year"', fixed = TRUE)
  expect_match(html, 'data-context="context_unit"', fixed = TRUE)
  expect_match(html, 'data-value="CP_MEUR"', fixed = TRUE)
})

test_that("context is preserved but not reviewable", {
  html <- render_review(small_countries_dataset)

  expect_match(html, 'data-context="context_year"', fixed = TRUE)
  expect_match(html, 'data-context="context_unit"', fixed = TRUE)

  expect_false(grepl('data-column="context_year"', html, fixed = TRUE))
  expect_false(grepl('data-column="context_unit"', html, fixed = TRUE))
})


# -------------------------------------------------------------------------
# Footer
# -------------------------------------------------------------------------

test_that("render_review() retains the Betwixt footer", {
  html <- render_review(delini)

  expect_match(html, "Created with Betwixt semantic review", fixed = TRUE)
  expect_match(html, "doi:10.5281/zenodo.22091535", fixed = TRUE)
})


# -------------------------------------------------------------------------
# Input validation
# -------------------------------------------------------------------------

test_that("render_review() validates sequence", {
  expect_error(render_review(delini, sequence = -1), "non-negative")
  expect_error(render_review(delini, sequence = 1.5), "non-negative")
  expect_error(render_review(delini, sequence = NA), "non-negative")
  expect_error(render_review(delini, sequence = c(1, 2)), "non-negative")
})


# -------------------------------------------------------------------------
# Presentation
# -------------------------------------------------------------------------

test_that("render_review() applies labels and subheadings", {
  html <- render_review(
    delini,
    cols = c(instance_of = "Instance of"),
    subheadings = c(instance_of = "wdt:P31")
  )

  expect_match(html, "Instance of", fixed = TRUE)
  expect_match(html, "wdt:P31", fixed = TRUE)
})

test_that("render_review() escapes document text", {
  html <- render_review(
    delini,
    title = "A & B <review>",
    description = 'Review "this" & that'
  )

  expect_match(html, "A &amp; B &lt;review&gt;", fixed = TRUE)
  expect_match(html, "Review &quot;this&quot; &amp; that", fixed = TRUE)
})


# -------------------------------------------------------------------------
# Artefact provenance
# -------------------------------------------------------------------------

test_that("render_review() records creation time", {
  html <- render_review(delini)

  expect_match(
    html,
    'id="original-created-at" type="text" value="',
    fixed = TRUE
  )
  expect_match(html, "T", fixed = TRUE)
  expect_match(html, "Z", fixed = TRUE)
})


# -------------------------------------------------------------------------
# Candidate provenance
# -------------------------------------------------------------------------

test_that("empty candidate provenance is not displayed", {
  html <- render_review(delini)

  expect_false(grepl("Data manager:", html, fixed = TRUE))
  expect_false(grepl("Project: ·", html, fixed = TRUE))

  # Machine-readable provenance remains present.
  expect_match(html, 'data-provenance="data_manager"', fixed = TRUE)
  expect_match(html, 'data-provenance="project_id"', fixed = TRUE)
})


# -------------------------------------------------------------------------
# HTML structure
# -------------------------------------------------------------------------

test_that("render_review() produces one site footer", {
  html <- render_review(delini)

  matches <- gregexpr(
    '<footer class="site-footer">',
    html,
    fixed = TRUE
  )[[1]]

  expect_length(matches[matches > 0], 1L)
})
