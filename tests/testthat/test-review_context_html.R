# -------------------------------------------------------------------------
# Assertion rendering
# -------------------------------------------------------------------------

test_that("subject definitions distinguish resolved entities", {
  context <- prepare_review_context(delini)

  expect_false(is.na(context$rows[[1]]$assertions[[1]]$definition))
  expect_true(is.na(context$rows[[2]]$assertions[[1]]$definition))
})


test_that("URL candidate values are rendered as links", {
  x <- create_candidate_dataset(
    evidence_media_url = "https://example.org/evidence.jpg",
    evidence_text = "Example evidence",
    label = "Example",
    description = "Example description",
    subject = "[image shown]"
  ) |>
    add_candidate_column(
      value = "https://example.org/access-point",
      name = "access_point"
    )

  html <- review_context_html(prepare_review_context(x))

  expect_match(html, 'data-column="access_point"', fixed = TRUE)
  expect_match(html, '<a class="entity-link" href="https://example.org/access-point"', fixed = TRUE)
})


test_that("URL subjects are not automatically rendered as links", {
  x <- create_candidate_dataset(
    evidence_media_url = "https://example.org/evidence.jpg",
    evidence_text = "Example evidence",
    label = "Example",
    description = "Example description",
    subject = "https://example.org/subject"
  )

  html <- review_context_html(prepare_review_context(x))

  expect_false(grepl('<a class="entity-link" href="https://example.org/subject"', html, fixed = TRUE))
})

# -------------------------------------------------------------------------
# Column identities
# -------------------------------------------------------------------------

test_that("assertions preserve their column identity", {
  context <- prepare_review_context(delini)
  html <- review_context_html(context)

  expect_match(
    html,
    'class="semantic-cell" data-column="subject"',
    fixed = TRUE
  )

  expect_match(
    html,
    'class="semantic-cell" data-column="instance_of"',
    fixed = TRUE
  )
})

test_that("assertions preserve their semantic column identities", {
  html <- review_context_html(prepare_review_context(delini))

  expect_match(html, 'data-column="subject"', fixed = TRUE)
  expect_match(html, 'data-column="instance_of"', fixed = TRUE)
  expect_match(html, 'data-column="heritage_of"', fixed = TRUE)
  expect_false(grepl('data-column="col_', html, fixed = TRUE))
})

# -------------------------------------------------------------------------
# Candidate values
# -------------------------------------------------------------------------

test_that("descriptive fields preserve their candidate values", {
  x <- create_candidate_dataset(
    evidence_media_url = "https://example.org/evidence.jpg",
    evidence_text = "Example evidence",
    label = "Example label",
    description = "Example description",
    subject = "[image shown]"
  )

  html <- review_context_html(prepare_review_context(x))

  expect_match(html, 'data-field="label"', fixed = TRUE)
  expect_match(html, 'data-field="description"', fixed = TRUE)
  expect_match(
    html, 'value="Example label" data-candidate="Example label"',
    fixed = TRUE
  )
  expect_match(
    html,
    '<textarea data-field="description" data-candidate="Example description">Example description</textarea>',
    fixed = TRUE
  )
})


test_that("assertions preserve their candidate values", {
  x <- create_candidate_dataset(
    evidence_media_url = "https://example.org/evidence.jpg",
    evidence_text = "Example evidence",
    label = "Example",
    description = "Example description",
    subject = "[image shown]"
  ) |>
    add_candidate_column(
      value = "depicts",
      name = "value"
    )

  html <- review_context_html(prepare_review_context(x))

  expect_match(html, 'data-candidate="[image shown]"', fixed = TRUE)
  expect_match(html, 'data-candidate="depicts"', fixed = TRUE)
})


# -------------------------------------------------------------------------
# Evidence
# -------------------------------------------------------------------------

test_that("evidence_media_url is rendered as an image", {
  x <- create_candidate_dataset(
    evidence_media_url = "https://example.org/evidence.jpg",
    evidence_text = "Example evidence",
    label = "Example",
    description = "Example description",
    subject = "[image shown]"
  )

  html <- review_context_html(prepare_review_context(x))

  expect_match(
    html, '<img src="https://example.org/evidence.jpg"',
    fixed = TRUE
  )
})


test_that("evidence_url is rendered as a link without an image", {
  x <- create_candidate_dataset(
    evidence_url = "https://example.org/evidence.html",
    evidence_text = "Example evidence",
    label = "Example",
    description = "Example description",
    subject = "[document shown]"
  )

  html <- review_context_html(prepare_review_context(x))

  expect_match(
    html,
    '<a class="evidence-link" href="https://example.org/evidence.html"',
    fixed = TRUE
  )
  expect_false(
    grepl('<img src="https://example.org/evidence.html"', html, fixed = TRUE)
  )
})


test_that("multiple evidence URLs are rendered", {
  x <- create_candidate_dataset(
    evidence_media_url = paste(
      "https://example.org/001.jpg",
      "https://example.org/002.jpg",
      sep = " | "
    ),
    evidence_url = paste(
      "https://example.org/001.html",
      "https://example.org/002.html",
      sep = " | "
    ),
    evidence_text = "Multiple evidence resources",
    label = "Example",
    description = "Example description",
    subject = "[evidence shown]"
  )

  html <- review_context_html(prepare_review_context(x))

  expect_match(html, 'src="https://example.org/001.jpg"', fixed = TRUE)
  expect_match(html, 'src="https://example.org/002.jpg"', fixed = TRUE)
  expect_match(html, 'href="https://example.org/001.html"', fixed = TRUE)
  expect_match(html, 'href="https://example.org/002.html"', fixed = TRUE)
})


test_that("review can be rendered without evidence", {
  x <- create_candidate_dataset(
    subject = c("House", "Sash", "Bed")
  )

  html <- review_context_html(prepare_review_context(x))

  expect_match(html, 'data-row="1"', fixed = TRUE)
  expect_match(html, 'data-row="2"', fixed = TRUE)
  expect_match(html, 'data-row="3"', fixed = TRUE)

  expect_match(
    html,
    '<td class="evidence"><div class="media-id"></div></td>',
    fixed = TRUE
  )
  expect_false(grepl('class="evidence-media-link"', html, fixed = TRUE))
  expect_false(grepl('class="evidence-link"', html, fixed = TRUE))

  expect_match(html, 'data-candidate="House"', fixed = TRUE)
  expect_match(html, 'data-candidate="Sash"', fixed = TRUE)
  expect_match(html, 'data-candidate="Bed"', fixed = TRUE)
})

# -------------------------------------------------------------------------
# Alternative descriptions
# -------------------------------------------------------------------------

test_that("alternative descriptive columns are omitted when unused", {
  x <- create_candidate_dataset(
    evidence_media_url = "https://example.org/evidence.jpg",
    evidence_text = "Example evidence",
    label = "Example",
    description = "Example description",
    subject = "[image shown]"
  )

  html <- review_context_html(prepare_review_context(x))

  expect_false(grepl("<th>Alternative label</th>", html, fixed = TRUE))
  expect_false(grepl("<th>Alternative description</th>", html, fixed = TRUE))
})


test_that("alternative label can be rendered alone", {
  x <- create_candidate_dataset(
    evidence_media_url = "https://example.org/evidence.jpg",
    evidence_text = "Example evidence",
    label = "Tablet-woven sash",
    description = "A tablet-woven textile object.",
    alternative_label = "Tablet-woven belt",
    subject = "[image shown]"
  )

  html <- review_context_html(prepare_review_context(x))

  expect_match(html, "<th>Alternative label</th>", fixed = TRUE)
  expect_match(html, 'value="Tablet-woven belt"', fixed = TRUE)
  expect_match(html, 'data-candidate="Tablet-woven belt"', fixed = TRUE)
  expect_false(grepl("<th>Alternative description</th>", html, fixed = TRUE))
})


test_that("alternative label and description are rendered together", {
  x <- create_candidate_dataset(
    evidence_media_url = "https://example.org/evidence.jpg",
    evidence_text = "Example evidence",
    label = "Tablet-woven sash",
    description = "A tablet-woven textile object.",
    alternative_label = "Tablet-woven belt",
    alternative_description = "A visitor-facing description of the object.",
    subject = "[image shown]"
  )

  html <- review_context_html(prepare_review_context(x))

  expect_match(html, 'data-field="alternative_label"', fixed = TRUE)
  expect_match(html, 'data-field="alternative_description"', fixed = TRUE)
  expect_match(html, "<th>Alternative label</th>", fixed = TRUE)
  expect_match(html, "<th>Alternative description</th>", fixed = TRUE)
  expect_match(html, 'data-candidate="Tablet-woven belt"', fixed = TRUE)
  expect_match(
    html,
    'data-candidate="A visitor-facing description of the object."',
    fixed = TRUE
  )
})

# -------------------------------------------------------------------------
# Context
# -------------------------------------------------------------------------

test_that("context preserves its column identity and value", {
  x <- create_candidate_dataset(subject = "Example") |>
    dplyr::mutate(context_held_by = "Estonian National Museum")

  html <- review_context_html(prepare_review_context(x))

  expect_match(html, 'data-context="context_held_by"', fixed = TRUE)
  expect_match(html, 'data-value="Estonian National Museum"', fixed = TRUE)
})

test_that("context is not reviewable", {
  x <- create_candidate_dataset(subject = "Example") |>
    dplyr::mutate(context_held_by = "Estonian National Museum")

  html <- review_context_html(prepare_review_context(x))

  expect_false(grepl('data-column="context_held_by"', html, fixed = TRUE))
  expect_false(grepl('data-field="context_held_by"', html, fixed = TRUE))
})

# -------------------------------------------------------------------------
# Row comments
# -------------------------------------------------------------------------

test_that("row comment is not rendered by default", {
  x <- create_candidate_dataset(
    evidence_media_url = "https://example.org/evidence.jpg",
    evidence_text = "Example evidence",
    label = "Example",
    description = "Example description",
    subject = "[image shown]"
  )

  html <- review_context_html(prepare_review_context(x))

  expect_false(
    grepl('<th class="row-comment">Reviewer comment</th>', html, fixed = TRUE)
  )
  expect_false(grepl('<td class="row-comment">', html, fixed = TRUE))
})


test_that("row comment is rendered when requested", {
  x <- create_candidate_dataset(
    evidence_media_url = "https://example.org/evidence.jpg",
    evidence_text = "Example evidence",
    label = "Example",
    description = "Example description",
    subject = "[image shown]"
  )

  html <- review_context_html(prepare_review_context(x), row_comment = TRUE)

  expect_match(
    html, '<th class="row-comment">Reviewer comment</th>',
    fixed = TRUE
  )
  expect_match(html, '<td class="row-comment">', fixed = TRUE)
  expect_match(
    html,
    '<textarea placeholder="Optional comment on row"></textarea>',
    fixed = TRUE
  )
})

# -------------------------------------------------------------------------
# Provenance
# -------------------------------------------------------------------------

test_that("candidate provenance is retained in HTML", {
  x <- create_candidate_dataset(
    subject = "Example",
    evidence_url = "https://example.org",
    data_manager_name = "Daniel Antal",
    data_manager_iri = "https://orcid.org/0000-0001-7513-6760",
    data_manager_email = "daniel@example.org",
    project_id = "example-project"
  )

  html <- review_context_html(prepare_review_context(x))

  expect_match(html, 'data-provenance="data_manager"', fixed = TRUE)
  expect_match(html, 'value="Daniel Antal"', fixed = TRUE)
  expect_match(html, 'data-provenance="data_manager_iri"', fixed = TRUE)
  expect_match(html, 'data-provenance="data_manager_email"', fixed = TRUE)
  expect_match(html, 'data-provenance="project_id"', fixed = TRUE)
  expect_match(html, 'data-provenance="generated_at"', fixed = TRUE)
  expect_match(html, 'data-provenance="software_agent"', fixed = TRUE)
  expect_match(html, 'data-provenance="software_version"', fixed = TRUE)
})
