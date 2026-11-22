# Local HTML review fixtures ---------------------------------------------

local_review_fixture <- function() {
  path <- tempfile("betwixt-local-")
  dir.create(file.path(path, "media"), recursive = TRUE)

  file.create(file.path(path, "media", "photo.jpg"))
  file.create(file.path(path, "media", "record.jpg"))

  file.create(file.path(path, "media", "article-1.html"))
  file.create(file.path(path, "media", "article-2.html"))

  candidate <- create_candidate_dataset(
    subject = c("urn:test:artefact:1", "urn:test:record:1"),
    input_media_url = c("media/photo.jpg", "media/record.jpg"),
    input_url = c("media/article-1.html", "media/article-2.html"),
    label = c("Delini sash", "Delini floor plan"),
    description = c("A woven sash", "A farmhouse floor plan")
  )

  list(path = path, candidate = candidate)
}

test_that("local HTML review is written to the output directory", {
  fixture <- local_review_fixture()
  on.exit(unlink(fixture$path, recursive = TRUE), add = TRUE)

  expect_message(
    html <- render_local_html_review(
      fixture$candidate,
      path = fixture$path
    ),
    "Review rendered:"
  )

  output <- file.path(fixture$path, "betwixt-review.html")

  expect_true(file.exists(output))
  expect_type(html, "character")
  expect_true(nzchar(html))
})

test_that("local HTML preserves relative media references", {
  fixture <- local_review_fixture()
  on.exit(unlink(fixture$path, recursive = TRUE), add = TRUE)

  suppressMessages(render_local_html_review(
    fixture$candidate,
    path = fixture$path
  ))

  html <- paste(
    readLines(file.path(fixture$path, "betwixt-review.html")),
    collapse = "\n"
  )

  expect_match(html, "media/photo.jpg", fixed = TRUE)
  expect_match(html, "media/record.jpg", fixed = TRUE)
})

test_that("local HTML contains candidate labels", {
  fixture <- local_review_fixture()
  on.exit(unlink(fixture$path, recursive = TRUE), add = TRUE)

  html <- suppressMessages(render_local_html_review(
    fixture$candidate,
    path = fixture$path
  ))

  expect_match(html, "Delini sash", fixed = TRUE)
  expect_match(html, "Delini floor plan", fixed = TRUE)
})

test_that("local HTML respects filename and sequence", {
  fixture <- local_review_fixture()
  on.exit(unlink(fixture$path, recursive = TRUE), add = TRUE)

  suppressMessages(render_local_html_review(
    fixture$candidate,
    filename_stem = "delini-review",
    sequence = 2L,
    path = fixture$path
  ))

  expect_true(file.exists(
    file.path(fixture$path, "delini-review_2.html")
  ))
})

test_that("local HTML rejects missing media resources", {
  fixture <- local_review_fixture()
  on.exit(unlink(fixture$path, recursive = TRUE), add = TRUE)

  fixture$candidate$input_media_url[1] <- "media/missing.jpg"

  expect_error(
    render_local_html_review(
      fixture$candidate,
      path = fixture$path
    ),
    "Missing local resource"
  )
})

test_that("local HTML rejects nonexistent output directories", {
  fixture <- local_review_fixture()
  on.exit(unlink(fixture$path, recursive = TRUE), add = TRUE)

  expect_error(
    render_local_html_review(
      fixture$candidate,
      path = file.path(fixture$path, "missing")
    ),
    "Review directory does not exist"
  )
})

test_that("local HTML rejects invalid review sequences", {
  fixture <- local_review_fixture()
  on.exit(unlink(fixture$path, recursive = TRUE), add = TRUE)

  expect_error(
    render_local_html_review(
      fixture$candidate,
      sequence = -1L,
      path = fixture$path
    ),
    "sequence must be"
  )
})

test_that("render_review dispatches to local HTML", {
  fixture <- local_review_fixture()
  on.exit(unlink(fixture$path, recursive = TRUE), add = TRUE)

  suppressMessages(render_review(
    fixture$candidate,
    type = "local_html",
    filename_stem = "dispatch-test",
    path = fixture$path
  ))

  expect_true(file.exists(
    file.path(fixture$path, "dispatch-test.html")
  ))

  # The dispatcher forwards the modal target.
  modal_html <- suppressMessages(render_review(
    fixture$candidate,
    type = "local_html",
    input_link_target = "modal",
    filename_stem = "dispatch-modal",
    path = fixture$path
  ))

  expect_true(file.exists(
    file.path(fixture$path, "dispatch-modal.html")
  ))
  expect_match(modal_html, 'data-open="modal"', fixed = TRUE)
})


# -------------------------------------------------------------------------
# Rendering of local input evidence
# -------------------------------------------------------------------------

test_that("local HTML evidence links support blank and modal targets", {
  fixture <- local_review_fixture()
  on.exit(unlink(fixture$path, recursive = TRUE), add = TRUE)

  # Extract evidence links without matching embedded JavaScript.
  evidence_links <- function(html) {
    matches <- gregexpr(
      '<a class="evidence-(media-)?link(?: [^"]+)?"[^>]*>',
      html,
      perl = TRUE
    )
    regmatches(html, matches)[[1]]
  }

  # Default behaviour remains a new browser tab.
  default_html <- suppressMessages(render_local_html_review(
    fixture$candidate,
    path = fixture$path
  ))

  blank_html <- suppressMessages(render_local_html_review(
    fixture$candidate,
    input_link_target = "blank",
    path = fixture$path
  ))

  blank_links <- evidence_links(blank_html)

  expect_identical(default_html, blank_html)
  expect_length(blank_links, 4L)
  expect_true(all(grepl('target="_blank"', blank_links, fixed = TRUE)))
  expect_true(all(grepl('rel="noopener"', blank_links, fixed = TRUE)))
  expect_false(any(grepl('data-open="modal"', blank_links, fixed = TRUE)))

  # Modal behaviour applies to both evidence links and images.
  modal_html <- suppressMessages(render_local_html_review(
    fixture$candidate,
    input_link_target = "modal",
    path = fixture$path
  ))

  modal_links <- evidence_links(modal_html)

  expect_length(modal_links, 4L)
  expect_true(all(grepl('target="evidence-viewer"', modal_links, fixed = TRUE)))
  expect_true(all(grepl('data-open="modal"', modal_links, fixed = TRUE)))
  expect_false(any(grepl('target="_blank"', modal_links, fixed = TRUE)))

  # Both resource links and image links use the modal target.
  expect_match(
    modal_html,
    paste0(
      '<a class="evidence-link input-url" ',
      'href="media/article-1.html" ',
      'title="media/article-1.html" ',
      'target="evidence-viewer" data-open="modal">'
    ),
    fixed = TRUE
  )
  expect_match(
    modal_html,
    '<a class="evidence-media-link" href="media/photo.jpg" target="evidence-viewer" data-open="modal">',
    fixed = TRUE
  )

  # Local evidence paths remain unchanged.
  expect_match(modal_html, 'src="media/photo.jpg"', fixed = TRUE)
  expect_match(modal_html, 'src="media/record.jpg"', fixed = TRUE)
  expect_match(modal_html, 'href="media/article-1.html"', fixed = TRUE)
  expect_match(modal_html, 'href="media/article-2.html"', fixed = TRUE)
})


test_that("local HTML validates input link targets", {
  fixture <- local_review_fixture()
  on.exit(unlink(fixture$path, recursive = TRUE), add = TRUE)

  # Invalid target values are rejected.
  invalid_targets <- list(
    "popup",
    "_blank",
    NA_character_,
    NULL,
    c("blank", "modal")
  )

  for (target in invalid_targets) {
    expect_error(
      render_local_html_review(
        fixture$candidate,
        input_link_target = target,
        path = fixture$path
      ),
      "input_link_target"
    )
  }

  # Valid target values are accepted.
  for (target in c("blank", "modal")) {
    expect_no_error(
      suppressMessages(render_local_html_review(
        fixture$candidate,
        input_link_target = target,
        path = fixture$path
      ))
    )
  }
})


# -------------------------------------------------------------------------
# Table identity
# -------------------------------------------------------------------------

test_that("local review preserves candidate table identity", {
  candidate <- create_candidate_dataset(
    subject = "Example",
    project_id = "example-project",
    table_id = "example-table"
  )

  html <- render_local_html_review(candidate)

  expect_match(
    html,
    'id="project-id" type="text" value="example-project" readonly',
    fixed = TRUE
  )

  expect_match(
    html,
    'id="table-id" type="text" value="example-table" readonly',
    fixed = TRUE
  )

  expect_match(
    html,
    'id="table-iri" type="text" value="https://example.com/example-project/example-table" readonly',
    fixed = TRUE
  )
})


test_that("local review preserves an explicit table IRI", {
  candidate <- create_candidate_dataset(
    subject = "Example",
    project_id = "example-project",
    table_id = "example-table",
    table_iri = "https://data.example.org/tables/123"
  )

  html <- render_local_html_review(candidate)

  expect_match(
    html,
    'id="table-iri" type="text" value="https://data.example.org/tables/123" readonly',
    fixed = TRUE
  )
})


test_that("local review uses candidate identity over renderer arguments", {
  candidate <- create_candidate_dataset(
    subject = "Example",
    project_id = "candidate-project",
    table_id = "candidate-table"
  )

  html <- render_local_html_review(
    candidate,
    project_id = "renderer-project",
    table_id = "renderer-table"
  )

  expect_match(
    html,
    'id="project-id" type="text" value="candidate-project" readonly',
    fixed = TRUE
  )

  expect_match(
    html,
    'id="table-id" type="text" value="candidate-table" readonly',
    fixed = TRUE
  )
})


test_that("local review supports missing table identity", {
  candidate <- create_candidate_dataset(
    subject = "Example"
  )

  html <- render_local_html_review(candidate)

  expect_match(
    html,
    'id="project-id" type="text" value="" readonly',
    fixed = TRUE
  )

  expect_match(
    html,
    'id="table-id" type="text" value="" readonly',
    fixed = TRUE
  )

  expect_match(
    html,
    'id="table-iri" type="text" value="" readonly',
    fixed = TRUE
  )
})


test_that("local review accepts an explicit renderer table IRI", {
  candidate <- create_candidate_dataset(
    subject = "Example"
  )

  html <- render_local_html_review(
    candidate,
    project_id = "finfair",
    table_id = "udmurt_clothing_example-1",
    table_iri = "https://doi.org/10.5281/zenodo.1234567"
  )

  expect_match(
    html,
    'id="table-iri" type="text" value="https://doi.org/10.5281/zenodo.1234567" readonly',
    fixed = TRUE
  )
})
