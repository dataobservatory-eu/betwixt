# Tests for assemble_review_html()
# --------------------------------

make_test_review_html <- function(
  project_id = "example-project",
  table_id = "example-table",
  table_iri = "https://example.com/example-project/example-table"
) {
  betwixt:::assemble_review_html(
    title = "Example Review",
    description = "Review the candidate assertions.",
    table_html = "<table></table>",
    css = "body { margin: 0; }",
    js = "console.log('test');",
    reviewer_name = "Example Reviewer",
    reviewer_iri = "https://orcid.org/0000-0001-2345-6789",
    project_id = project_id,
    table_id = table_id,
    table_iri = table_iri,
    sequence = 0L,
    original_filename = "example-review.html",
    original_created_at = "2026-11-22T12:00:00Z",
    filename_stem = "example-review",
    review_comment_html = "",
    provenance_footer = "<div>Candidate provenance</div>"
  )
}


test_that("assembler produces a standalone HTML document", {
  html <- make_test_review_html()

  expect_type(html, "character")
  expect_length(html, 1L)

  expect_match(html, "<!doctype html>", fixed = TRUE)
  expect_match(html, '<html lang="en">', fixed = TRUE)
  expect_match(html, "<title>Example Review</title>", fixed = TRUE)
  expect_match(html, "<table></table>", fixed = TRUE)
  expect_match(html, "</html>", fixed = TRUE)
})


test_that("assembler preserves project and table identity", {
  html <- make_test_review_html()

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
    paste0(
      'id="table-iri" type="text" value="',
      "https://example.com/example-project/example-table",
      '" readonly'
    ),
    fixed = TRUE
  )
})


test_that("assembler preserves an explicit table IRI", {
  html <- make_test_review_html(
    table_iri = "https://data.example.org/tables/123"
  )

  expect_match(
    html,
    'id="table-iri" type="text" value="https://data.example.org/tables/123" readonly',
    fixed = TRUE
  )
})


test_that("assembler permits an empty table IRI", {
  html <- make_test_review_html(
    project_id = "example-project",
    table_id = "",
    table_iri = ""
  )

  expect_match(
    html,
    'id="table-iri" type="text" value="" readonly',
    fixed = TRUE
  )
})


test_that("assembler does not generate or modify table identity", {
  html <- make_test_review_html(
    project_id = "another-project",
    table_id = "another-table",
    table_iri = "https://data.example.org/custom-identifier"
  )

  expect_match(
    html,
    'id="project-id" type="text" value="another-project" readonly',
    fixed = TRUE
  )

  expect_match(
    html,
    'id="table-id" type="text" value="another-table" readonly',
    fixed = TRUE
  )

  expect_match(
    html,
    'id="table-iri" type="text" value="https://data.example.org/custom-identifier" readonly',
    fixed = TRUE
  )
})


test_that("assembler escapes special characters in table identity", {
  html <- make_test_review_html(
    project_id = "project&test",
    table_id = 'table"test',
    table_iri = "https://example.org/table?a=1&b=2"
  )

  expect_match(
    html,
    'id="project-id" type="text" value="project&amp;test" readonly',
    fixed = TRUE
  )

  expect_match(
    html,
    'id="table-id" type="text" value="table&quot;test" readonly',
    fixed = TRUE
  )

  expect_match(
    html,
    'id="table-iri" type="text" value="https://example.org/table?a=1&amp;b=2" readonly',
    fixed = TRUE
  )
})


test_that("assembler preserves review process metadata", {
  html <- make_test_review_html()

  expect_match(
    html,
    'id="review-sequence" type="number" value="0" readonly',
    fixed = TRUE
  )

  expect_match(
    html,
    'id="original-filename" type="text" value="example-review.html" readonly',
    fixed = TRUE
  )

  expect_match(
    html,
    'id="filename-stem" type="hidden" value="example-review"',
    fixed = TRUE
  )

  expect_match(
    html,
    'id="review-status" type="text" value="in-progress" readonly',
    fixed = TRUE
  )
})
