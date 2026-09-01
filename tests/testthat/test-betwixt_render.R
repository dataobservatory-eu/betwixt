test_that("betwixt_render() writes an initial Delini review", {
  path <- tempdir()

  betwixt_render(
    delini,
    cols = c(
      col_1 = "Subject",
      col_2 = "instance of",
      col_3 = "heritage of",
      context_1 = "held by"
    ),
    subheadings = c(
      col_2 = "wdt:P31",
      col_3 = "controlled range"
    ),
    title = "Delini semantic review",
    description = "Review the proposed semantic assertions.",
    filename_stem = "delini_wide",
    project_id = "delini",
    sequence = 0L,
    path = path
  )

  output <- file.path(
    path,
    "delini_wide.html"
  )

  expect_true(file.exists(output))

  html <- paste(
    readLines(output, warn = FALSE),
    collapse = "\n"
  )

  # Check the document identity.
  expect_match(html, "Delini semantic review", fixed = TRUE)
  expect_match(html, 'id="project-id"', fixed = TRUE)
  expect_match(html, 'value="delini"', fixed = TRUE)
  expect_match(html, 'id="filename-stem"', fixed = TRUE)
  expect_match(html, 'value="delini_wide"', fixed = TRUE)
  expect_match(html, 'id="review-sequence"', fixed = TRUE)
  expect_match(html, 'value="0"', fixed = TRUE)

  # Check the initial review structure and state.
  expect_match(html, 'id="review-table"', fixed = TRUE)
  expect_match(html, 'data-finalised="false"', fixed = TRUE)
  expect_match(html, 'data-qualification="none"', fixed = TRUE)
  expect_match(html, 'value="in-progress"', fixed = TRUE)

  # Check that both review save operations are available.
  expect_match(html, 'id="save-draft"', fixed = TRUE)
  expect_match(html, 'id="save-final"', fixed = TRUE)

  # Check the JavaScript review contract.
  expect_match(
    html,
    'document.getElementById("filename-stem")',
    fixed = TRUE
  )
  expect_match(
    html,
    'document.getElementById("review-sequence")',
    fixed = TRUE
  )
  expect_match(
    html,
    '`${filenameStem}_${sequence}`',
    fixed = TRUE
  )
  expect_match(
    html,
    '"-betwixt-draft.html"',
    fixed = TRUE
  )
  expect_match(
    html,
    '"-betwixt-finalised.html"',
    fixed = TRUE
  )

  # Check the JavaScript interaction hooks.
  expect_match(
    html,
    'querySelectorAll("[data-qualify]")',
    fixed = TRUE
  )
  expect_match(
    html,
    'querySelector(".finalise-check")',
    fixed = TRUE
  )
  expect_match(
    html,
    'querySelector(".create-item")',
    fixed = TRUE
  )
})
