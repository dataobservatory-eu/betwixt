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
  expect_match(html, 'id="original-filename"', fixed = TRUE)
  expect_match(html, 'value="delini_wide.html"', fixed = TRUE)
  expect_match(html, 'id="review-filename"', fixed = TRUE)
  expect_match(html, 'id="original-created-at"', fixed = TRUE)
  expect_match(
    html,
    'class="semantic-cell" data-column="col_1"',
    fixed = TRUE
  )
  expect_match(
    html,
    'class="semantic-cell" data-column="col_2"',
    fixed = TRUE
  )

  # Check the initial review structure and state.
  expect_match(html, 'id="review-table"', fixed = TRUE)
  expect_match(html, 'data-finalised="false"', fixed = TRUE)
  expect_match(html, 'data-qualification="none"', fixed = TRUE)
  expect_match(html, 'value="in-progress"', fixed = TRUE)

  # Check the timestamp structure.
  expect_match(
    html,
    'id="original-created-at" type="text" value="\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}Z"'
  )

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
    "if (sequence === 0)",
    fixed = TRUE
  )
  expect_match(
    html,
    "sequence = 1",
    fixed = TRUE
  )
  expect_match(
    html,
    "sequenceInput.value = sequence",
    fixed = TRUE
  )
  expect_match(
    html,
    "const stem = `${filenameStem}_${sequence}`",
    fixed = TRUE
  )
  expect_match(
    html,
    'const suffix = finaliseReview ? "-finalised" : "-draft"',
    fixed = TRUE
  )
  expect_match(
    html,
    "const filename = `${stem}${suffix}.html`",
    fixed = TRUE
  )
  expect_match(
    html,
    "function utcTimestamp()",
    fixed = TRUE
  )
  expect_match(
    html,
    'toISOString().replace(/\\.\\d{3}Z$/, "Z")',
    fixed = TRUE
  )
  expect_match(
    html,
    "startedAt.value = utcTimestamp()",
    fixed = TRUE
  )
  expect_match(
    html,
    "const now = utcTimestamp()",
    fixed = TRUE
  )
  expect_match(
    html,
    'document.getElementById("review-filename").value = filename',
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
    'querySelectorAll\\("\\.create-item"\\)\\.forEach'
  )
  # Review controls are wired into the standalone document.
  expect_match(html, 'querySelectorAll\\("\\[data-qualify\\]"\\)')
  expect_match(html, 'querySelectorAll\\("\\.candidate-select"\\)')
  expect_match(html, 'querySelectorAll\\("\\.create-item"\\)\\.forEach')
  expect_match(html, 'querySelector\\("\\.finalise-check"\\)')

  # Review state is persisted into saved HTML.
  expect_match(html, "persistStateIntoClone")
  expect_match(html, "dataset\\.qualification")
  expect_match(html, "dataset\\.finalised")
  expect_match(html, "dataset\\.outcome")
  expect_match(html, 'class="create-item"')
  expect_match(html, 'querySelectorAll\\("\\.create-item"\\)\\.forEach')

  # Draft and final review actions remain available.
  expect_match(html, 'getElementById\\("save-draft"\\)')
  expect_match(html, 'getElementById\\("save-final"\\)')
})

test_that("betwixt_render() preserves candidate row numbers", {
  candidate <- delini[1:3, ]

  html <- betwixt_render(candidate)

  expect_match(html, 'data-row="1"', fixed = TRUE)
  expect_match(html, 'data-row="2"', fixed = TRUE)
  expect_match(html, 'data-row="3"', fixed = TRUE)
})

test_that("betwixt_render() records a sequenced original filename", {
  html <- betwixt_render(
    delini,
    filename_stem = "delini_wide",
    sequence = 2L
  )

  expect_match(
    html,
    'id="original-filename" type="text" value="delini_wide_2.html"',
    fixed = TRUE
  )

  expect_match(
    html,
    'id="review-filename" type="text" value="delini_wide_2.html"',
    fixed = TRUE
  )

  expect_match(
    html,
    'id="review-sequence" type="number" value="2"',
    fixed = TRUE
  )
})

test_that("comments are not rendered by default", {
  html <- betwixt_render(delini)

  expect_false(grepl(
    '<th class="row-comment">',
    html,
    fixed = TRUE
  ))

  expect_false(grepl(
    'id="review-comment"',
    html,
    fixed = TRUE
  ))
})


test_that("row comments are rendered when requested", {
  html <- betwixt_render(
    delini,
    row_comment = TRUE
  )

  expect_match(
    html,
    '<th class="row-comment">Reviewer comment</th>',
    fixed = TRUE
  )

  expect_match(
    html,
    '<td class="row-comment">',
    fixed = TRUE
  )

  expect_false(grepl(
    'id="review-comment"',
    html,
    fixed = TRUE
  ))
})

## Optional comments ---------------------------------------------------------
test_that("review comment is rendered when requested", {
  html <- betwixt_render(
    delini,
    review_comment = TRUE
  )

  expect_match(
    html,
    '<label class="review-comment">Review comment',
    fixed = TRUE
  )

  expect_match(
    html,
    'id="review-comment"',
    fixed = TRUE
  )

  expect_false(grepl(
    '<th class="row-comment">',
    html,
    fixed = TRUE
  ))
})
