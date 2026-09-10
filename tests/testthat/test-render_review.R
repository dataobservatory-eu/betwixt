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

  # Document identity.
  expect_match(html, "Delini semantic review", fixed = TRUE)
  expect_match(html, 'value="delini"', fixed = TRUE)
  expect_match(html, 'value="delini_wide"', fixed = TRUE)
  expect_match(html, 'value="delini_wide.html"', fixed = TRUE)
  expect_match(html, 'value="0"', fixed = TRUE)

  # Semantic review structure.
  expect_match(html, 'data-column="subject"', fixed = TRUE)
  expect_match(html, 'data-column="instance_of"', fixed = TRUE)
  expect_match(html, 'data-column="heritage_of"', fixed = TRUE)
  expect_false(grepl('data-column="col_', html, fixed = TRUE))

  # Initial review state.
  expect_match(html, 'id="review-table"', fixed = TRUE)
  expect_match(html, 'data-finalised="false"', fixed = TRUE)
  expect_match(html, 'data-qualification="none"', fixed = TRUE)
  expect_match(html, 'value="in-progress"', fixed = TRUE)

  # Review lifecycle controls.
  expect_match(html, 'id="save-draft"', fixed = TRUE)
  expect_match(html, 'id="save-final"', fixed = TRUE)
})

test_that("render_review() preserves candidate row numbers", {
  candidate <- delini[1:3, ]

  html <- render_review(candidate)

  expect_match(html, 'data-row="1"', fixed = TRUE)
  expect_match(html, 'data-row="2"', fixed = TRUE)
  expect_match(html, 'data-row="3"', fixed = TRUE)
})

test_that("render_review() records a sequenced original filename", {
  html <- render_review(
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
  html <- render_review(delini)

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
  html <- render_review(
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
  html <- render_review(
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


# Numeric candidate fixture ------------------------------------------------

numeric_candidate <- data.frame(
  row_number = 1:4,
  subject = c("Liechtenstein", "Liechtenstein", "Malta", "Malta"),
  gdp = c(7365.4, 7812.6, 17927.3, 19147.2),
  gdp_definition =
    "http://dd.eionet.europa.eu/vocabulary/eurostat/na_item/B1GQ",
  context_year = c(2023L, 2024L, 2023L, 2024L)
)


# Numeric candidate preparation -------------------------------------------

test_that("prepare_review_context() preserves numeric candidates", {
  context <- prepare_review_context(numeric_candidate)
  gdp <- lapply(context$rows, \(row) row$assertions[[1]]$value)

  expect_equal(unlist(gdp), numeric_candidate$gdp)
  expect_true(all(vapply(gdp, is.numeric, logical(1))))
})


# Numeric candidate rendering ---------------------------------------------

test_that("render_review() renders numeric candidate values", {
  html <- render_review(numeric_candidate)

  expect_match(html, "7365.4", fixed = TRUE)
  expect_match(html, "7812.6", fixed = TRUE)
  expect_match(html, "17927.3", fixed = TRUE)
  expect_match(html, "19147.2", fixed = TRUE)
})

