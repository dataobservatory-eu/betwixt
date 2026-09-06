test_that("subject definitions identify resolved entities", {
  context <- prepare_review_context(betwixt::delini)

  expect_true(is.na(
    context$rows[[2]]$assertions[[1]]$definition
  ))
})

test_that("URL candidate values are rendered as links", {
  x <- candidate_dataset(
    evidence_media_url = "https://example.org/evidence.jpg",
    evidence_text = "Example evidence",
    label = "Example",
    description = "Example description",
    subject = "[image shown]"
  ) |>
    add_candidate_column(
      value = "https://example.org/access-point"
    )

  context <- prepare_review_context(x)
  html <- review_context_html(context)

  expect_match(
    html,
    '<a class="entity-link" href="https://example.org/access-point"',
    fixed = TRUE
  )
})

test_that("URL subjects are not automatically rendered as links", {
  x <- candidate_dataset(
    evidence_media_url = "https://example.org/evidence.jpg",
    evidence_text = "Example evidence",
    label = "Example",
    description = "Example description",
    subject = "https://example.org/subject"
  )

  context <- prepare_review_context(x)
  html <- review_context_html(context)

  expect_false(grepl(
    '<a class="entity-link" href="https://example.org/subject"',
    html,
    fixed = TRUE
  ))
})


test_that("evidence_media_url is rendered as an image", {
  x <- candidate_dataset(
    evidence_media_url = "https://example.org/evidence.jpg",
    evidence_text = "Example evidence",
    label = "Example",
    description = "Example description",
    subject = "[image shown]"
  )

  html <- review_context_html(prepare_review_context(x))

  expect_match(
    html,
    '<img src="https://example.org/evidence.jpg"',
    fixed = TRUE
  )
})


test_that("evidence_url is rendered as a link without an image", {
  x <- candidate_dataset(
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

  expect_false(grepl(
    '<img src="https://example.org/evidence.html"',
    html,
    fixed = TRUE
  ))
})

test_that("multiple evidence URLs are rendered", {
  x <- candidate_dataset(
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


test_that("alternative descriptive columns are not rendered when unused", {
  x <- candidate_dataset(
    evidence_media_url = "https://example.org/evidence.jpg",
    evidence_text = "Example evidence",
    label = "Example",
    description = "Example description",
    subject = "[image shown]"
  )

  html <- review_context_html(prepare_review_context(x))

  expect_false(grepl(
    "<th>Alternative label</th>",
    html,
    fixed = TRUE
  ))

  expect_false(grepl(
    "<th>Alternative description</th>",
    html,
    fixed = TRUE
  ))
})


test_that("alternative label is rendered without alternative description", {
  x <- candidate_dataset(
    evidence_media_url = "https://example.org/evidence.jpg",
    evidence_text = "Example evidence",
    label = "Tablet-woven sash",
    description = "A tablet-woven textile object.",
    alternative_label = "Tablet-woven belt",
    subject = "[image shown]"
  )

  html <- review_context_html(prepare_review_context(x))

  expect_match(
    html,
    "<th>Alternative label</th>",
    fixed = TRUE
  )

  expect_match(
    html,
    'value="Tablet-woven belt"',
    fixed = TRUE
  )

  expect_false(grepl(
    "<th>Alternative description</th>",
    html,
    fixed = TRUE
  ))
})


test_that("alternative label and description are rendered together", {
  x <- candidate_dataset(
    evidence_media_url = "https://example.org/evidence.jpg",
    evidence_text = "Example evidence",
    label = "Tablet-woven sash",
    description = "A tablet-woven textile object.",
    alternative_label = "Tablet-woven belt",
    alternative_description =
      "A visitor-facing description of the textile object.",
    subject = "[image shown]"
  )

  html <- review_context_html(prepare_review_context(x))

  expect_match(
    html,
    "<th>Alternative label</th>",
    fixed = TRUE
  )

  expect_match(
    html,
    'value="Tablet-woven belt"',
    fixed = TRUE
  )

  expect_match(
    html,
    "<th>Alternative description</th>",
    fixed = TRUE
  )

  expect_match(
    html,
    "A visitor-facing description of the textile object.",
    fixed = TRUE
  )
})


test_that("row comment is not rendered by default", {
  x <- candidate_dataset(
    evidence_media_url = "https://example.org/evidence.jpg",
    evidence_text = "Example evidence",
    label = "Example",
    description = "Example description",
    subject = "[image shown]"
  )

  html <- review_context_html(prepare_review_context(x))

  expect_false(grepl(
    '<th class="row-comment">Reviewer comment</th>',
    html,
    fixed = TRUE
  ))

  expect_false(grepl(
    '<td class="row-comment">',
    html,
    fixed = TRUE
  ))
})


test_that("row comment is rendered when requested", {
  x <- candidate_dataset(
    evidence_media_url = "https://example.org/evidence.jpg",
    evidence_text = "Example evidence",
    label = "Example",
    description = "Example description",
    subject = "[image shown]"
  )

  html <- review_context_html(
    prepare_review_context(x),
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

  expect_match(
    html,
    '<textarea placeholder="Optional comment on row"></textarea>',
    fixed = TRUE
  )
})
