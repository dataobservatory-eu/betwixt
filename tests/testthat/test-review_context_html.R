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
