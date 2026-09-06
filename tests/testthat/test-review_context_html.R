test_that("subject definitions identify resolved entities", {
  context <- prepare_review_context(betwixt::delini)

  expect_true(is.na(
    context$rows[[2]]$assertions[[1]]$definition
  ))
})

test_that("URL candidate values are rendered as links", {
  x <- candidate_dataset(
    evidence_url = "https://example.org/evidence.jpg",
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
    evidence_url = "https://example.org/evidence.jpg",
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
