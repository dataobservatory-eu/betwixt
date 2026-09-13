test_that("assertion without range or definition is editable", {
  assertion <- list(
    name = "instance_of",
    value = "Farmhouse",
    range = character(),
    definition = NA_character_
  )

  html <- assertion_html(assertion)

  expect_match(
    html,
    '<input class="subject-input" type="text" value="Farmhouse">',
    fixed = TRUE
  )
  expect_match(html, "Create new item", fixed = TRUE)
})

test_that("assertion with definition and no range is linked", {
  assertion <- list(
    name = "instance_of",
    value = "Farmhouse",
    range = character(),
    definition = "https://example.org/farmhouse"
  )

  html <- assertion_html(assertion)

  expect_match(
    html,
    '<a class="entity-link" href="https://example.org/farmhouse"',
    fixed = TRUE
  )
  expect_match(html, ">Farmhouse</a>", fixed = TRUE)
})

test_that("assertion with range and no definition is controlled", {
  assertion <- list(
    name = "instance_of",
    value = "Farmhouse",
    range = c("Farmhouse", "Dwelling", "Other\u2026"),
    definition = NA_character_
  )

  html <- assertion_html(assertion)

  expect_match(
    html,
    '<select class="candidate-select">',
    fixed = TRUE
  )
  expect_match(
    html,
    '<option value="Farmhouse" selected>Farmhouse</option>',
    fixed = TRUE
  )
  expect_match(
    html,
    '<option value="__other__">Other\u2026</option>',
    fixed = TRUE
  )
  expect_false(grepl("definition-link", html, fixed = TRUE))
})

test_that("assertion with range and definition renders both", {
  assertion <- list(
    name = "instance_of",
    value = "Farmhouse",
    range = c("Farmhouse", "Dwelling", "Other\u2026"),
    definition = "https://example.org/farmhouse"
  )

  html <- assertion_html(assertion)

  expect_match(
    html,
    '<select class="candidate-select">',
    fixed = TRUE
  )
  expect_match(
    html,
    '<option value="Farmhouse" selected>Farmhouse</option>',
    fixed = TRUE
  )
  expect_match(
    html,
    '<a class="definition-link" href="https://example.org/farmhouse"',
    fixed = TRUE
  )
  expect_match(html, ">Definition</a>", fixed = TRUE)
})
