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
    '<option value="__other__">Other [curator entered]</option>',
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

# Special candidate-range values -------------------------------------------

test_that("legacy and canonical other options activate free-text entry", {
  assertion <- list(
    name = "instance_of",
    value = "Farmhouse",
    range = c("Farmhouse", "Other\u2026", "Other...", "<other>"),
    definition = NA_character_
  )

  html <- assertion_html(assertion)

  expect_match(html, '<option value="__other__">Other [curator entered]</option>', fixed = TRUE)
  expect_equal(
    lengths(regmatches(html, gregexpr('value="__other__"', html, fixed = TRUE))),
    3L
  )
  expect_match(html, '<input class="write-in"', fixed = TRUE)
})

test_that("reserved semantic values have canonical identifiers", {
  assertion <- list(
    name = "instance_of",
    value = "Farmhouse",
    range = c("Farmhouse", "<unknown>", "<no-value>"),
    definition = NA_character_
  )

  html <- assertion_html(assertion)

  expect_match(html, '<option value="__unknown__">Value unknown</option>', fixed = TRUE)
  expect_match(html, '<option value="__no_value__">No value possible</option>', fixed = TRUE)
  expect_match(html, '<option value="Farmhouse" selected>Farmhouse</option>', fixed = TRUE)
})

test_that("ordinary values remain distinct from reserved tokens", {
  assertion <- list(
    name = "instance_of",
    value = "Other",
    range = c("Other", "Other...", "Unknown", "None", "<unknown>"),
    definition = NA_character_
  )

  html <- assertion_html(assertion)

  expect_match(html, '<option value="Other" selected>Other</option>', fixed = TRUE)
  expect_match(html, '<option value="__other__">Other [curator entered]</option>', fixed = TRUE)
  expect_match(html, '<option value="Unknown">Unknown</option>', fixed = TRUE)
  expect_match(html, '<option value="None">None</option>', fixed = TRUE)
  expect_match(html, '<option value="__unknown__">Value unknown</option>', fixed = TRUE)
})


test_that("reserved candidate values retain their selection", {
  for (candidate in c("<other>", "<unknown>", "<no-value>")) {
    assertion <- list(
      name = "instance_of",
      value = candidate,
      range = c("Farmhouse", "<other>", "<unknown>", "<no-value>"),
      definition = NA_character_
    )

    html <- assertion_html(assertion)

    expected_value <- switch(candidate,
      "<other>" = "__other__",
      "<unknown>" = "__unknown__",
      "<no-value>" = "__no_value__"
    )

    expect_match(
      html,
      paste0('<option value="', expected_value, '" selected>'),
      fixed = TRUE
    )
  }
})
