test_that("default template can be loaded", {
  template <- get_template()

  expect_type(template, "character")
  expect_length(template, 1)
  expect_match(template, "<table>")
})

test_that("default template contains claim placeholders", {
  template <- get_template()

  expect_match(template, "{{#claims}}", fixed = TRUE)
  expect_match(template, "{{scope}}", fixed = TRUE)
  expect_match(template, "{{subject}}", fixed = TRUE)
  expect_match(template, "{{predicate}}", fixed = TRUE)
  expect_match(template, "{{value}}", fixed = TRUE)
})

test_that("image review template can be loaded", {
  template <- get_template("image_review")

  expect_type(template, "character")
  expect_length(template, 1)
  expect_match(template, "{{#claims}}", fixed = TRUE)
})

test_that("wide review template bundle can be loaded", {
  template <- get_template("wide_review")

  expect_type(template, "list")
  expect_named(template, c("html", "css", "js"))

  expect_type(template$html, "character")
  expect_type(template$css, "character")
  expect_type(template$js, "character")

  expect_length(template$html, 1)
  expect_length(template$css, 1)
  expect_length(template$js, 1)
})

test_that("wide review template contains review placeholders", {
  template <- get_template("wide_review")

  expect_match(template$html, "{{#claims}}", fixed = TRUE)
  expect_match(template$html, "{{#description_columns}}", fixed = TRUE)
  expect_match(template$html, "{{#assertion_columns}}", fixed = TRUE)
  expect_match(template$html, "{{#context_columns}}", fixed = TRUE)
  expect_match(template$html, "{{{betwixt_css}}}", fixed = TRUE)
  expect_match(template$html, "{{{betwixt_js}}}", fixed = TRUE)
})

test_that("long review template bundle can be loaded", {
  template <- get_template("long_review")

  expect_type(template, "list")
  expect_named(template, c("html", "css", "js"))

  expect_type(template$html, "character")
  expect_type(template$css, "character")
  expect_type(template$js, "character")

  expect_length(template$html, 1)
  expect_length(template$css, 1)
  expect_length(template$js, 1)
})

test_that("long review template contains review placeholders", {
  template <- get_template("long_review")

  expect_match(template$html, "{{#claims}}", fixed = TRUE)
  expect_match(template$html, "{{#description_columns}}", fixed = TRUE)
  expect_match(template$html, "{{#context_columns}}", fixed = TRUE)
  expect_match(template$html, "{{{betwixt_css}}}", fixed = TRUE)
  expect_match(template$html, "{{{betwixt_js}}}", fixed = TRUE)
})
