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

