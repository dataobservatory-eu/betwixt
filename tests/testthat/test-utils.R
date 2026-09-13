test_that("betwixt version works", {
  expect_true(is.character(betwixt_version()))
  expect_true(grepl(pattern = "0.0", x = betwixt_version()))
})

test_that("escape_html escapes HTML special characters", {
  expect_equal(
    escape_html('A & B <tag> "quoted"'),
    "A &amp; B &lt;tag&gt; &quot;quoted&quot;"
  )
})

test_that("escape_html coerces values to character", {
  expect_equal(escape_html(123), "123")
})

test_that("qualification_html renders qualification controls", {
  html <- qualification_html()

  expect_match(html, 'class="qualify"', fixed = TRUE)
  expect_match(html, 'data-qualify="defer"', fixed = TRUE)
  expect_match(html, 'data-qualify="reject"', fixed = TRUE)
})
