test_that("betwixt_render returns HTML as character", {
  x <- claim(
    scope = "country=AD;year=2023",
    subject = "country",
    predicate = "GDP",
    value = "3.73 billion EUR"
  )

  html <- betwixt_render(x)

  expect_type(html, "character")
  expect_length(html, 1)
})

test_that("betwixt_render contains claim values", {
  x <- claim(
    scope = "country=AD;year=2023",
    subject = "country",
    predicate = "GDP",
    value = "3.73 billion EUR"
  )

  html <- betwixt_render(x)

  expect_match(html, "country=AD;year=2023")
  expect_match(html, "GDP")
  expect_match(html, "3.73 billion EUR")
})


test_that("betwixt_render writes to a connection", {
  x <- claim(
    scope = "box45",
    subject = "page",
    predicate = "language",
    value = "Latin"
  )

  tf <- tempfile(fileext = ".html")

  con <- file(tf, open = "w")

  expect_invisible(
    betwixt_render(x, con = con)
  )

  close(con)

  expect_true(file.exists(tf))

  html <- paste(readLines(tf), collapse = "\n")

  expect_match(html, "Latin")
})

test_that("betwixt_render handles multiple claims", {
  claims <- as_claim_df(
    data.frame(
      scope = c("box45", "box46"),
      subject = c("page", "page"),
      predicate = c("language", "language"),
      value = c("Latin", "German")
    )
  )

  html <- betwixt_render(claims)

  expect_match(html, "Latin")
  expect_match(html, "German")
  expect_match(html, "box45")
  expect_match(html, "box46")
})
