test_that("multiplication works", {
  betwixt_render(
    delini,
    cols = c(
      col_1 = "Subject",
      col_2 = "instance of",
      col_3 = "heritage of",
      context_1 = "held by"
    ),
    subheadings = c(
      col_2 = "wdt:P31",
      col_3 = "controlled range"
    ),
    title = "Delini semantic review",
    description = "Review the proposed semantic assertions.",
    project_id = "delini_wide",
    sequence = 0L,
    con = "delini_wide.html"
  )


  expect_equal(2 * 2, 4)
})
