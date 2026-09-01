context <- prepare_review_context(delini)

table_html <- review_context_html(
  context,
  cols = c(
    col_1 = "Subject",
    col_2 = "instance of",
    col_3 = "heritage of",
    context_1 = "held by"
  ),
  subheadings = c(
    col_2 = "wdt:P31",
    col_3 = "controlled range"
  )
)

writeLines(table_html, "delini_generated_table.html")
