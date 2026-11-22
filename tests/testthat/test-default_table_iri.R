test_that("default_table_iri() preserves explicit IRIs", {
  expect_identical(
    default_table_iri(
      "https://data.example.org/table/123",
      "table-1",
      "project-1"
    ),
    "https://data.example.org/table/123"
  )
})

test_that("default_table_iri() generates fallback IRIs", {
  expect_identical(
    default_table_iri(
      table_id = "table-1",
      project_id = "project-1"
    ),
    "https://example.com/project-1/table-1"
  )

  expect_identical(
    default_table_iri(table_id = "table-1"),
    "https://example.com/table-1"
  )

  expect_identical(
    default_table_iri(),
    ""
  )
})
