test_that("add_candidate_column() creates the first candidate triplet", {
  x <- w3c_life_expectancy |>
    dplyr::select(observation)

  result <- add_candidate_column(
    x,
    value = w3c_life_expectancy$area,
    range = "Newport | Cardiff | Monmouthshire | Merthyr Tydfil | Other...",
    definition = "http://purl.org/linked-data/sdmx/2009/dimension#refArea"
  )

  expect_s3_class(result, "data.frame")

  expect_true(all(c(
    "col_1",
    "col_1_range",
    "col_1_definition"
  ) %in% names(result)))

  expect_equal(
    result$col_1,
    w3c_life_expectancy$area
  )

  expect_equal(
    result$col_1_range,
    rep(
      "Newport | Cardiff | Monmouthshire | Merthyr Tydfil | Other...",
      nrow(w3c_life_expectancy)
    )
  )

  expect_equal(
    result$col_1_definition,
    rep(
      "http://purl.org/linked-data/sdmx/2009/dimension#refArea",
      nrow(w3c_life_expectancy)
    )
  )
})


test_that("add_candidate_column() appends the next numbered triplet", {
  result <- add_candidate_column(
    delini,
    value = rep("review candidate", nrow(delini)),
    range = rep("candidate | Other...", nrow(delini)),
    definition = rep(
      "https://example.org/property/test",
      nrow(delini)
    )
  )

  expect_true(all(c(
    "col_4",
    "col_4_range",
    "col_4_definition"
  ) %in% names(result)))

  expect_equal(
    result$col_4,
    rep("review candidate", nrow(delini))
  )

  expect_equal(
    result$col_4_range,
    rep("candidate | Other...", nrow(delini))
  )

  expect_equal(
    result$col_4_definition,
    rep(
      "https://example.org/property/test",
      nrow(delini)
    )
  )
})


test_that("add_candidate_column() preserves existing candidate columns", {
  result <- add_candidate_column(
    delini,
    value = rep("new candidate", nrow(delini))
  )

  expect_equal(
    result$col_1,
    delini$col_1
  )

  expect_equal(
    result$col_2,
    delini$col_2
  )

  expect_equal(
    result$col_3,
    delini$col_3
  )

  expect_equal(
    result$col_1_range,
    delini$col_1_range
  )

  expect_equal(
    result$col_2_definition,
    delini$col_2_definition
  )
})


test_that("add_candidate_column() preserves non-candidate columns", {
  result <- add_candidate_column(
    delini,
    value = rep("new candidate", nrow(delini))
  )

  original_names <- names(delini)

  expect_equal(
    result[original_names],
    delini
  )
})


test_that("add_candidate_column() uses NA defaults", {
  x <- w3c_life_expectancy |>
    dplyr::select(observation)

  result <- add_candidate_column(
    x,
    value = w3c_life_expectancy$life_expectancy
  )

  expect_equal(
    result$col_1,
    w3c_life_expectancy$life_expectancy
  )

  expect_true(all(is.na(result$col_1_range)))
  expect_true(all(is.na(result$col_1_definition)))

  expect_type(result$col_1_range, "character")
  expect_type(result$col_1_definition, "character")
})


test_that("add_candidate_column() can be called successively", {
  x <- w3c_life_expectancy |>
    dplyr::select(observation)

  result <- x |>
    add_candidate_column(
      value = w3c_life_expectancy$area
    ) |>
    add_candidate_column(
      value = w3c_life_expectancy$period
    ) |>
    add_candidate_column(
      value = w3c_life_expectancy$sex
    ) |>
    add_candidate_column(
      value = w3c_life_expectancy$life_expectancy
    )

  expect_true(all(c(
    "col_1",
    "col_2",
    "col_3",
    "col_4"
  ) %in% names(result)))

  expect_equal(result$col_1, w3c_life_expectancy$area)
  expect_equal(result$col_2, w3c_life_expectancy$period)
  expect_equal(result$col_3, w3c_life_expectancy$sex)
  expect_equal(result$col_4, w3c_life_expectancy$life_expectancy)
})


test_that("add_candidate_column() uses the highest existing candidate number", {
  x <- tibble::tibble(
    observation = w3c_life_expectancy$observation,
    col_1 = w3c_life_expectancy$area,
    col_3 = w3c_life_expectancy$sex
  )

  result <- add_candidate_column(
    x,
    value = w3c_life_expectancy$life_expectancy
  )

  expect_true("col_4" %in% names(result))
  expect_false("col_2" %in% names(result))
})


test_that("add_candidate_column() preserves row count and order", {
  result <- add_candidate_column(
    delini,
    value = seq_len(nrow(delini))
  )

  expect_equal(nrow(result), nrow(delini))
  expect_equal(result$row_number, delini$row_number)
  expect_equal(result$evidence_text, delini$evidence_text)
})
