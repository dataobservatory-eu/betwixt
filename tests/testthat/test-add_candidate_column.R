# Create named candidate columns ---------------------------------------------

test_that("add_candidate_column() creates a named candidate triplet", {
  x <- w3c_life_expectancy |> dplyr::select(observation)

  result <- add_candidate_column(
    x,
    name = "area",
    value = w3c_life_expectancy$area,
    range = "Newport | Cardiff | Monmouthshire | Merthyr Tydfil | Other...",
    definition = "http://purl.org/linked-data/sdmx/2009/dimension#refArea"
  )

  expect_s3_class(result, "data.frame")
  expect_true(all(c("area", "area_range", "area_definition") %in% names(result)))
  expect_equal(result$area, w3c_life_expectancy$area)
  expect_equal(
    result$area_range,
    rep(
      "Newport | Cardiff | Monmouthshire | Merthyr Tydfil | Other...",
      nrow(w3c_life_expectancy)
    )
  )
  expect_equal(
    result$area_definition,
    rep(
      "http://purl.org/linked-data/sdmx/2009/dimension#refArea",
      nrow(w3c_life_expectancy)
    )
  )
})


# Preserve existing data -----------------------------------------------------

test_that("add_candidate_column() preserves existing candidate columns", {
  result <- add_candidate_column(
    delini,
    name = "test_candidate",
    value = rep("new candidate", nrow(delini))
  )

  expect_equal(result$subject, delini$subject)
  expect_equal(result$instance_of, delini$instance_of)
  expect_equal(result$heritage_of, delini$heritage_of)
  expect_equal(result$subject_range, delini$subject_range)
  expect_equal(result$instance_of_definition, delini$instance_of_definition)
})

test_that("add_candidate_column() preserves non-candidate columns", {
  result <- add_candidate_column(
    delini,
    name = "test_candidate",
    value = rep("new candidate", nrow(delini))
  )

  expect_equal(result[names(delini)], delini)
})


# Defaults -------------------------------------------------------------------

test_that("add_candidate_column() uses NA defaults", {
  x <- w3c_life_expectancy |> dplyr::select(observation)

  result <- add_candidate_column(
    x,
    name = "life_expectancy",
    value = w3c_life_expectancy$life_expectancy
  )

  expect_equal(result$life_expectancy, w3c_life_expectancy$life_expectancy)
  expect_true(all(is.na(result$life_expectancy_range)))
  expect_true(all(is.na(result$life_expectancy_definition)))
  expect_type(result$life_expectancy_range, "character")
  expect_type(result$life_expectancy_definition, "character")
})


# Successive candidate columns -----------------------------------------------

test_that("add_candidate_column() can be called successively", {
  x <- w3c_life_expectancy |> dplyr::select(observation)

  result <- x |>
    add_candidate_column("area", w3c_life_expectancy$area) |>
    add_candidate_column("period", w3c_life_expectancy$period) |>
    add_candidate_column("sex", w3c_life_expectancy$sex) |>
    add_candidate_column("life_expectancy", w3c_life_expectancy$life_expectancy)

  expected <- c("area", "period", "sex", "life_expectancy")
  expect_true(all(expected %in% names(result)))
  expect_equal(result$area, w3c_life_expectancy$area)
  expect_equal(result$period, w3c_life_expectancy$period)
  expect_equal(result$sex, w3c_life_expectancy$sex)
  expect_equal(result$life_expectancy, w3c_life_expectancy$life_expectancy)
})


# Row integrity ---------------------------------------------------------------

test_that("add_candidate_column() preserves row count and order", {
  result <- add_candidate_column(
    delini,
    name = "test_candidate",
    value = seq_len(nrow(delini))
  )

  expect_equal(nrow(result), nrow(delini))
  expect_equal(result$row_number, delini$row_number)
  expect_equal(result$evidence_text, delini$evidence_text)
})
