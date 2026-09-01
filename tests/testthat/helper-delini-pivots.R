data("delini")

## Delini long -----------------------------------------------------

delini_long <- delini |>
  dplyr::mutate(
    context_2 = NA_character_
  ) |>
  dplyr::rename(
    subject = col_1,
    subject_range = col_1_range,
    subject_definition = col_1_definition,
    instance_of__value = col_2,
    instance_of__value_range = col_2_range,
    instance_of__predicate_definition = col_2_definition,
    heritage_of__value = col_3,
    heritage_of__value_range = col_3_range,
    heritage_of__predicate_definition = col_3_definition
  ) |>
  tidyr::pivot_longer(
    cols = matches("^(instance_of|heritage_of)__"),
    names_to = c("claim", ".value"),
    names_sep = "__"
  ) |>
  dplyr::mutate(
    predicate = dplyr::case_when(
      claim == "instance_of" ~ "instance of",
      claim == "heritage_of" ~ "heritage of"
    ),
    predicate_range = NA_character_,
    value_definition = NA_character_
  ) |>
  dplyr::select(
    row_number,
    evidence_url,
    evidence_text,
    label,
    description,
    subject,
    subject_range,
    subject_definition,
    predicate,
    predicate_range,
    predicate_definition,
    value,
    value_range,
    value_definition,
    context_1,
    context_2
  ) |>
  dplyr::arrange(row_number)

## Dual wide -------------------------------------------------------
delini_dual_wide <- candidate_dataset(
  evidence_url = delini$evidence_url,
  evidence_text = delini$evidence_text,
  evidence_relation = rep("depicts", nrow(delini)),
  evidence_relation_range = candidate_range(
    "depicts",
    "documents",
    "is evidence for",
    "Other…"
  ),
  label = delini$label,
  description = delini$description,
  subject = delini$col_1,
  subject_range = delini$col_1_range,
  subject_definition = delini$col_1_definition
) |>
  add_candidate_column(
    value = delini$col_2,
    range = delini$col_2_range,
    definition = delini$col_2_definition
  ) |>
  add_candidate_column(
    value = delini$col_3,
    range = delini$col_3_range,
    definition = delini$col_3_definition
  ) |>
  dplyr::mutate(
    context_1 = delini$context_1
  )


## Dual long -------------------------------------------------------
delini_dual_long <- delini_dual_wide |>
  dplyr::mutate(
    context_2 = NA_character_
  ) |>
  dplyr::rename(
    subject = col_1,
    subject_range = col_1_range,
    subject_definition = col_1_definition,
    instance_of__value = col_2,
    instance_of__value_range = col_2_range,
    instance_of__predicate_definition = col_2_definition,
    heritage_of__value = col_3,
    heritage_of__value_range = col_3_range,
    heritage_of__predicate_definition = col_3_definition
  ) |>
  tidyr::pivot_longer(
    cols = matches("^(instance_of|heritage_of)__"),
    names_to = c("claim", ".value"),
    names_sep = "__"
  ) |>
  dplyr::mutate(
    predicate = dplyr::case_when(
      claim == "instance_of" ~ "instance of",
      claim == "heritage_of" ~ "heritage of"
    ),
    predicate_range = NA_character_,
    value_definition = NA_character_
  ) |>
  dplyr::select(
    row_number,
    evidence_url,
    evidence_text,
    evidence_relation,
    evidence_relation_range,
    label,
    description,
    subject,
    subject_range,
    subject_definition,
    predicate,
    predicate_range,
    predicate_definition,
    value,
    value_range,
    value_definition,
    context_1,
    context_2
  ) |>
  dplyr::arrange(row_number)


test_that("Delini pivot fixtures have the expected dimensions", {
  expect_equal(nrow(delini), 5L)
  expect_equal(nrow(delini_long), 10L)
  expect_equal(nrow(delini_dual_wide), 5L)
  expect_equal(nrow(delini_dual_long), 10L)
})


test_that("Delini wide and dual-wide fixtures are consistent", {
  expect_false("evidence_relation" %in% names(delini))
  expect_true("evidence_relation" %in% names(delini_dual_wide))
  expect_equal(delini_dual_wide$evidence_relation, rep("depicts", 5L))
  expect_equal(delini_dual_wide$col_1, delini$col_1)
  expect_equal(delini_dual_wide$col_2, delini$col_2)
  expect_equal(delini_dual_wide$col_3, delini$col_3)
  expect_equal(delini_dual_wide$context_1, delini$context_1)
})

test_that("Delini long fixtures contain atomic claims", {
  # Check the expected atomic claim structure.
  expect_setequal(
    unique(delini_long$predicate),
    c("instance of", "heritage of")
  )
  expect_equal(as.integer(table(delini_long$row_number)), rep(2L, 5L))
  expect_false("evidence_relation" %in% names(delini_long))
  expect_true("evidence_relation" %in% names(delini_dual_long))
  expect_equal(delini_dual_long$evidence_relation, rep("depicts", 10L))
})


test_that("Delini long pivot preserves candidate assertions", {
  instance_of <- delini_long |>
    dplyr::filter(predicate == "instance of")
  heritage_of <- delini_long |>
    dplyr::filter(predicate == "heritage of")

  expect_equal(instance_of$value, delini$col_2)
  expect_equal(instance_of$value_range, delini$col_2_range)
  expect_equal(instance_of$predicate_definition, delini$col_2_definition)
  expect_equal(heritage_of$value, delini$col_3)
  expect_equal(heritage_of$value_range, delini$col_3_range)
  expect_equal(heritage_of$predicate_definition, delini$col_3_definition)
})
