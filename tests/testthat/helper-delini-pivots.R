data("delini")


## Delini long ---------------------------------------------------------------

delini_long <- delini |>
  dplyr::mutate(context_2 = NA_character_) |>
  dplyr::rename(
    instance_of__value = instance_of,
    instance_of__value_range = instance_of_range,
    instance_of__predicate_definition = instance_of_definition,
    heritage_of__value = heritage_of,
    heritage_of__value_range = heritage_of_range,
    heritage_of__predicate_definition = heritage_of_definition
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
    evidence_media_url,
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


## Dual wide -----------------------------------------------------------------

delini_dual_wide <- candidate_dataset(
  evidence_media_url = delini$evidence_media_url,
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
  subject = delini$subject,
  subject_range = delini$subject_range,
  subject_definition = delini$subject_definition
) |>
  add_candidate_column(
    name = "instance_of",
    value = delini$instance_of,
    range = delini$instance_of_range,
    definition = delini$instance_of_definition
  ) |>
  add_candidate_column(
    name = "heritage_of",
    value = delini$heritage_of,
    range = delini$heritage_of_range,
    definition = delini$heritage_of_definition
  ) |>
  dplyr::mutate(context_1 = delini$context_1)


## Dual long -----------------------------------------------------------------

delini_dual_long <- delini_dual_wide |>
  dplyr::mutate(context_2 = NA_character_) |>
  dplyr::rename(
    instance_of__value = instance_of,
    instance_of__value_range = instance_of_range,
    instance_of__predicate_definition = instance_of_definition,
    heritage_of__value = heritage_of,
    heritage_of__value_range = heritage_of_range,
    heritage_of__predicate_definition = heritage_of_definition
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
    evidence_media_url,
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


## Dimensions ----------------------------------------------------------------

test_that("Delini pivot fixtures have the expected dimensions", {
  expect_equal(nrow(delini), 5L)
  expect_equal(nrow(delini_long), 10L)
  expect_equal(nrow(delini_dual_wide), 5L)
  expect_equal(nrow(delini_dual_long), 10L)
})


## Wide consistency ----------------------------------------------------------

test_that("Delini wide and dual-wide fixtures are consistent", {
  expect_false("evidence_relation" %in% names(delini))
  expect_true("evidence_relation" %in% names(delini_dual_wide))
  expect_equal(delini_dual_wide$evidence_relation, rep("depicts", 5L))
  expect_equal(delini_dual_wide$subject, delini$subject)
  expect_equal(delini_dual_wide$instance_of, delini$instance_of)
  expect_equal(delini_dual_wide$heritage_of, delini$heritage_of)
  expect_equal(delini_dual_wide$context_1, delini$context_1)
})


## Atomic claims -------------------------------------------------------------

test_that("Delini long fixtures contain atomic claims", {
  expect_setequal(
    unique(delini_long$predicate),
    c("instance of", "heritage of")
  )
  expect_equal(as.integer(table(delini_long$row_number)), rep(2L, 5L))
  expect_false("evidence_relation" %in% names(delini_long))
  expect_true("evidence_relation" %in% names(delini_dual_long))
  expect_equal(delini_dual_long$evidence_relation, rep("depicts", 10L))
})


## Candidate assertions ------------------------------------------------------

test_that("Delini long pivot preserves candidate assertions", {
  instance_of <- delini_long |> dplyr::filter(predicate == "instance of")
  heritage_of <- delini_long |> dplyr::filter(predicate == "heritage of")

  expect_equal(instance_of$value, delini$instance_of)
  expect_equal(instance_of$value_range, delini$instance_of_range)
  expect_equal(instance_of$predicate_definition, delini$instance_of_definition)
  expect_equal(heritage_of$value, delini$heritage_of)
  expect_equal(heritage_of$value_range, delini$heritage_of_range)
  expect_equal(heritage_of$predicate_definition, delini$heritage_of_definition)
})
