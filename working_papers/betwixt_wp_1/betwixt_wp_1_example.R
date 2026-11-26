library(betwixt)
library(dplyr)
library(tidyr)

data("delini")

delini |>
  select(
    row_id,
    input_description,
    subject,
    instance_of,
    heritage_of,
    context_1
  )

# 2. Elementary long projection


delini_dual_wide <- delini |>
  mutate(
    input_predicate = c(
      "depicts",
      "depicts",
      "depicts",
      "documents",
      "documents"
    ),
    input_predicate_range = add_candidate_range(
      "depicts",
      "documents",
      "<other>"
    )
  )

validate_candidate_dataset(delini_dual_wide)


delini_dual_long <- delini_dual_wide |>
  select(
    row_id,
    input_media_url,
    input_predicate,
    subject,
    instance_of,
    heritage_of
  ) |>
  pivot_longer(
    cols = c(instance_of, heritage_of),
    names_to = "predicate",
    values_to = "value"
  )

delini_dual_long

render_review(
  delini_dual_wide,
  cols = c(
    input_predicate = "Input relation",
    subject = "Subject",
    instance_of = "instance of",
    heritage_of = "heritage of",
    context_1 = "held by"
  ),
  title = "Dēliņi semantic review",
  description = "Review the proposed semantic assertions.",
  project_id = "delini",
  table_id = "delini",
  filename_stem = "delini-dual-wide",
  sequence = 0L,
  path = here::here("working_papers", "betwixt_wp_1")
)


# Elementary long projection -----------------------------------------------

delini_long <- delini |>
  select(
    row_id,
    subject,
    instance_of,
    heritage_of
  ) |>
  pivot_longer(
    cols = c(instance_of, heritage_of),
    names_to = "predicate",
    values_to = "value"
  )

delini_long


# Read the completed review ------------------------------------------------

review_path <- file.choose()

delini_review <- read_review(review_path)

delini_review$metadata
delini_review$provenance

delini_review$candidate
delini_review$reviewed
