devtools::load_all()
data(delini)
library(tidyverse)

## For import
candidate <- create_candidate_template(
  evidence_media_url = TRUE,
  evidence_url = TRUE,
  evidence_text = TRUE,
  evidence_relation = TRUE,
  label = TRUE,
  description = TRUE,
  subject_range = TRUE,
  subject_definition = TRUE,
  columns = c(
    "instance_of",
    "represents",
    "represented_instance_of",
    "part_of",
    "part_of_range",
    "collection",
    "creator",
    "use_rights"
  ),
  context = "held_by",
)

writexl::write_xlsx(candidate, "delini_farmstead_import.xlsx")


# For programmatic

candidate <- create_candidate_dataset(
  label = character(),
  subject = character()
)

candidate <- add_candidate_column(
  candidate,
  name = "represents",
  value = character()
)

candidate <- add_candidate_column(
  candidate,
  name = "represented_instance_of",
  value = character()
)

candidate <- add_candidate_column(
  candidate,
  name = "instance_of",
  value = character()
)

candidate$context_collection <- character()

# review input

review_input <- tibble::tribble(
  ~evidence_media_url, ~evidence_url, ~evidence_text, ~evidence_relation,
  ~evidence_relation_range, ~subject, ~subject_definition, ~subject_range,
  ~label, ~description,
  ~represents, ~represented_instance_of, ~instance_of, ~context_collection,
  "https://example.com/image.jpg", "https://example.com/record/1",
  "Photograph used as evidence.", "depicts", "example:photo1",
  "https://example.com/entity/photo1", "photograph", "Example photograph",
  "A photograph depicting a farmhouse.", "example:farmhouse1", "farmhouse",
  "photograph", "Example collection"
)

controlled_vocab_classes <-
  paste(
    "thing | person | building | farmhouse | farmstead | garment | sash |",
    "tablet-woven sash | recognisable person | living person | deceased person"
  )

controlled_vocab_relations <- "depicts | depicted by | documents | documented by"

review_input <- tibble::tribble(
  ~evidence_media_url, ~evidence_url, ~evidence_text,
  ~evidence_relation, ~evidence_relation_range,
  ~label, ~description,
  ~subject, ~subject_definition, ~subject_range,
  ~represents, ~represents_range,
  ~represented_instance_of, ~represented_instance_of_range,
  ~instance_of, ~instance_of_range,
  ~context_collection,

  # The photograph is the subject and depicts the farmstead.
  photo_url,
  "https://reprexbase.eu/fu/Item:Q7349",
  "Photograph used as evidence.",
  "depicts",
  controlled_vocab_relations,
  "Dēliņi farmstead - detail with farmhouse and granary-cart shed (photo)",
  "Photograph showing the Dēliņi farmstead.",
  "fudss:Q7349",
  "https://reprexbase.eu/fu/Item:Q7349",
  controlled_vocab_classes,
  "Dēliņi farmstead",
  controlled_vocab_classes,
  "farmstead",
  controlled_vocab_classes,
  "photograph",
  controlled_vocab_classes,
  "Ethnographic Open-Air Museum of Latvia",

  # The farmstead is the subject and is depicted by the photograph.
  photo_url,
  "https://reprexbase.eu/fu/Item:Q7349",
  "Photograph used as evidence.",
  "depicted by",
  controlled_vocab_relations,
  "Dēliņi farmstead",
  "Farmstead represented in the photograph.",
  "Dēliņi farmstead",
  NA_character_,
  controlled_vocab_classes,
  "fudss:Q7349",
  controlled_vocab_classes,
  "photograph",
  controlled_vocab_classes,
  "farmstead",
  controlled_vocab_classes,
  "Ethnographic Open-Air Museum of Latvia"
)

represents_values <- c(
  NA,
  NA,
  NA,
  NA,
  NA,
  NA,
  "Dēliņi farmstead: granary  (BDM 8071)",
  NA,
  NA,
  "Dēliņi farmhouse: architectural plan (BMD CZM 1109-3)",
  NA,
  "Dēliņi farmstead",
  "Dēliņi farmhouse",
  "Dēliņi farmhouse",
  NA,
  "sash (BDM zpn 8246)",
  "sash (BDM zpn 8246)",
  "Dēliņi combined cowshed-barn (BDM 8069)",
  "Dēliņi combined cowshed-barn (BDM 8069)",
  NA
)

represents_range_values <- c(
  NA,
  NA,
  NA,
  NA,
  NA,
  NA,
  "Dēliņi farmstead: granary  (BDM 8071)",
  NA,
  NA,
  "Dēliņi farmhouse: architectural plan (BMD CZM 1109-3)",
  NA,
  "Dēliņi farmstead | Dēliņi farmhouse | Dēliņi farmstead: granary  (BDM 8071)",
  "Dēliņi farmstead | Dēliņi farmhouse",
  "Dēliņi farmstead | Dēliņi farmhouse",
  NA,
  "sash (BDM zpn 8246)",
  "sash (BDM zpn 8246)",
  "Dēliņi combined cowshed-barn (BDM 8069)",
  "Dēliņi combined cowshed-barn (BDM 8069)",
  NA
)

represents_definition_values <- c(
  NA,
  NA,
  NA,
  NA,
  NA,
  NA,
  "https://reprexbase.eu/fu/Item:Q7351",
  NA,
  NA,
  "https://reprexbase.eu/fu/Item:Q7353",
  NA,
  "https://reprexbase.eu/fu/Item:Q5582",
  "https://reprexbase.eu/fu/Item:Q7328",
  "https://reprexbase.eu/fu/Item:Q7328",
  NA,
  "https://reprexbase.eu/fu/Item:Q7355",
  "https://reprexbase.eu/fu/Item:Q7355",
  "https://reprexbase.eu/fu/Item:Q5576",
  "https://reprexbase.eu/fu/Item:Q5576",
  NA
)

represented_by_values <- c(
  "Dēliņi farmstead - detail with farmhouse and granary-cart shed (photo) ",
  NA,
  NA,
  NA,
  NA,
  "Dēliņi farmstead: farmhouse, frontal view with well (photo)",
  NA,
  "Dēliņi farmstead: granary, oblique view (photograph)",
  "Dēliņi farmhouse: architectural plan (photographic reproduction)",
  NA,
  "sash (BDM zpn 8246, detail)",
  NA,
  NA,
  NA,
  NA,
  NA,
  NA,
  NA,
  NA,
  "Dēliņi combined barn-shed: oblique front (photograph) "
)

represented_by_range_values <- c(
  paste(
    c(
      "Dēliņi farmstead - detail with farmhouse and granary-cart shed (photo) "
    ),
    collapse = " | "
  ),
  NA,
  NA,
  NA,
  NA,
  paste(
    c(
      "Dēliņi farmstead: farmhouse, frontal view with well (photo)",
      "Dēliņi farmstead: farmhouse (oblique view, photo) "
    ),
    collapse = " | "
  ),
  NA,
  "Dēliņi farmstead: granary, oblique view (photograph)",
  "Dēliņi farmhouse: architectural plan (photographic reproduction)",
  NA,
  paste(
    c(
      "sash (BDM zpn 8246, detail)",
      "sash (BDM zpn 8246; photograph)"
    ),
    collapse = " | "
  ),
  NA,
  NA,
  NA,
  NA,
  NA,
  NA,
  NA,
  NA,
  paste(
    c(
      "Dēliņi combined barn-shed: oblique front (photograph) ",
      "Dēliņi combined barn-shed: rear view (photograph)"
    ),
    collapse = " | "
  )
)

represented_by_definition_values <- c(
  "https://reprexbase.eu/fu/Item:Q7349",
  NA,
  NA,
  NA,
  NA,
  "https://reprexbase.eu/fu/Item:Q7362",
  NA,
  "https://reprexbase.eu/fu/Item:Q7350",
  "https://reprexbase.eu/fu/Item:Q7354",
  NA,
  "https://reprexbase.eu/fu/Item:Q7366",
  NA,
  NA,
  NA,
  NA,
  NA,
  NA,
  NA,
  NA,
  "https://reprexbase.eu/fu/Item:Q5578"
)

delini_dataset_filled <-
  readxl::read_excel("delini_farmstead_import_filled.xlsx")

collections <-
  delini_dataset_filled %>%
  dplyr::filter(instance_of == "collection") %>%
  select(label, starts_with("subject"))

collection_labels <- c(unique(collections$label), "Other…")
instance_labels <- c(
  unique(delini_dataset_filled$instance_of),
  "shirt", "garment", "textile",  "Other…")

delini_dataset_render <-
  delini_dataset_filled %>%
  mutate(
    row_number = as.integer(1:nrow(delini_dataset_filled))
  ) %>%
  mutate(
    subject = dplyr::case_when(
      instance_of == "collection" ~ label,
      .default = subject
    )
  ) %>%
  mutate(
    subject_range = paste(
      unique(subject),
      collapse = " | "
    )
  ) %>%
  mutate(
    instance_of_range = paste(
      instance_labels,
      collapse = " | "
    )
  ) %>%
  mutate(
    curated_member_of = dplyr::case_when(
      curated_member_of == collections$subject[1] ~ collections$label[1],
      curated_member_of == collections$subject[2] ~ collections$label[2],
      curated_member_of == collections$subject[3] ~ collections$label[3],
      curated_member_of == collections$subject[4] ~ collections$label[4],
      .default = "Not applicable"
    )
  ) %>%
  mutate(
    curated_member_of_range = paste(
      collection_labels,
      collapse = " | "
    )
  ) %>%
  mutate(
    evidence_text = "If you are unsure, you can visit the link"
  ) %>%
  mutate(
    creator = ifelse(
      instance_of == "photograph",
      "Daniel Antal",
      ""
    )
  ) %>%
  mutate(
    represents = .env$represents_values,
    represents_range = .env$represents_range_values,
    represents_definition =
      .env$represents_definition_values
  ) %>%
  mutate(
    represented_by = .env$represented_by_values,
    represented_by_range = .env$represented_by_range_values,
    represented_by_definition =
      .env$represented_by_definition_values
  )  %>%
  relocate(
    represented_by,
    .after = represents_definition
  ) %>%
  relocate(
    represented_by_range,
    .after = represented_by
  ) %>%
  relocate(
    represented_by_definition,
    .after = represented_by_range
  ) %>%
  select(-starts_with("represented_instance_of"))

library(dplyr)
render_review(
  delini_dataset_render,
  title = "Delini semantic review",
  description = "Review the proposed semantic assertions.",
  reviewer_name = "Daniel Antal",
  reviewer_iri = "https://orcid.org/0000-0002-1825-0097",
  project_id = "delini-filled",
  row_comment = TRUE,
  review_comment = TRUE,
  filename_stem = "delini-test-review",
  path = here::here()
)



x2$curated_member_of_range

x2 %>% select(label, starts_with("instance_of"), starts_with("curated")) %>%
  pivot_longer(-any_of('label'))

browseURL(
  here::here("delini-two-row-test.html")
)


test <- delini_dataset_filled %>%
  mutate(
    curated_member_of = dplyr::case_when(
      curated_member_of == collections$subject[1] ~ collections$label[1],
      curated_member_of == collections$subject[2] ~ collections$label[2],
      curated_member_of == collections$subject[3] ~ collections$label[3],
      curated_member_of == collections$subject[4] ~ collections$label[4],
      .default = curated_member_of
    )
  ) %>%
  mutate(
    curated_member_of_range = paste(
      collections$label,
      collapse = " | "
    )
  )

test

test %>%
  select(
    curated_member_of,
    curated_member_of_range,
    curated_member_of_definition
  ) %>%
  distinct() %>%
  print(width = Inf)

delini_dataset_render %>% select (
  starts_with ("instance_of"), starts_with ("curated_member")
)

context <- prepare_review_context(test)

lapply(
  context$rows,
  function(row) {
    row$assertions[
      vapply(
        row$assertions,
        function(x) identical(x$name, "curated_member_of"),
        logical(1)
      )
    ]
  }
)


review <- read_review("delini-review_1-finalised.html")
wide <- project_review_wide(review)


library(dplyr)
sample_n(wide, 10)
wide %>% select(-evidence_media_url)


long <- project_review_long(review)
long

ttl <- serialise_review(
  review,
  prefix = "https://example.org/reviews/",
  filename = "delini-review_1-finalised.ttl"
)
cat(ttl)

writeLines(ttl, con = "delini-review_1-finalised.ttl" )


x <- create_candidate_dataset(
  evidence_media_url = "https://placehold.co/300x200",
  evidence_text = "Minimal rendering test",
  label = "Example object",
  description = "Testing range plus definition",
  subject = "[example object]"
) |>
  add_candidate_column(
    name = "instance_of",
    value = "Farmhouse",
    range = add_candidate_range(
      "Farmhouse",
      "Dwelling",
      "Building"
    ),
    definition = "https://example.org/farmhouse"
  )

render_review(
  x,
  file = "minimal-definition-range",
  path = here::here()
)

browseURL("minimal-definition-range.html")
