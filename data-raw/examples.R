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


delini_dataset <- readxl::read_excel("delini_import_dataset.xlsx") %>%
  mutate (row_number = as.integer(seq_along(.data$subject))) %>%
  mutate ( label = trimws(label), description = trimws(description))

names(delini_dataset)
delini_dataset$label

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






