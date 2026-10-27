devtools::load_all()
library(dplyr)

## muis_dual_representation_example ------------------------------------------

# Three MuIS objects and their photographic representations.

muis_dual_representation_source <- tibble::tibble(
  record_id = c("635780", "633053", "635778"),
  object_label = c(
    "sweater, women's",
    "gloves",
    "shirt, women's"
  ),
  media_id = c(
    "ebc07930-f719-44f2-a108-6698bcecc20b",
    "6440f24f-eaad-4cd4-84d7-1ae7a9d44d5a",
    "826c402e-c130-4860-b11e-9538bd403ecf"
  )
)

# Construct six distinct candidate entities.

muis_dual_representation_example <- tibble::tibble(
  row_id = as.integer(1:6),
  input_url = rep(
    paste0(
      "https://www.muis.ee/museaalview/",
      muis_dual_representation_source$record_id
    ),
    2
  ),
  input_media_url = rep(
    paste0(
      "https://www.muis.ee/digitaalhoidla/api/meedia/pisipilt?id=",
      muis_dual_representation_source$media_id
    ),
    2
  ),
  input_description = rep(
    paste(
      "MuIS museum record",
      muis_dual_representation_source$record_id
    ),
    2
  ),
  label = c(
    muis_dual_representation_source$object_label,
    paste(
      "Photograph of",
      muis_dual_representation_source$object_label
    )
  ),
  description = c(
    paste(
      "Museum object documented in MuIS record",
      muis_dual_representation_source$record_id
    ),
    paste(
      "Photographic representation associated with MuIS record",
      muis_dual_representation_source$record_id
    )
  ),
  alternative_label = c(
    "női pulóver",
    "kesztyű",
    "női ing",
    "Női pulóver fényképe",
    "Kesztyű fényképe",
    "Női ing fényképe"
  ),
  alternative_description = c(
    "A MuIS 635780 számú rekordjában dokumentált múzeumi tárgy.",
    "A MuIS 633053 számú rekordjában dokumentált múzeumi tárgy.",
    "A MuIS 635778 számú rekordjában dokumentált múzeumi tárgy.",
    "A MuIS 635780 számú rekordjához kapcsolódó fényképfelvétel.",
    "A MuIS 633053 számú rekordjához kapcsolódó fényképfelvétel.",
    "A MuIS 635778 számú rekordjához kapcsolódó fényképfelvétel."
  ),
  subject = paste0("item", 1:6),
  subject_range = paste(
    c(paste0("item", 1:6), "other"),
    collapse = " | "
  ),
  subject_definition = NA_character_,
  context_held_by = "Estonian National Museum"
) %>%
  add_candidate_column(
    name = "instance_of",
    value = c(
      rep("object", 3),
      rep("photograph", 3)
    ),
    range = paste(
      c(
        "object", "shirt", "handwear",
        "sweater", "drawing", "photograph", "other"
      ),
      collapse = " | "
    ),
    definition = NA_character_
  ) %>%
  add_candidate_column(
    name = "depicts",
    value = c(
      rep("No value possible", 3),
      "item1", "item2", "item3"
    ),
    range = paste(
      c(
        "item1", "item2", "item3",
        "No value possible", "other"
      ),
      collapse = " | "
    ),
    definition = NA_character_
  ) %>%
  add_candidate_column(
    name = "depicted_by",
    value = c(
      "item4", "item5", "item6",
      rep("No value possible", 3)
    ),
    range = paste(
      c(
        "item4", "item5", "item6",
        "No value possible", "other"
      ),
      collapse = " | "
    ),
    definition = NA_character_
  ) %>%
  add_candidate_column(
    name = "heritage_of",
    value = rep(NA_character_, 6),
    range = paste(
      c(
        "Estonians", "Livonians", "Latvians",
        "No value possible", "Value unknown", "other"
      ),
      collapse = " | "
    ),
    definition = NA_character_
  )

# Exercise 7: Dual review of objects and photographs
#
# Six entities are proposed for registration:
# three physical museum objects and three photographic representations.
#
# item1 -> depicted_by -> item4
# item4 -> depicts     -> item1
#
# item2 -> depicted_by -> item5
# item5 -> depicts     -> item2
#
# item3 -> depicted_by -> item6
# item6 -> depicts     -> item3
#
# Cultural heritage affiliation is independently reviewable.
# The holding institution is contextual information only.

validate_candidate_dataset(muis_dual_representation_example)

render_review(
  muis_dual_representation_example,
  title = "Dual Representation Example: Objects and photographic representations",
  description = paste(
    "Six entities are proposed for registration in a database:",
    "three museum objects and three photographs.",
    "Review their classifications and the relationships between them.",
    "Check both depicts and depicted_by.",
    "Determine the appropriate cultural heritage associations.",
    "The holding institution is provided as contextual information."
  ),
  reviewer_name = "",
  reviewer_iri = "",
  table_id = "muis_dual_representation_example",
  row_comment = TRUE,
  review_comment = TRUE,
  filename_stem = "muis_dual_representation_example",
  path = here::here()
)
