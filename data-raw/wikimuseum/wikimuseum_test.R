library(betwixt)
library(tibble)
library(dplyr)

# 1. Source tibble ------------------------------------------------

items <- tibble(
  commons_file = c(
    "File:Apron ERM B 10 18.jpg",
    "File:Woollen shawl ERM B 8 76.jpg",
    "File:Woollen shawl SU4106 384.jpg"
  ),
  label = c(
    "Traditional festive Livonian women's silk apron",
    "Traditional Livonian women's woollen festive shawl",
    "Traditional Livonian women's woollen shoulder shawl"
  ),
  caption = c(
    paste0(
      "Traditional festive Livonian women's silk apron. ",
      "Collected by Oskar Loorits on the Livonian Coast in ",
      "Pitrags (Pitrõg) in 1925. Finno-Ugric Artefact ",
      "Collection of the Estonian National Museum. ",
      "Original artefact: põll, ERM B 10:18, ",
      "Eesti Rahva Muuseum, ",
      "http://www.muis.ee/museaalview/633062 ERM B 10:18."
    ),
    paste0(
      "Traditional Livonian women's woollen festive shawl, ",
      "usually worn around the shoulders. Collected by ",
      "Oskar Loorits on the Livonian Coast in Pitrags ",
      "(Pitrõg) in 1924. Finno-Ugric Artefact Collection ",
      "of the Estonian National Museum. Original artefact: ",
      "kõrik, ERM B 8:76, Eesti Rahva Muuseum, ",
      "http://www.muis.ee/museaalview/633031 ERM B 8:76."
    ),
    paste0(
      "Traditional Livonian women's woollen shoulder shawl ",
      "from 1830-1840, collected by A. O. Heikel in ",
      "Miķeļtornis (Piza). Antell's Collection, Finno-Ugric ",
      "Collection of the National Museum of Finland. ",
      "Original artefact: Heikel, A. O. shawl; huivi; ",
      "hartiahuivi. SU4106:384."
    )
  ),
  room = c(
    "Main Garments", # Must be in the valid range
    "Headgear", "Headgear"
  ),
  sequence = c(1L, 2L, 1L)
)

# 2. Exhibition room vocabulary ----------------------------------

rooms <- tibble(
  room_label = c(
    "Introduction",
    "Main Garments",
    "Headgear",
    "Hosiery and Footwear",
    "Handwear",
    "Other Costume Accessories",
    "Bridal Attire",
    "Photographic Evidence"
  ),
  room_url = c(
    paste0(
      "https://et.wikipedia.org/wiki/",
      "Vikipeedia:GLAM/Traditional_Livonian_Clothing/Introduction"
    ),
    paste0(
      "https://et.wikipedia.org/wiki/",
      "Vikipeedia:GLAM/Traditional_Livonian_Clothing/en/",
      1:7
    )
  )
)

room_definitions <- rooms$room_url[
  match(items$room, rooms$room_label)
]


# 3. Candidate dataset --------------------------------------------

room_range <- paste(
  c(rooms$room_label, "Other…"),
  collapse = " | "
)

sequence_range <- paste(
  c(as.character(seq_len(nrow(items))), "<unknown>", "<other>"),
  collapse = " | "
)

candidates <- create_candidate_dataset(
  subject = items$label,
  input_url = paste0(
    "https://commons.wikimedia.org/wiki/",
    gsub(" ", "_", items$commons_file)
  ),
  input_media_url = paste0(
    "https://commons.wikimedia.org/wiki/Special:Redirect/file/",
    sub("^File:", "", gsub(" ", "_", items$commons_file))
  ),
  input_description = items$caption,
  label = items$label,
  description = item$caption,
  alternative_label = items$label,
  alternative_description = items$caption,
  table_id = "wikimuseum-exhibition",
  project_id = "wikimuseum"
) |>
  add_candidate_column(
    name = "room",
    value = items$room,
    range = room_range,
    definition = room_definitions
  ) |>
  add_candidate_column(
    name = "sequence",
    value = as.character(items$sequence),
    range = rep(sequence_range, nrow(items))
  ) |>
  add_candidate_column(
    name = "inventory_number",
    value = c("ERM B 10:18", "ERM B 8:76", "SU4106:384")
  )

# 4. Validate -----------------------------------------------------

validate_candidate_dataset(candidates)


message(unique(candidates$room_range))

# 5. Render -------------------------------------------------------

render_review(
  candidates,
  title = "Wikimuseum exhibition review",
  description = paste(
    "Review the proposed exhibition title and description.",
    "Select the appropriate virtual room and display sequence.",
    "Choose OTHER [CURATOR ENTERED] if the appropriate room is not listed, or if you want ",
    "to increase the number of items in a room (for example, if you can select ",
    "only 1, 2, 3, Value unknown, with you can add 4 with OTHER. ",
    "Choose VALUE UNKNOWN for the sequence if the item should be omitted ",
    "from the exhibition."
  ),
  reviewer_name = "",
  reviewer_iri = "",
  table_id = "wikimuseum-exhibition",
  row_comment = TRUE,
  review_comment = TRUE,
  filename_stem = "wikimuseum-exhibition",
  path = here::here("data-raw", "wikimuseum")
)
