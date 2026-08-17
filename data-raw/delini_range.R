## code to prepare `delini_range` dataset goes here


# Delini Farmstead review ranges -----------------------------------------

delini_range <- tibble::tribble(
  ~type, ~field, ~rank, ~label, ~namespace, ~url,

  # Subjects --------------------------------------------------------------

  "subject", NA_character_, 1, "[image shown]", NA_character_,
  NA_character_,
  "subject", NA_character_, 2, "[image file]", NA_character_,
  NA_character_,

  # Predicates ------------------------------------------------------------

  "predicate", NA_character_, 1, "depicts", "Wikidata",
  "https://www.wikidata.org/wiki/Property:P180",
  "predicate", NA_character_, 2, "instance of", NA_character_,
  NA_character_,
  "predicate", NA_character_, 3, "documents", NA_character_,
  NA_character_,
  "predicate", NA_character_, 4, "MIME Type", NA_character_,
  NA_character_,

  # Values for instance_of -----------------------------------------------

  "value", "instance_of", 1, "farmhouse", "AAT",
  "http://vocab.getty.edu/page/aat/300005574",
  "value", "instance_of", 2, "tablet-woven sash", NA_character_,
  NA_character_,
  "value", "instance_of", 3, "bed", NA_character_,
  NA_character_,
  "value", "instance_of", 4, "photographic record", NA_character_,
  NA_character_,
  "value", "instance_of", 5, "floor plan", NA_character_,
  NA_character_,
  "value", "instance_of", 6, "image", "DCMI",
  "http://purl.org/dc/dcmitype/Image",
  "value", "instance_of", 99, "other", NA_character_,
  NA_character_
)

usethis::use_data(delini_range, overwrite = TRUE)
