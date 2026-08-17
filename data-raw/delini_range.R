## code to prepare `delini_range` dataset goes here

# Delini Farmstead review ranges -----------------------------------------

delini_range <- tibble::tribble(
  ~type,
  ~rank,
  ~label,
  ~namespace,
  ~url,

  # Subjects --------------------------------------------------------------

  "subject",
  1,
  "[image shown]",
  NA_character_,
  NA_character_,

  "subject",
  2,
  "[image file]",
  NA_character_,
  NA_character_,

  # Predicates ------------------------------------------------------------

  "predicate",
  1,
  "depicts",
  "Wikidata",
  "https://www.wikidata.org/wiki/Property:P180",

  "predicate",
  2,
  "instance of",
  NA_character_,
  NA_character_,

  "predicate",
  3,
  "inventory number",
  NA_character_,
  NA_character_,

  "predicate",
  4,
  "documents",
  NA_character_,
  NA_character_,

  "predicate",
  5,
  "MIME Type",
  NA_character_,
  NA_character_,

  # Values ----------------------------------------------------------------

  "value",
  1,
  "farmhouse",
  "AAT",
  "http://vocab.getty.edu/page/aat/300005574",

  "value",
  2,
  "tablet-woven sash",
  NA_character_,
  NA_character_,

  "value",
  3,
  "bed",
  NA_character_,
  NA_character_,

  "value",
  4,
  "photographic record",
  NA_character_,
  NA_character_,

  "value",
  5,
  "floor plan",
  NA_character_,
  NA_character_,

  "value",
  6,
  "image",
  "DCMI",
  "http://purl.org/dc/dcmitype/Image",

  "value",
  99,
  "other",
  NA_character_,
  NA_character_
)


usethis::use_data(delini_range, overwrite = TRUE)
