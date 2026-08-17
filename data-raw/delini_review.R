## code to prepare `delini_review` dataset goes here



# Delini Farmstead review example ----------------------------------------

delini_review <- tibble::tribble(
  ~evidence_id,
  ~evidence_type,
  ~thumbnail_url,
  ~subject,
  ~label_en,
  ~description_en,
  ~label_hu,
  ~description_hu,
  ~instance_of,
  ~inventory_number,
  ~held_by,

  "P7101556",
  "artefact photograph",
  "https://betwixt.dataobservatory.eu/articles/images/delini/artefacts/P7101556_thumbnail.jpg",
  "[image shown]",
  "farmhouse (LEBM)",
  "the farmhouse of the Delini farmstead",
  "lakóépület (LEBM)",
  "a Delini tanya lakóépülete",
  "farmhouse",
  "delin 13",
  "The Ethnographic Open-Air Museum of Latvia",

  "P7101565",
  "artefact photograph",
  "https://betwixt.dataobservatory.eu/articles/images/delini/artefacts/P7101565_thumbnail.jpg",
  "[image shown]",
  "sash (LEBM)",
  "a tablet-woven sash in the master bedroom of the Delini farmhouse",
  "öv (LEBM)",
  NA_character_,
  "tablet-woven sash",
  "pn 8246",
  "The Ethnographic Open-Air Museum of Latvia",

  "P7101561",
  "artefact photograph",
  "https://betwixt.dataobservatory.eu/articles/images/delini/artefacts/P7101561_thumbnail.jpg",
  "[image shown]",
  "bed (LEBM)",
  "a bed in the master bedroom of the Delini farmhouse",
  "ágy (LEBM)",
  NA_character_,
  "bed",
  NA_character_,
  "The Ethnographic Open-Air Museum of Latvia",

  "P7101590",
  "record",
  "media/delini/records/P7101590_thumbnail.jpg",
  "[image shown]",
  "Delini farmhouse photographs",
  "a record containing two photographs of the Delini farmhouse",
  "Delini lakóépület fényképei",
  NA_character_,
  "photographic record",
  NA_character_,
  "The Ethnographic Open-Air Museum of Latvia",

  "P7101623",
  "record",
  "media/delini/records/P7101623_thumbnail.jpg",
  "[image shown]",
  "Delini farmhouse floor plan",
  "a floor plan of the Delini farmhouse prepared for its reconstruction",
  "Delini lakóépület alaprajza",
  NA_character_,
  "floor plan",
  NA_character_,
  "The Ethnographic Open-Air Museum of Latvia"
)

usethis::use_data(
  delini_review,
  overwrite = TRUE
)


usethis::use_data(delini_review, overwrite = TRUE)
