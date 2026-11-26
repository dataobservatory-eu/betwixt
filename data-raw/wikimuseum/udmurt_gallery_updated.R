library(tibble)
library(dplyr)
library(httr2)
devtools::load_all()

# Wikimedia Commons reader ---------------------------------------------------

# Perform one request only. Retry timing is managed by the harvesting loop,
# so failures can be checkpointed without terminating the entire script.
read_commons_wikitext <- function(commons_file,
                                  section = 1,
                                  sleep = 1) {
  api <- "https://commons.wikimedia.org/w/api.php"

  Sys.sleep(sleep)

  response <- request(api) |>
    req_url_query(
      action = "parse",
      page = commons_file,
      prop = "wikitext",
      section = section,
      format = "json"
    ) |>
    req_perform() |>
    resp_body_json(simplifyVector = TRUE)

  response$parse$wikitext[["*"]]
}

# Wikitext extraction helpers ------------------------------------------------

find_value <- function(wikitext, regex, url = FALSE) {
  value_text <- stringr::str_extract(
    wikitext,
    stringr::regex(regex, ignore_case = TRUE)
  )

  if (url) {
    value_text <- stringr::str_extract(
      value_text,
      "https?://[^\\s|}\\];]+"
      # "https?://[^\\s|}\\]]+"
    )
  }

  value_text
}

find_language_value <- function(wikitext, field, lang) {
  field_regex <- paste0(
    "(?s)\\|\\s*", field,
    "\\s*=(.*?)(?=\\n\\s*\\|\\s*[A-Za-z_ ]+\\s*=|\\n\\}\\})"
  )

  field_text <- stringr::str_match(
    wikitext,
    field_regex
  )[, 2]

  if (is.na(field_text)) {
    return(NA_character_)
  }

  lang_regex <- paste0(
    "\\{\\{", lang, "\\|1=(.*?)\\}\\}"
  )

  stringr::str_match(
    field_text,
    lang_regex
  )[, 2]
}

# Wikimedia Commons file inventory ------------------------------------------

commons_files <- c(
  "054013 ERM B74 1 054013.jpg",
  "060390 ERM B159 6 060390.jpg",
  "060395 ERM B114 90 060395.jpg",
  "060408 ERM B159 107 060408.jpg",
  "082594 ERM B166 13-ab 082594.jpg",
  "082595 ERM B152 55-ab 082595.jpg",
  "082623 ERM B176 40-ab 082623.jpg",
  "082625 ERM B128 48-ab 082625.jpg",
  "082628 ERM B49 2-ab 082628.jpg",
  "126799 ERM B139 50 126799.jpg",
  "126826 ERM B102 126 126826.jpg",
  "126832 ERM B106 20 126832.jpg",
  "194677 ERM B114 51 194677.jpg",
  "194887 ERM B106 68 194887.jpg",
  "204973 ERM B106 11 204973.jpg",
  "204987 ERM B114 46 204987.jpg",
  "205025 ERM B123 10 205025.jpg",
  "208041 ERM B114 58 208041.jpg",
  "208110 ERM B146 82 208110.jpg",
  "216788 ERM B114 20 216788.jpg",
  "216789 ERM B159 5 216789.jpg",
  "216792 ERM B123 72 216792.jpg",
  "216931 ERM B128 66 216931.jpg",
  "216941 ERM B70 4 216941.jpg",
  "227803 ERM B70 11 227803.jpg",
  "227817 ERM B114 72 227817.jpg",
  "227872 ERM B102 118 227872.jpg",
  "227876 ERM B123 20 227876.jpg",
  "227877 ERM B123 21 227877.jpg",
  "227878 ERM B123 22 227878.jpg",
  "227881 ERM B152 36 227881.jpg",
  "227882 ERM B152 74 227882.jpg",
  "228041 ERM B102 1 228041.jpg",
  "228047 ERM B102 95 228047.jpg",
  "228048 ERM B102 117 228048.jpg",
  "235660 ERM B102 67 235660.jpg",
  "274608 ERM B77 24 274608.jpg",
  "274610 ERM B77 27 274610.jpg",
  "274628 ERM B114 54 274628.jpg",
  "274668 ERM B70 14 274668.jpg",
  "274670 ERM B146 40 274670.jpg",
  "274718 ERM B123 53 274718.jpg",
  "274806 ERM B77 22 274806.jpg",
  "274826 ERM B106 9 274826.jpg",
  "274862 ERM B152 56 274862.jpg",
  "310411 ERM B123 27 310411.jpg",
  "310435 ERM B128 4 310435.jpg",
  "310436 ERM B123 14 310436.jpg",
  "310441 ERM B77 2 310441.jpg",
  "310442 ERM B77 1 310442.jpg",
  "310701 ERM B106 133 310701.jpg",
  "310705 ERM B77 21 310705.jpg",
  "310717 ERM B123 32 310717.jpg",
  "Apron - SU2191-12.jpg",
  "Apron - SU4817-54.jpg",
  "Apron - SU4817-57.jpg",
  "Bast shoes - ERM B 102-79-ab.jpg",
  "Bast shoes - ERM B 176-136-ab.jpg",
  "Coat - SU4817-52.jpg",
  "Coat - SU4817-53.jpg",
  "Coat - SU4817-68.jpg",
  "Coat - SU4817-69.jpg",
  "Dress - SU2191-4.jpg",
  "Dress - SU4817-47.jpg",
  "Dress - SU6191-2.jpg",
  "Overcoat - SU2191-3.jpg",
  "Shirt - SU 2191-1.jpg",
  "Shirt - SU2191-5.jpg",
  "Shirt - SU5994-62.jpg",
  "Trousers - SU4817-44.jpg"
)

# Candidate dataset template -------------------------------------------------

udmurt_items <- create_candidate_template(
  n = length(commons_files),
  input_url = TRUE,
  input_media_url = TRUE,
  input_label = TRUE,
  input_description = TRUE,
  label = TRUE,
  description = TRUE,
  alternative_label = TRUE,
  alternative_description = TRUE,
  subject_range = FALSE,
  subject_definition = TRUE,
  columns = c(
    "exhibition_room",
    "accession_number",
    "institution",
    "permission"
  ),
  context = c(
    "commons_file",
    "context_label_en",
    "context_label_hu",
    "context_label_lv",
    "context_label_ru",
    "context_label_udm",
    "context_description_en",
    "context_description_hu",
    "context_description_lv",
    "context_description_ru",
    "context_description_udm",
    "muis_url",
    "finna_url",
    "textilebase_url",
    "fuds_url",
    "wikitext"
  )
) |>
  mutate(
    commons_file = paste0("File:", commons_files),
    input_url = paste0(
      "https://commons.wikimedia.org/wiki/",
      gsub(" ", "_", commons_file)
    ),
    input_media_url = paste0(
      "https://commons.wikimedia.org/wiki/Special:Redirect/file/",
      gsub(" ", "_", commons_files)
    )
  )

names(udmurt_items)

# Extraction patterns --------------------------------------------------------

textilebase_regex <-
  "https?://reprexbase\\.eu/textilebase/Item:Q[0-9]+"

finnougric_regex <-
  "finno-?ugric[^:]*:\\s*https?://[^\\s|}\\];]+"

finna_regex <-
  "https?://(?:www\\.)?finna\\.fi/Record/[^\\s|}\\];]+"

muis_regex <-
  "https?://(?:www\\.)?muis\\.ee/museaalview/[0-9]+"

inventory_regex <- "(?<=\\()[^()]+(?=\\)$)"

institution_regex <-
  "(?m)^\\s*\\|institution\\s*=\\s*\\{\\{Institution:[^}]+\\}\\}"

permission_regex <-
  "(?m)(?<=\\|permission = ).+(?=\\r?$)"

women_pattern <- "\\b(girl|girls|girls'|woman|woman's|women|women's)\\b"
men_pattern <- "\\b(man|man's|men|boy|boy's|boys)\\b"


# Harvest configuration and persistent checkpoint ----------------------------

languages <- c("en", "hu", "lv", "ru", "udm")
sleep <- 3
max_tries <- 3

# Keep the existing output path and file name unchanged.
checkpoint_file <- here::here(
  "data-raw", "wikimuseum", "udmurt_candidate_items.csv"
)

if (file.exists(checkpoint_file)) {
  saved <- read.csv(
    checkpoint_file,
    stringsAsFactors = FALSE,
    na.strings = c("", "NA"),
    check.names = FALSE
  )

  # Restore by Commons filename rather than row position. This also allows
  # the file inventory to be reordered or extended between harvesting runs.
  idx <- match(udmurt_items$commons_file, saved$commons_file)
  matched <- which(!is.na(idx))

  for (col in intersect(names(udmurt_items), names(saved))) {
    if (col == "commons_file") next
    udmurt_items[[col]][matched] <- saved[[col]][idx[matched]]
  }

  message("Resuming from existing checkpoint: ", checkpoint_file)
} else {
  # Create the checkpoint before the first network request.
  write.csv(udmurt_items, checkpoint_file, row.names = FALSE, na = "")
  message("Created checkpoint: ", checkpoint_file)
}

# Harvest Commons metadata ---------------------------------------------------

for (i in seq_along(commons_files)) {
  # A record is complete when both English fields have content. Prefer the
  # harvested context fields, since label/description are populated later.
  has_label <- !is.na(udmurt_items$context_label_en[i]) &&
    nzchar(trimws(udmurt_items$context_label_en[i]))
  has_description <- !is.na(udmurt_items$context_description_en[i]) &&
    nzchar(trimws(udmurt_items$context_description_en[i]))

  if (has_label && has_description) {
    message(i, ": Already saved. Skipping.")
    next
  }

  wikitext <- NULL

  # Three attempts, spaced by 2, 4 and 8 seconds by default. A failed
  # request is not written as a completed record, so it can be resumed.
  for (attempt in seq_len(max_tries)) {
    result <- tryCatch(
      read_commons_wikitext(
        commons_file = udmurt_items$commons_file[i],
        sleep = sleep * 2^(attempt - 1)
      ),
      error = function(e) e
    )

    if (!inherits(result, "error")) {
      wikitext <- result
      break
    }

    warning(
      "Attempt ", attempt, "/", max_tries,
      " failed for ", udmurt_items$commons_file[i],
      ": ", conditionMessage(result)
    )
  }

  # On repeated HTTP 429 or another persistent error, stop this run without
  # losing earlier records. Re-running will resume from the checkpoint.
  if (is.null(wikitext)) {
    message("Stopping at row ", i, ". Checkpoint preserved.")
    break
  }

  if (length(wikitext) != 1L || is.na(wikitext) ||
    !nzchar(wikitext)) {
    warning(udmurt_items$commons_file[i], " is not read. Skipping...")
    next
  }

  udmurt_items$wikitext[i] <- wikitext

  # External identifiers
  udmurt_items$textilebase_url[i] <- find_value(
    wikitext, textilebase_regex,
    url = TRUE
  )

  udmurt_items$fuds_url[i] <- find_value(
    wikitext, finnougric_regex,
    url = TRUE
  )

  udmurt_items$finna_url[i] <- find_value(
    wikitext, finna_regex,
    url = TRUE
  )

  udmurt_items$muis_url[i] <- find_value(
    wikitext, muis_regex,
    url = TRUE
  )

  # Multilingual titles and descriptions
  for (lang in languages) {
    udmurt_items[[paste0("context_label_", lang)]][i] <-
      find_language_value(wikitext, "title", lang)

    udmurt_items[[paste0("context_description_", lang)]][i] <-
      find_language_value(wikitext, "description", lang)
  }

  udmurt_items$accession_number[i] <- find_value(
    udmurt_items$context_label_en[i], inventory_regex
  )

  udmurt_items$institution[i] <- find_value(
    wikitext, institution_regex
  ) |>
    sub(".*\\{\\{Institution:([^}]+)\\}\\}", "\\1", x = _)

  udmurt_items$permission[i] <- find_value(
    wikitext, permission_regex
  ) |>
    sub(".*\\{\\{([^}]+)\\}\\}", "\\1", x = _)


  # Store the review-facing fields as well as the context values. They are
  # used to identify completed rows when this checkpoint is loaded again.
  udmurt_items$label[i] <- udmurt_items$context_label_en[i]
  udmurt_items$description[i] <- udmurt_items$context_description_en[i]

  # Persist each completed record immediately; a later 429 cannot erase it.
  write.csv(udmurt_items, checkpoint_file, row.names = FALSE, na = "")

  message(
    i, ": ", udmurt_items$commons_file[i],
    " - ", udmurt_items$institution[i],
    " (", udmurt_items$accession_number[i], ") [saved]"
  )
}

# Candidate enrichment and clean-up ------------------------------------------

udmurt_items$label <- udmurt_items$context_label_en
udmurt_items$description <- udmurt_items$context_description_en
udmurt_items$subject_definition <- udmurt_items$fuds_url


udmurt_items$subject <- paste0(
  "fuds::",
  sub("^https?://reprexbase\\.eu/fu/", "", udmurt_items$fuds_url)
)

udmurt_items$accession_number_definition <- case_when(
  !is.na(udmurt_items$muis_url) ~ udmurt_items$muis_url,
  !is.na(udmurt_items$finna_url) ~ udmurt_items$finna_url,
)

udmurt_items$accession_number_range <- paste(
  c(
    "<no-value>",
    "<other>",
    unique(na.omit(udmurt_items$accession_number))
  ),
  collapse = " | "
)

udmurt_items$permission <- trimws(udmurt_items$permission)

udmurt_items$permission_range <- paste(
  c(
    "<no-value>",
    "<other>",
    unique(na.omit(udmurt_items$permission))
  ),
  collapse = " | "
)

udmurt_items$permission_definition <- NULL


# Exhibition room organisation ----------------------------------------------
udmurt_items <- udmurt_items |>
  dplyr::mutate(
    exhibition_room = dplyr::case_when(
      is.na(description) | !nzchar(trimws(description)) ~
        "No Exhibition Room",
      stringr::str_detect(
        description, stringr::regex(women_pattern, ignore_case = TRUE)
      ) ~
        "Garments for Women",
      stringr::str_detect(description, stringr::regex(
        men_pattern,
        ignore_case = TRUE
      )) ~
        "Garments for Men",
      .default = "No Exhibition Room"
    )
  )

udmurt_items$exhibition_room_range <- paste(
  c(
    "Garments for Women",
    "Garments for Men",
    "No Exhibition Room",
    "<other>"
  ),
  collapse = " | "
)

udmurt_items$institution_range <- paste(
  c(
    "<other>",
    unique(stats::na.omit(udmurt_items$institution))
  ),
  collapse = " | "
)

udmurt_items$alternative_description <- "[Optional changes for exhibition]"

# Save the enriched dataset to the same checkpoint, retaining all changes.
write.csv(
  x = udmurt_items,
  file = here::here(
    "data-raw", "wikimuseum",
    "udmurt_candidate_items.csv"
  ),
  row.names = F
)

# Validate candidate dataset ------------------------------------------------


candidates <- udmurt_items

# Render review -------------------------------------------------------------

render_review(
  candidates,
  title = "Udmurt Ethnographic Clothing",
  description = paste(
    "Review the proposed descriptions and metadata of",
    "Udmurt ethnographic clothing objects documented in",
    "Wikimedia Commons and the Estonian National Museum.",
    "Check classifications, accession numbers, institutions,",
    "places of creation, and permission statements."
  ),
  reviewer_name = "Ieva Pigozne",
  reviewer_iri = "https://orcid.org/0000-0003-1726-0928",
  table_id = "udmurt_clothing_dataset",
  project_id = "finfair",
  row_comment = TRUE,
  review_comment = TRUE,
  filename_stem = "udmurt_clothing_dataset",
  path = here::here("data-raw", "wikimuseum")
)


example_candidates <- udmurt_items[c(4, 6, 7, 10, 15, 58, 65, 67), ]


render_review(
  example_candidates,
  title = "Udmurt Ethnographic Clothing",
  description = paste(
    "Review the proposed descriptions and metadata of",
    "Udmurt ethnographic clothing objects.",
    "Classification: select the exhibition room.",
    "Description: if necessary, create a more suitable description for ",
    "the exhibition's target audience."
  ),
  reviewer_name = "Ieva Pigozne",
  reviewer_iri = "https://orcid.org/0000-0003-1726-0928",
  table_id = "udmurt_clothing_example-1",
  table_iri = "https://doi.org/10.5281/zenodo.24448050",
  project_id = "finfair",
  row_comment = TRUE,
  review_comment = TRUE,
  filename_stem = "udmurt_exhibition_example_clothing",
  path = here::here("data-raw", "wikimuseum")
)

devtools::load_all()



tibble::tribble(
  ~value, ~type, ~range, ~definition,
  "Q124", NA_character_, list(NULL), "https://example.com/Item:/Q124",
  "my string", "en", list(NULL), NA_character_,
  "123", "integer", list(NULL), NA_character_,
  "124", "integer", list(121:129), NA_character_,
  "2026-10-29T13:21:11", "dateTime", list(NULL), NA_character_,
  "szöveg", "lang_hu", list(NULL), NA_character_
)
