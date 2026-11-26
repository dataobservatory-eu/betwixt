library(dplyr)
library(stringr)
library(tibble)
library(readr)
library(httr2)


devtools::load_all()

output_path <- here::here("data-raw", "wikimuseum")

room_definitions <- tibble::tibble(
  room_number = 1:7,
  room_title = rep(NA_character_, 7),
  room_definition = rep(NA_character_, 7)
)

# External identifier patterns -------------------------------------------

muis_regex <- "https?://(?:www\\.)?muis\\.ee/museaalview/[0-9]+"
finna_regex <- "https?://(?:www\\.)?finna\\.fi/Record/[^\\s|}\\];]+"
textilebase_regex <- "https?://reprexbase\\.eu/textilebase/Item:Q[0-9]+"
fuds_regex <- "https?://reprexbase\\.eu/fu/[^\\s|}\\];]+"

# Process all eight rooms -------------------------------------------------

for (room in 1:7) {

  message("Processing exhibition room ", room)

  file_stem <- paste0("livonian_exhibition_", room)

  # Read saved MediaWiki source ------------------------------------------

  source_file <- file.path(
    output_path,
    paste0(file_stem, ".txt")
  )

  wikitext <- paste(
    readLines(source_file, encoding = "UTF-8", warn = FALSE),
    collapse = "\n"
  )

  # Extract gallery entries ----------------------------------------------

  gallery_blocks <- str_extract_all(
    wikitext,
    regex("<gallery\\b[^>]*>.*?</gallery>", dotall = TRUE)
  )[[1]]

  livonian_items <- bind_rows(
    lapply(seq_along(gallery_blocks), function(i) {

      entries <- str_split(gallery_blocks[i], "\n")[[1]]
      entries <- str_trim(entries)
      entries <- entries[str_detect(entries, "^File:")]

      tibble(
        room_number = room,
        page_section = i,
        wikitext = entries
      )
    })
  )

  # Extract metadata -----------------------------------------------------

  livonian_items <- livonian_items |>
    mutate(
      commons_file = str_extract(wikitext, "^File:[^|]+"),

      description = str_remove(wikitext, "^File:[^|]+\\|"),

      label = str_extract(description, "^[^.]+\\."),
      label = str_remove(label, "\\.$"),

      input_url = paste0(
        "https://commons.wikimedia.org/wiki/",
        str_replace_all(commons_file, " ", "_")
      ),

      input_media_url = paste0(
        "https://commons.wikimedia.org/wiki/Special:Redirect/file/",
        str_replace_all(
          str_remove(commons_file, "^File:"),
          " ", "_"
        )
      ),

      muis_url = str_extract(description, muis_regex),
      finna_url = str_extract(description, finna_regex),
      textilebase_url = str_extract(description, textilebase_regex),
      fuds_url = str_extract(description, fuds_regex),

      accession_number = str_extract(
        description,
        "\\b(?:ERM\\s+B\\s*\\d+|SUK?\\s*\\d+)\\s*:\\s*\\d+\\b"
      ),

      accession_number = str_squish(accession_number),

      accession_number_definition = coalesce(
        muis_url,
        finna_url
      ),

      institution = case_when(
        str_detect(description, fixed("Estonian National Museum")) ~
          "Estonian National Museum",
        str_detect(description, fixed("National Museum of Finland")) ~
          "National Museum of Finland",
        TRUE ~ NA_character_
      )
    ) |>
    select(
      room_number,
      page_section,
      commons_file,
      input_url,
      input_media_url,
      label,
      description,
      accession_number,
      accession_number_definition,
      institution,
      muis_url,
      finna_url,
      textilebase_url,
      fuds_url,
      wikitext
    )

  # Save extracted CSV ---------------------------------------------------

  write_csv(
    livonian_items,
    file.path(output_path, paste0(file_stem, ".csv"))
  )

  # Create Betwixt candidate table ---------------------------------------

  livonian_candidates <- create_candidate_template(
    n = nrow(livonian_items),
    input_url = TRUE,
    input_media_url = TRUE,
    input_label = TRUE,
    input_description = TRUE,
    label = TRUE,
    description = TRUE,
    subject_definition = TRUE,
    columns = c(
      "accession_number",
      "institution",
      "room_number"
    ),
    context = c(
      "page_section",
      "commons_file",
      "muis_url",
      "finna_url",
      "textilebase_url",
      "fuds_url",
      "wikitext"
    )
  )

  # Populate candidate table ---------------------------------------------

  livonian_candidates$subject <- livonian_items$input_url
  livonian_candidates$subject_definition <- livonian_items$input_url

  livonian_candidates$input_url <- livonian_items$input_url
  livonian_candidates$input_media_url <- livonian_items$input_media_url

  livonian_candidates$input_label <- livonian_items$label
  livonian_candidates$input_description <- livonian_items$description

  livonian_candidates$label <- livonian_items$label
  livonian_candidates$description <- livonian_items$description

  livonian_candidates$alternative_description <- "[Add Latvian translation]"

  livonian_candidates$accession_number <- livonian_items$accession_number
  livonian_candidates$accession_number_definition <-
    livonian_items$accession_number_definition

  livonian_candidates$institution <- livonian_items$institution

  livonian_candidates$room_number <-
    as.character(livonian_items$room_number)

  livonian_candidates$room_number_definition <- paste0(
    "https://et.wikipedia.org/w/index.php?title=",
    "Vikipeedia:GLAM/Traditional_Livonian_Clothing/en/",
    livonian_items$room_number
  )

  livonian_candidates$room_number_range <-
    paste(1:7, collapse = " | ")

  # Context fields -------------------------------------------------------

  livonian_candidates$page_section <-
    as.character(livonian_items$page_section)

  livonian_candidates$commons_file <- livonian_items$commons_file
  livonian_candidates$muis_url <- livonian_items$muis_url
  livonian_candidates$finna_url <- livonian_items$finna_url
  livonian_candidates$textilebase_url <- livonian_items$textilebase_url
  livonian_candidates$fuds_url <- livonian_items$fuds_url
  livonian_candidates$wikitext <- livonian_items$wikitext

  # Render Betwixt review HTML -------------------------------------------

  render_review(
    livonian_candidates,
    title = paste0("Traditional Livonian Clothing: Room ", room),
    description = paste(
      "Review the descriptions and metadata of photographs",
      "documenting traditional Livonian clothing.",
      "Verify museum accession numbers, institutions,",
      "and exhibition classifications."
    ),
    reviewer_name = "Ieva Pigozne",
    reviewer_iri = "https://orcid.org/0000-0003-1726-0928",
    table_id = file_stem,
    project_id = "finfair",
    row_comment = TRUE,
    review_comment = TRUE,
    filename_stem = paste0(file_stem, "_review"),
    path = output_path
  )

  message("Finished room ", room, ": ", nrow(livonian_items), " photographs")
}

livonian_all <- lapply(1:7, function(room) {

  read_csv(
    file.path(
      output_path,
      paste0("livonian_exhibition_", room, ".csv")
    ),
    show_col_types = FALSE
  ) |>
    mutate(
      room_number = as.integer(room)
    )

}) |>
  bind_rows() |>
  relocate(room_number, .before = accession_number) |>
  mutate ( room_number_range = paste(1:7, collapse =" | "))

write_csv(
  livonian_all,
  file.path(output_path, "livonian_exhibition_all.csv")
)


# Wikimedia Commons file inventory ------------------------------------------

commons_files <- unique(livonian_all$commons_file)

languages <- c("en", "hu", "lv", "ru", "et", "liv")

# Initialize harvesting tibble -----------------------------------------------

livonian_commons <- tibble(
  commons_file = commons_files,
  accession_number = NA_character_,
  institution = NA_character_,
  permission = NA_character_,
  muis_url = NA_character_,
  finna_url = NA_character_,
  textilebase_url = NA_character_,
  fuds_url = NA_character_,
  wikitext = NA_character_
)

for (lang in languages) {
  livonian_commons[[paste0("label_", lang)]] <- NA_character_
  livonian_commons[[paste0("description_", lang)]] <- NA_character_
}

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

# Harvest configuration and persistent checkpoint ----------------------------

sleep <- 3.5
max_tries <- 3

checkpoint_file <- here::here(
  "data-raw", "wikimuseum", "livonian_commons.csv"
)

if (file.exists(checkpoint_file)) {

  saved <- read.csv(
    checkpoint_file,
    stringsAsFactors = FALSE,
    na.strings = c("", "NA"),
    check.names = FALSE,
    colClasses = "character"
  )

  # Restore by Commons filename rather than row position.
  idx <- match(livonian_commons$commons_file, saved$commons_file)
  matched <- which(!is.na(idx))

  for (col in intersect(names(livonian_commons), names(saved))) {
    if (col == "commons_file") next
    livonian_commons[[col]][matched] <- saved[[col]][idx[matched]]
  }

  message("Resuming from existing checkpoint: ", checkpoint_file)

} else {

  # Create the checkpoint before the first network request.
  write.csv(
    livonian_commons,
    checkpoint_file,
    row.names = FALSE,
    na = ""
  )

  message("Created checkpoint: ", checkpoint_file)
}

# Harvest Commons metadata ---------------------------------------------------

for (i in seq_along(commons_files)) {

  # Skip previously harvested pages.
  has_wikitext <- !is.na(livonian_commons$wikitext[i]) &&
    nzchar(trimws(livonian_commons$wikitext[i]))

  if (has_wikitext) {
    message(i, ": Already saved. Skipping.")
    next
  }

  wikitext <- NULL

  # Three attempts, with delays of 3, 6 and 12 seconds.
  for (attempt in seq_len(max_tries)) {

    result <- tryCatch(
      read_commons_wikitext(
        commons_file = livonian_commons$commons_file[i],
        sleep = sleep * 6^(attempt - 1)
      ),
      error = function(e) e
    )

    if (
      !inherits(result, "error") &&
      length(result) == 1L &&
      !is.na(result) &&
      nzchar(trimws(result))
    ) {
      wikitext <- result
      break
    }

    warning(
      "Attempt ", attempt, "/", max_tries,
      " failed for ", livonian_commons$commons_file[i],
      if (inherits(result, "error")) {
        paste0(": ", conditionMessage(result))
      } else {
        ": Empty response"
      }
    )
  }

  # Stop on persistent failure without losing earlier records.
  if (is.null(wikitext)) {
    message("Stopping at row ", i, ". Checkpoint preserved.")
    break
  }

  # External identifiers -----------------------------------------------------

  livonian_commons$textilebase_url[i] <- find_value(
    wikitext, textilebase_regex,
    url = TRUE
  )

  livonian_commons$fuds_url[i] <- find_value(
    wikitext, finnougric_regex,
    url = TRUE
  )

  livonian_commons$finna_url[i] <- find_value(
    wikitext, finna_regex,
    url = TRUE
  )

  livonian_commons$muis_url[i] <- find_value(
    wikitext, muis_regex,
    url = TRUE
  )

  # Multilingual titles and descriptions -------------------------------------

  for (lang in languages) {

    livonian_commons[[paste0("label_", lang)]][i] <-
      find_language_value(wikitext, "title", lang)

    livonian_commons[[paste0("description_", lang)]][i] <-
      find_language_value(wikitext, "description", lang)
  }

  # Accession number ---------------------------------------------------------

  livonian_commons$accession_number[i] <- find_value(
    livonian_commons$label_en[i], inventory_regex
  )

  # Institution --------------------------------------------------------------

  livonian_commons$institution[i] <- find_value(
    wikitext, institution_regex
  ) |>
    sub(".*\\{\\{Institution:([^}]+)\\}\\}", "\\1", x = _)

  # Permission ---------------------------------------------------------------

  livonian_commons$permission[i] <- find_value(
    wikitext, permission_regex
  ) |>
    sub(".*\\{\\{([^}]+)\\}\\}", "\\1", x = _)

  # Store wikitext after successful extraction --------------------------------

  livonian_commons$wikitext[i] <- wikitext

  # Persist each completed record immediately --------------------------------

  write.csv(
    livonian_commons,
    checkpoint_file,
    row.names = FALSE,
    na = ""
  )

  message(
    i, "/", length(commons_files), ": ",
    livonian_commons$commons_file[i],
    " - ", livonian_commons$institution[i],
    " (", livonian_commons$accession_number[i], ") [saved]"
  )
}
