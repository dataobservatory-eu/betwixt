#' Resolve a missing table IRI
#'
#' @keywords internal
#' @noRd
default_table_iri <- function(
  table_iri = NULL,
  table_id = NA_character_,
  project_id = NA_character_
) {
  has_id <- function(x) {
    is.character(x) &&
      length(x) == 1L &&
      !is.na(x) &&
      nzchar(x)
  }

  # Preserve an explicitly supplied IRI.
  if (has_id(table_iri)) {
    return(table_iri)
  }

  # No table identity available.
  if (!has_id(table_id)) {
    return("")
  }

  iri_path <- if (has_id(project_id)) {
    paste(
      utils::URLencode(project_id, reserved = TRUE),
      utils::URLencode(table_id, reserved = TRUE),
      sep = "/"
    )
  } else {
    utils::URLencode(table_id, reserved = TRUE)
  }

  paste0("https://example.com/", iri_path)
}
