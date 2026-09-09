#' Prepare a Betwixt review context
#'
#' @description
#' Converts a Betwixt candidate dataset into a simple list structure that can
#' subsequently be used to generate a review interface.
#'
#'
#' Candidate columns are identified by accompanying `_range` and
#' `_definition` columns. For example, the candidate column `instance_of`
#' is associated with `instance_of_range` and `instance_of_definition`.
#'
#' Pipe-separated values in `evidence_media_url` and `evidence_url` are parsed
#' into character vectors. Each review row may therefore contain zero, one, or
#' multiple evidence media URLs and evidence resource URLs for subsequent
#' rendering.
#'
#' @param candidate A Betwixt candidate dataset.
#'
#' @return A list containing the candidate column names, context column names,
#'   whether an evidence relation is present, and row-wise review data. Each
#'   row contains parsed evidence media and resource URLs, primary and
#'   alternative descriptive information, reviewable assertions, and
#'   display-only context.
#'
#' @noRd
#' @keywords internal
prepare_review_context <- function(candidate) {
  # Validate the candidate dataset.
  if (!is.data.frame(candidate)) {
    stop("candidate must be a data frame.", call. = FALSE)
  }

  # Convert a canonical or legacy candidate range to a character vector.
  parse_range <- function(x) {
    if (is.na(x)) {
      return(character())
    }
    trimws(strsplit(x, "|", fixed = TRUE)[[1]])
  }

  # Check the columns required by every candidate dataset.
  required <- c(
    "row_number", "evidence_url", "evidence_media_url", "evidence_text",
    "label", "description"
  )

  missing <- setdiff(required, names(candidate))

  if (length(missing) > 0) {
    stop(
      "Missing required columns: ", paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  # Identify candidate and contextual columns.
  range_cols <- grep("_range$", names(candidate), value = TRUE)
  definition_cols <- grep("_definition$", names(candidate), value = TRUE)

  range_names <- sub("_range$", "", range_cols)
  definition_names <- sub("_definition$", "", definition_cols)

  candidate_cols <- intersect(range_names, definition_names)
  context_cols <- grep("^context_[0-9]+$", names(candidate), value = TRUE)

  # Determine whether the optional evidence relation is present.
  has_evidence_relation <- "evidence_relation" %in% names(candidate)

  # Prepare each candidate row for rendering.
  rows <- lapply(seq_len(nrow(candidate)), function(i) {
    # Prepare the reviewable candidate assertions.
    assertions <- lapply(candidate_cols, function(col) {
      list(
        name = col,
        value = candidate[[col]][i],
        range = parse_range(candidate[[paste0(col, "_range")]][i]),
        definition = candidate[[paste0(col, "_definition")]][i]
      )
    })

    # Prepare display-only contextual information.
    context <- lapply(context_cols, function(col) {
      list(name = col, value = candidate[[col]][i])
    })

    # Assemble the common rendering information for one row.
    # Several evidence URL's may be pipe separated.
    row <- list(
      row_number = candidate$row_number[i],
      evidence_url = parse_range(candidate$evidence_url[i]),
      evidence_media_url = parse_range(candidate$evidence_media_url[i]),
      evidence_text = candidate$evidence_text[i],
      label = candidate$label[i],
      description = candidate$description[i],
      alternative_label = if (
        "alternative_label" %in% names(candidate)) {
        candidate$alternative_label[i]
      } else {
        NA_character_
      },
      alternative_description = if (
        "alternative_description" %in% names(candidate)) {
        candidate$alternative_description[i]
      } else {
        NA_character_
      },
      assertions = assertions,
      context = context
    )

    # Add the optional reviewable evidence relation.
    if (has_evidence_relation) {
      row$evidence_relation <- candidate$evidence_relation[i]

      if ("evidence_relation_range" %in% names(candidate)) {
        row$evidence_relation_range <- parse_range(
          candidate$evidence_relation_range[i]
        )
      }
    }

    row
  })

  # Return the complete renderer-ready context.
  list(
    candidate_columns = candidate_cols,
    context_columns = context_cols,
    has_evidence_relation = has_evidence_relation,
    rows = rows
  )
}
