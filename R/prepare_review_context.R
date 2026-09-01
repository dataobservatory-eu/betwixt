#' Prepare a Betwixt review context
#'
#' @description
#' Converts a Betwixt candidate dataset into a simple list structure that can
#' subsequently be used to generate a review interface.
#'
#' Candidate columns are identified by names of the form `col_n`. Context
#' columns are identified by names of the form `context_n`. Evidence relations
#' are included when present but are not required.
#'
#' @param candidate A Betwixt candidate dataset.
#'
#' @return A list containing the candidate column names, context column names,
#'   whether an evidence relation is present, and row-wise review data.
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
    "row_number", "evidence_url", "evidence_text",
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
  candidate_cols <- grep("^col_[0-9]+$", names(candidate), value = TRUE)
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
    row <- list(
      row_number = candidate$row_number[i],
      evidence_url = candidate$evidence_url[i],
      evidence_text = candidate$evidence_text[i],
      label = candidate$label[i],
      description = candidate$description[i],
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
