#' Prepare a Betwixt review context
#'
#' @description
#' Prepares a validated Betwixt candidate dataset for rendering as a review
#' interface.
#'
#' The function separates the serialised candidate dataset into its rendering
#' roles: input information, subject presentation, reviewable assertions,
#' display-only context, and dataset-level provenance.
#'
#' Candidate assertions are identified by the Betwixt candidate-column
#' contract: an assertion-bearing column accompanied by a `_range` or
#' `_definition` column. A candidate may have either or both.
#'
#' `input_url` and `input_media_url` may contain pipe-separated resources and
#' are parsed into character vectors for rendering. `input_label` and
#' `input_description` remain distinct in the candidate dataset but are
#' temporarily combined for presentation by the current renderer.
#'
#' When present, `input_predicate` represents a reviewable candidate relation
#' directed from the input to the subject. It is distinct from provenance and
#' from display-only `context_*` columns.
#'
#' @param candidate A validated Betwixt candidate dataset.
#'
#' @return A renderer-ready list containing the reviewable candidate columns,
#'   display-only context columns, the presence of an input predicate,
#'   row-wise rendering data, and dataset-level provenance. Each row contains
#'   parsed input resources, subject presentation information, reviewable
#'   assertions, and contextual information.
#'
#' @details
#' This function prepares a rendering projection and does not modify the
#' serialised candidate dataset. In particular, combining `input_label` and
#' `input_description` for presentation does not alter their separate semantic
#' representation in the source data.
#'
#' @noRd
#' @keywords internal
prepare_review_context <- function(candidate) {
  # Validate the candidate dataset.
  validate_candidate_dataset(candidate)

  provenance <- attr(candidate, "provenance")

  if (is.null(provenance)) {
    # If there is no provenance at least at Betwixt version number
    provenance <- list(
      data_manager = "",
      data_manager_iri = "",
      data_manager_email = "",
      table_id = "",
      generated_at = NA_character_,
      software_agent = "Betwixt",
      software_version = betwixt_version() # see utils.R
    )
  }

  # Convert a pipe-separated value to a character vector.
  parse_range <- function(x) {
    if (is.na(x)) {
      return(character())
    }
    trimws(strsplit(x, "|", fixed = TRUE)[[1]])
  }

  # Identify candidate and contextual columns.
  range_cols <- grep("_range$", names(candidate), value = TRUE)
  definition_cols <- grep("_definition$", names(candidate), value = TRUE)

  range_names <- sub("_range$", "", range_cols)
  definition_names <- sub("_definition$", "", definition_cols)

  candidate_cols <- union(range_names, definition_names)
  candidate_cols <- candidate_cols[candidate_cols %in% names(candidate)]
  context_cols <- grep("^context_", names(candidate), value = TRUE)

  # Determine whether the optional evidence relation is present.
  has_input_predicate <- "input_predicate" %in% names(candidate)

  # Prepare each candidate row for rendering.
  rows <- lapply(seq_len(nrow(candidate)), function(i) {
    # Prepare the reviewable candidate assertions.
    assertions <- lapply(candidate_cols, function(col) {
      range_col <- paste0(col, "_range")
      definition_col <- paste0(col, "_definition")

      list(
        name = col,
        value = candidate[[col]][i],
        range = if (range_col %in% names(candidate)) {
          parse_range(candidate[[range_col]][i])
        } else {
          character()
        },
        definition = if (definition_col %in% names(candidate)) {
          candidate[[definition_col]][i]
        } else {
          NA_character_
        }
      )
    })

    # Prepare display-only contextual information.
    context <- lapply(context_cols, function(col) {
      list(name = col, value = candidate[[col]][i])
    })

    # Assemble the common rendering information for one row.
    row <- list(
      row_number = candidate$row_number[i],
      input_url = if ("input_url" %in% names(candidate)) {
        parse_range(candidate$input_url[i])
      } else {
        character()
      },
      input_media_url = if ("input_media_url" %in% names(candidate)) {
        parse_range(candidate$input_media_url[i])
      } else {
        character()
      },
      input_description = if ("input_description" %in% names(candidate)) {
        candidate$input_description[i]
      } else {
        NA_character_
      },
      label = if ("label" %in% names(candidate)) {
        candidate$label[i]
      } else {
        NA_character_
      },
      description = if ("description" %in% names(candidate)) {
        candidate$description[i]
      } else {
        NA_character_
      },
      alternative_label = if (
        "alternative_label" %in% names(candidate)
      ) {
        candidate$alternative_label[i]
      } else {
        NA_character_
      },
      alternative_description = if (
        "alternative_description" %in% names(candidate)
      ) {
        candidate$alternative_description[i]
      } else {
        NA_character_
      },
      assertions = assertions,
      context = context
    )

    # Add the optional reviewable evidence relation.
    if (has_input_predicate) {
      row$input_predicate <- candidate$input_predicate[i]

      if ("input_predicate_range" %in% names(candidate)) {
        row$input_predicate_range <- parse_range(
          candidate$input_predicate_range[i]
        )
      }
    }

    row
  })

  # Return the complete renderer-ready context.
  list(
    candidate_columns = candidate_cols,
    context_columns = context_cols,
    has_input_predicate = has_input_predicate,
    rows = rows,
    provenance = provenance
  )
}
