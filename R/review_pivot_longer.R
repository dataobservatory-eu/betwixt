#' Project a Betwixt dataset to canonical long form
#'
#' @description
#' Projects a Betwixt candidate or reviewed dataset into the canonical
#' assertion representation.
#'
#' The projection preserves the order of rows, fields, and pipe-separated
#' values in the supplied dataset. `row_id` identifies the originating
#' wide-form tuple. `assertion_id` identifies the position of the assertion
#' in the canonical long projection.
#'
#' Dataset identity is represented with `row_id = 0`. Domain assertions,
#' input-to-subject relations, contextual statements, and row comments are
#' assigned distinct components.
#'
#' Range and definition columns describe candidate fields and are not
#' themselves projected as assertions. Review-state columns such as
#' qualifications, finalisation, and outcomes are likewise not domain
#' assertions.
#'
#' @param x A Betwixt candidate or reviewed data frame.
#'
#' @return A tibble with columns `row_id`, `assertion_id`, `component`,
#'   `subject`, `predicate`, and `value`.
#'
#' @importFrom dplyr bind_rows filter mutate select
#' @importFrom tidyr pivot_longer separate_longer_delim
#' @importFrom rlang .data
#' @export
review_pivot_longer <- function(x) {
  representation <- betwixt_representation(x)

  if (representation == "invalid") {
    stop("Invalid Betwixt representation.", call. = FALSE)
  }

  is_reviewed <- "plane" %in% names(x)

  if (is_reviewed) {
    candidate <- dplyr::filter(x, .data$plane == "candidate")
    reviewed_values <- dplyr::filter(x, .data$plane == "reviewed")
    status <- dplyr::filter(x, .data$plane == "status")
  } else {
    candidate <- x
  }

  provenance <- attr(x, "provenance")

  table_id <- if (!is.null(provenance$table_id)) {
    provenance$table_id
  } else {
    ""
  }

  project_id <- if (!is.null(provenance$project_id)) {
    provenance$project_id
  } else {
    ""
  }

  # Dataset identity --------------------------------------------------------

  dataset <- tibble::tibble(
    row_id = 0L,
    component = "dataset_identity",
    subject = table_id,
    predicate = "type",
    value = "schema:Dataset"
  )

  if (!is.na(project_id) && nzchar(project_id)) {
    dataset <- dplyr::bind_rows(
      dataset,
      tibble::tibble(
        row_id = 0L,
        component = "dataset_identity",
        subject = table_id,
        predicate = "project_id",
        value = project_id
      )
    )
  }

  # Domain assertions -------------------------------------------------------

  metadata_cols <- grepl(
    "_range$|_definition$|_qualification$",
    names(candidate)
  )

  structural_cols <- grepl(
    "^(row_id$|input_|context_|comment$|row_comment$|finalised$|outcome$|plane$)",
    names(candidate)
  )

  domain_cols <- names(candidate)[
    !metadata_cols &
      !structural_cols &
      names(candidate) != "subject"
  ]

  domain <- tibble::tibble(
    row_id = integer(),
    component = character(),
    subject = character(),
    predicate = character(),
    value = character()
  )

  if (length(domain_cols) > 0L) {
    domain <- candidate |>
      dplyr::select(dplyr::all_of(c("row_id", "subject", domain_cols))) |>
      tidyr::pivot_longer(
        cols = dplyr::all_of(domain_cols),
        names_to = "predicate",
        values_to = "value",
        values_transform = as.character
      ) |>
      tidyr::separate_longer_delim("value", delim = "|") |>
      dplyr::mutate(
        component = "domain",
        predicate = trimws(.data$predicate),
        value = trimws(.data$value)
      ) |>
      dplyr::filter(!is.na(.data$value), nzchar(.data$value)) |>
      dplyr::select(dplyr::all_of(c("row_id", "component", "subject", "predicate", "value")))
  }

  # Input relation ----------------------------------------------------------

  input <- tibble::tibble(
    row_id = integer(),
    component = character(),
    subject = character(),
    predicate = character(),
    value = character()
  )

  if ("input_media_url" %in% names(candidate)) {
    input <- candidate |>
      dplyr::select(
        dplyr::all_of(c("row_id", "subject", "input_media_url")),
        dplyr::any_of("input_predicate")
      )

    if (!"input_predicate" %in% names(input)) {
      input$input_predicate <- "review_input"
    }

    input$input_predicate[
      is.na(input$input_predicate) |
        !nzchar(trimws(as.character(input$input_predicate)))
    ] <- "review_input"

    input <- input |>
      tidyr::separate_longer_delim("input_media_url", delim = "|") |>
      dplyr::mutate(
        subject = as.character(.data$subject),
        component = "input",
        predicate = trimws(as.character(.data$input_predicate)),
        value = trimws(as.character(.data$input_media_url))
      ) |>
      dplyr::filter(!is.na(.data$value), nzchar(.data$value)) |>
      dplyr::select(dplyr::all_of(c("row_id", "component", "subject", "predicate", "value")))
  }

  # Context assertions ------------------------------------------------------

  context_cols <- grep("^context_", names(candidate), value = TRUE)

  context <- tibble::tibble(
    row_id = integer(),
    component = character(),
    subject = character(),
    predicate = character(),
    value = character()
  )

  if (length(context_cols) > 0L) {
    context <- candidate |>
      dplyr::select(dplyr::all_of(c("row_id", "subject", context_cols))) |>
      tidyr::pivot_longer(
        cols = dplyr::all_of(context_cols),
        names_to = "predicate",
        values_to = "value",
        values_transform = as.character
      ) |>
      tidyr::separate_longer_delim("value", delim = "|") |>
      dplyr::mutate(
        component = "context",
        predicate = trimws(.data$predicate),
        value = trimws(.data$value)
      ) |>
      dplyr::filter(!is.na(.data$value), nzchar(.data$value)) |>
      dplyr::select(dplyr::all_of(c("row_id", "component", "subject", "predicate", "value")))
  }

  # Row comments ------------------------------------------------------------

  comment_col <- intersect(
    c("row_comment", "comment"),
    names(candidate)
  )

  if (is_reviewed) {
    reviewed_comment_col <- intersect(
      c("row_comment", "comment"),
      names(reviewed_values)
    )

    comment_source <- reviewed_values
    comment_col <- reviewed_comment_col
  } else {
    comment_source <- candidate
  }

  comments <- tibble::tibble(
    row_id = integer(),
    component = character(),
    subject = character(),
    predicate = character(),
    value = character()
  )

  if (length(comment_col) > 0L) {
    comments <- comment_source |>
      dplyr::mutate(
        component = "row_comment",
        subject = as.character(.data$row_id),
        predicate = "hasComment",
        value = as.character(.data[[comment_col[[1L]]]])
      ) |>
      dplyr::filter(!is.na(.data$value), nzchar(.data$value)) |>
      dplyr::select(dplyr::all_of(c("row_id", "component", "subject", "predicate", "value")))
  }

  # Assertion provenance ----------------------------------------------------

  assertion_provenance <- tibble::tibble(
    row_id = integer(),
    component = character(),
    subject = character(),
    predicate = character(),
    value = character()
  )

  if (is_reviewed) {
    review_cols <- intersect(domain_cols, names(status))

    if (length(review_cols) > 0L) {
      candidate_long <- candidate |>
        dplyr::select(dplyr::all_of(c("row_id", "subject", review_cols))) |>
        tidyr::pivot_longer(
          cols = dplyr::all_of(review_cols),
          names_to = "predicate",
          values_to = "candidate_value",
          values_transform = as.character
        )

      reviewed_long <- reviewed_values |>
        dplyr::select(dplyr::all_of(c("row_id", review_cols))) |>
        tidyr::pivot_longer(
          cols = dplyr::all_of(review_cols),
          names_to = "predicate",
          values_to = "reviewed_value",
          values_transform = as.character
        )

      status_long <- status |>
        dplyr::select(dplyr::all_of(c("row_id", review_cols))) |>
        tidyr::pivot_longer(
          cols = dplyr::all_of(review_cols),
          names_to = "predicate",
          values_to = "review_status",
          values_transform = as.character
        )

      assertion_provenance <- candidate_long |>
        dplyr::left_join(
          reviewed_long,
          by = c("row_id", "predicate")
        ) |>
        dplyr::left_join(
          status_long,
          by = c("row_id", "predicate")
        ) |>
        dplyr::filter(
          !is.na(.data$candidate_value) |
            !is.na(.data$reviewed_value)
        ) |>
        dplyr::mutate(
          candidate_value = trimws(.data$candidate_value),
          reviewed_value = trimws(.data$reviewed_value),
          review_status = trimws(.data$review_status),
          value = dplyr::case_when(
            .data$review_status == "corrected" ~ paste0(
              .data$candidate_value,
              " -> ",
              .data$reviewed_value
            ),
            .data$review_status %in% c("rejected", "deferred") ~ .data$candidate_value,
            TRUE ~ .data$reviewed_value
          ),
          component = "assertion_provenance"
        ) |>
        dplyr::filter(!is.na(.data$value), nzchar(.data$value)) |>
        dplyr::select(dplyr::all_of(c("row_id", "component", "subject", "predicate", "value")))
    }
  }

  # Dataset comment ---------------------------------------------------------

  dataset_comment <- tibble::tibble(
    row_id = integer(),
    component = character(),
    subject = character(),
    predicate = character(),
    value = character()
  )

  if (!is.null(provenance$review_comment) &&
    !is.na(provenance$review_comment) &&
    nzchar(provenance$review_comment)) {
    dataset_comment <- tibble::tibble(
      row_id = 0L,
      component = "dataset_comment",
      subject = table_id,
      predicate = "review_comment",
      value = as.character(provenance$review_comment)
    )
  }

  # Dataset provenance ------------------------------------------------------

  dataset_provenance <- tibble::tibble(
    row_id = integer(),
    component = character(),
    subject = character(),
    predicate = character(),
    value = character()
  )

  if (!is.null(provenance)) {
    provenance_values <- provenance[
      setdiff(
        names(provenance),
        c("project_id", "table_id", "review_comment")
      )
    ]

    if (length(provenance_values) > 0L) {
      dataset_provenance <- tibble::tibble(
        row_id = 0L,
        component = "dataset_provenance",
        subject = table_id,
        predicate = names(provenance_values),
        value = as.character(
          unlist(provenance_values, use.names = FALSE)
        )
      ) |>
        dplyr::filter(!is.na(.data$value), nzchar(.data$value))
    }
  }

  # Canonical ordering ------------------------------------------------------

  dplyr::bind_rows(
    dataset, domain, input, context, comments,
    assertion_provenance, dataset_comment, dataset_provenance
  ) |>
    dplyr::mutate(assertion_id = dplyr::row_number()) |>
    dplyr::select(dplyr::all_of(c("row_id", "assertion_id", "component", "subject", "predicate", "value")))
}
