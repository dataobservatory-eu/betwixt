#' Create a Betwixt candidate template
#'
#' @description
#' Creates a candidate dataset template for preparing a Betwixt review.
#' Standard descriptive and evidence columns can be included or omitted.
#' Additional reviewable columns are specified through `columns`, while
#' display-only contextual columns are specified through `context`.
#'
#' @details
#' By default, creates a header-only template for use in a spreadsheet
#' application. Set `n` to initialise rows for programmatic population.
#' Re-imported datasets can be checked with [validate_candidate_dataset()]
#' before rendering.
#'
#' @param input_url Include the `input_url` column.
#' @param input_media_url Include the `input_media_url` column.
#' @param input_label Include `input_label` for short human-readable
#'   labels identifying the inputs.
#' @param input_description Include `input_description` for optional
#'   human-readable descriptions of the inputs.
#' @param label Include `label` for optional human-readable subject labels.
#' @param description Include `description` for optional subject descriptions.
#' @param alternative_label Include `alternative_label` for alternative
#'   subject labels, such as translations or labels for different audiences.
#' @param alternative_description Include `alternative_description` for
#'   alternative subject descriptions.
#' @param input_predicate Include `input_predicate` and
#'   `input_predicate_range` columns.
#' @param subject_range Include the `subject_range` column.
#' @param subject_definition Include the `subject_definition` column.
#' @param columns Optional character vector naming additional reviewable
#'   candidate columns. Each creates a column with corresponding `_range`
#'   and `_definition` columns.
#' @param context Optional character vector naming display-only contextual
#'   columns.
#' @param n Number of rows to initialise. `NULL` (default) or `0` creates
#'   a header-only template; a positive whole number creates rows with
#'   sequential `row_id` values.
#'
#' @return
#' A data.frame containing `row_id`, `subject`, and the requested evidence,
#' descriptive, candidate, and contextual columns. Unpopulated fields are `NA`.
#'
#' @examples
#' create_candidate_template()
#'
#' create_candidate_template(
#'   n = 3,
#'   columns = c("instance_of", "heritage_of"),
#'   context = "held_by"
#' )
#'
#' @seealso [create_candidate_dataset()]
#' @importFrom stats setNames
#' @export
create_candidate_template <- function(
  input_url = TRUE,
  input_media_url = TRUE,
  input_label = TRUE,
  input_description = TRUE,
  label = TRUE,
  description = TRUE,
  alternative_label = FALSE,
  alternative_description = FALSE,
  input_predicate = FALSE,
  subject_range = TRUE,
  subject_definition = TRUE,
  columns = NULL,
  context = NULL,
  n = 0
) {
  if (!is.null(n)) {
    if (
      length(n) != 1L || !is.numeric(n) ||
        is.na(n) || !is.finite(n) ||
        n < 0 || n != floor(n)
    ) {
      stop(
        "`n` must be a single, non-negative, finite whole number ",
        "specifying the number of rows to create.",
        call. = FALSE
      )
    }
  }

  names <- c(
    "row_id",
    if (input_url) "input_url",
    if (input_media_url) "input_media_url",
    if (input_label) "input_label",
    if (input_description) "input_description",
    if (input_predicate) {
      c("input_predicate", "input_predicate_range")
    },
    if (label) "label",
    if (description) "description",
    if (alternative_label) "alternative_label",
    if (alternative_description) "alternative_description",
    "subject",
    if (subject_range) "subject_range",
    if (subject_definition) "subject_definition"
  )

  if (length(columns)) {
    names <- c(
      names,
      unlist(lapply(
        columns,
        \(x) c(x, paste0(x, "_range"), paste0(x, "_definition"))
      ))
    )
  }

  names <- c(names, context)

  # Create the header or an empty, 0-row data.frame
  out <- stats::setNames(rep(list(character()), length(names)), names)
  out$row_id <- integer()

  if (!is.null(n) && n > 0L) {
    # If non-empty template was requested with n rows
    out <- stats::setNames(
      rep(list(rep(NA_character_, n)), length(names)),
      names
    )
    out$row_id <- seq_len(n)
  }

  as.data.frame(out, stringsAsFactors = FALSE)
}
