#' Create an empty Betwixt candidate template
#'
#' @description
#' Creates an empty candidate dataset template for preparing a Betwixt review.
#' Standard descriptive and evidence columns can be included or omitted.
#' Reviewable candidate columns are supplied through `columns`, while
#' display-only contextual columns are supplied through `context`.
#'
#' @details
#' Candidate datasets can be created in a spreadsheet application after
#' initialising a conforming header with [create_candidate_template()].
#' Re-imported datasets can be checked with [validate_candidate_dataset()]
#' before rendering.
#' @param input_url Include the `input_url` column.
#' @param input_media_url Include the `input_media_url` column.
#' @param input_description Include the `input_description` column.
#' @param label Include the `label` column.
#' @param description Include the `description` column.
#' @param alternative_label Include the `alternative_label` column.
#' @param alternative_description Include the `alternative_description`
#'   column.
#' @param input_relation Include the `input_relation` and
#'   `input_relation_range` columns.
#' @param subject_range Include the `subject_range` column.
#' @param subject_definition Include the `subject_definition` column.
#' @param columns Optional character vector naming additional reviewable
#'   candidate columns. Each name creates a candidate column together with
#'   its `_range` and `_definition` columns.
#' @param context Optional character vector naming display-only contextual
#'   columns.
#'
#' @return
#' An empty tibble containing `row_number` and `subject`, together with the
#' requested evidence, descriptive, candidate, and contextual columns.
#'
#' @examples
#' create_candidate_template()
#'
#' create_candidate_template(
#'   columns = c("instance_of", "heritage_of"),
#'   context = "held_by"
#' )
#'
#' @importFrom stats setNames
#' @export
create_candidate_template <- function(
  input_url = TRUE,
  input_media_url = TRUE,
  input_description = TRUE,
  label = TRUE,
  description = TRUE,
  alternative_label = FALSE,
  alternative_description = FALSE,
  input_relation = FALSE,
  subject_range = TRUE,
  subject_definition = TRUE,
  columns = NULL,
  context = NULL
) {
  names <- c(
    "row_number",
    if (input_url) "input_url",
    if (input_media_url) "input_media_url",
    if (input_description) "input_description",
    if (input_relation) {
      c("input_relation", "input_relation_range")
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

  out <- stats::setNames(rep(list(character()), length(names)), names)
  out$row_number <- integer()

  as.data.frame(out, stringsAsFactors = FALSE)
}
