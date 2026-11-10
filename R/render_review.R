#' Render a Betwixt review
#'
#' @description
#' Renders a Betwixt candidate dataset as an interactive, standalone HTML
#' review. The `type` argument selects between a web-based review using
#' HTTP(S) resources and a local review using files stored alongside the
#' generated HTML document.
#'
#' Both formats preserve the original candidate values alongside editable
#' review values. The generated HTML includes the original filename and
#' creation timestamp as artefact provenance. Timestamps are recorded in
#' ISO 8601 UTC format at one-second precision.
#'
#' A review belongs to a project, has a filename stem, and has a non-negative
#' sequence number. Sequence `0` represents the initial candidate review.
#' Subsequent review states may use sequence `1`, `2`, and so on. Saving a
#' draft or finalising a review does not itself change the sequence.
#'
#' The project identifier identifies the larger knowledge-production project,
#' while the table identifier identifies a table across successive review
#' states. The filename stem independently determines the saved HTML filename.
#'
#' @details
#' The `"web_html"` renderer accepts HTTP(S) references in `input_url` and
#' `input_media_url`. These references are validated syntactically without
#' checking network availability.
#'
#' The `"local_html"` renderer uses local resource paths, including relative
#' references such as `media/photo.jpg`. Resources must already exist and be
#' readable. Relative paths are resolved against the review output directory.
#' The local renderer does not copy resources automatically.
#'
#' Both formats embed the review CSS and JavaScript in the HTML document.
#' Local reviews can therefore be moved together with their referenced
#' resources without changing the relative paths.
#'
#' @param candidate A Betwixt candidate dataset.
#' @param ... Arguments passed to the selected review renderer.
#' @param type Review format: `"web_html"` (default) or `"local_html"`.
#' @param cols Optional named character vector containing presentation labels
#'   for candidate and context columns.
#' @param subheadings Optional named character vector containing presentation
#'   subheadings for candidate columns.
#' @param title Character string used as the review title.
#' @param description Character string containing review instructions.
#' @param filename_stem Base name for saved review files. Sequences greater
#'   than `0` append the sequence number.
#' @param reviewer_name Initial reviewer name.
#' @param reviewer_iri IRI identifying the reviewer, such as an ORCID, ISNI,
#'   or Wikidata URI.
#' @param project_id Identifier of the larger knowledge-production project.
#' @param table_id Identifier of the table across successive review states.
#' @param sequence Single non-negative integer identifying the review sequence.
#'   Defaults to `0`, representing the initial candidate review.
#' @param row_comment Logical. Add an optional comment field to each review row.
#'   Defaults to `FALSE`.
#' @param review_comment Logical. Add an optional comment field for the review
#'   as a whole. Defaults to `FALSE`.
#' @param input_link_target How links in the Input column are opened.
#'   `"blank"` (default) opens resources in a new browser tab.
#'   `"modal"` opens resources in a popup within the review page.
#'   Does not affect semantic assertion or definition links.
#' @param path Output directory for the generated HTML. For `"web_html"`,
#'   `NULL` returns the HTML without writing a file. For `"local_html"`,
#'   an existing directory is required to resolve local resource references.
#'
#' @return A character string containing the rendered HTML, returned invisibly
#'   when written to `path`.
#'
#' @seealso [create_candidate_dataset()],
#'   [validate_candidate_dataset()],
#'   [validate_external_resources()],
#'   [validate_local_resources()]
#' @export

render_review <- function(candidate, ..., type = "web_html") {
  switch(type,
    web_html = render_web_html_review(candidate, ...),
    local_html = render_local_html_review(candidate, ...),
    stop("Unsupported review type: ", type, call. = FALSE)
  )
}

#' Render a web-based HTML review
#'
#' @rdname render_review
#' @examples
#' # Review using externally hosted photographs.
#' data(delini)
#'
#' candidates <- create_candidate_dataset(
#'   input_media_url = delini$input_media_url,
#'   input_description = delini$input_description,
#'   label = delini$label,
#'   description = delini$description,
#'   subject = delini$subject,
#'   subject_range = delini$subject_range,
#'   subject_definition = delini$subject_definition
#' )
#'
#' review_html <- render_web_html_review(
#'   candidates,
#'   title = "Deliņi Farmstead Review"
#' )
#' @export

render_web_html_review <- function(
  candidate,
  cols = NULL,
  subheadings = NULL,
  title = "Betwixt Review",
  description = "Please review the following claims.",
  filename_stem = "betwixt-review",
  reviewer_name = "",
  reviewer_iri = "",
  table_id = "",
  project_id = "",
  sequence = 0L,
  row_comment = FALSE,
  review_comment = FALSE,
  input_link_target = "blank",
  path = NULL
) {
  # Validate candidate structure and external resources.
  validate_candidate_dataset(candidate)
  validate_external_resources(candidate)

  # Validate the review sequence.
  if (length(sequence) != 1L ||
    is.na(sequence) ||
    sequence < 0 ||
    sequence != as.integer(sequence)) {
    stop(
      "sequence must be a single non-negative integer.",
      call. = FALSE
    )
  }

  # Subject is a required column
  if (!"subject" %in% names(candidate)) {
    stop(
      "`render_review()` requires a `subject` column. ",
      "Each candidate row must identify the subject of the assertions ",
      "being reviewed.",
      call. = FALSE
    )
  }

  # Validate how Input links are opened.
  if (!is.character(input_link_target) ||
    length(input_link_target) != 1L ||
    is.na(input_link_target) ||
    !input_link_target %in% c("blank", "modal")) {
    stop(
      "`input_link_target` must be 'blank' or 'modal'.",
      call. = FALSE
    )
  }

  # Store the validated sequence as an integer.
  sequence <- as.integer(sequence)

  # Record the creation time in ISO 8601 UTC at one-second precision.
  original_created_at <- format(
    Sys.time(),
    tz = "UTC",
    format = "%Y-%m-%dT%H:%M:%SZ"
  )

  # Record the filename of the HTML artefact created by this render.
  original_filename <- if (sequence == 0L) {
    paste0(filename_stem, ".html")
  } else {
    paste0(filename_stem, "_", sequence, ".html")
  }

  # Prepare the candidate data for rendering.
  context <- prepare_review_context(candidate)

  # Generate the review table.
  table_html <- review_context_html(
    context,
    cols = cols,
    subheadings = subheadings,
    row_comment = row_comment,
    input_link_target = input_link_target
  )

  # Locate the packaged review resources.
  css_file <- system.file(
    "templates", "css", "betwixt-review.css",
    package = "betwixt"
  )

  js_file <- system.file(
    "templates", "js", "betwixt-review.js",
    package = "betwixt"
  )

  # Require both resources before constructing the review.
  if (!nzchar(css_file)) {
    stop("Betwixt review CSS was not found.", call. = FALSE)
  }

  if (!nzchar(js_file)) {
    stop("Betwixt review JavaScript was not found.", call. = FALSE)
  }

  # Read the resources into the standalone document.
  css <- paste(readLines(css_file, warn = FALSE), collapse = "\n")
  js <- paste(readLines(js_file, warn = FALSE), collapse = "\n")

  # Escape document-level text before inserting it into HTML.
  escape_html <- function(x) {
    x <- gsub("&", "&amp;", x, fixed = TRUE)
    x <- gsub("<", "&lt;", x, fixed = TRUE)
    x <- gsub(">", "&gt;", x, fixed = TRUE)
    x <- gsub('"', "&quot;", x, fixed = TRUE)
    x
  }

  # Render non-empty candidate provenance above the site footer.
  provenance <- context$provenance

  provenance_items <- c(
    if (nzchar(provenance$data_manager)) {
      paste0("Data manager: ", escape_html(provenance$data_manager))
    },
    if (nzchar(provenance$data_manager_iri)) {
      paste0("IRI: ", escape_html(provenance$data_manager_iri))
    },
    if (nzchar(provenance$table_id)) {
      paste0("Project: ", escape_html(provenance$table_id))
    },
    if (!is.na(provenance$generated_at) &&
      nzchar(provenance$generated_at)) {
      paste0("Generated: ", escape_html(provenance$generated_at))
    },
    paste0(
      escape_html(provenance$software_agent), " ",
      escape_html(provenance$software_version)
    )
  )

  provenance_footer <- paste0(
    '<div class="candidate-provenance">',
    paste(provenance_items, collapse = " \u00b7 "),
    "</div>\n"
  )

  # Render an optional comment field for the review as a whole.
  review_comment_html <- if (review_comment) {
    paste0(
      '<label class="review-comment">Review comment',
      '<textarea id="review-comment" ',
      'placeholder="Optional comment on review"></textarea>',
      "</label>\n"
    )
  } else {
    ""
  }

  # Assemble the standalone review document.
  html <- assemble_review_html(
    title = title,
    description = description,
    table_html = table_html,
    css = css,
    js = js,
    reviewer_name = reviewer_name,
    reviewer_iri = reviewer_iri,
    project_id = project_id,
    table_id = table_id,
    sequence = sequence,
    original_filename = original_filename,
    original_created_at = original_created_at,
    filename_stem = filename_stem,
    review_comment_html = review_comment_html,
    provenance_footer = provenance_footer
  )

  # Return the HTML directly when no output path is requested.
  if (is.null(path)) {
    return(html)
  }

  # Write the review using the filename recorded in its provenance.
  output <- file.path(path, original_filename)

  writeLines(html, output, useBytes = TRUE)

  message("Review rendered: ", output)

  invisible(html)
}
