#' Renders a Betwixt candidate dataset as a standalone HTML review.
#'
#' @description
#' The generated review preserves the original candidate values alongside
#' editable review values. It also records the original HTML filename and its
#' creation time as artefact provenance. Timestamps are represented as
#' ISO 8601 UTC values at one-second precision.
#'
#' A review belongs to a project, has a filename stem, and has a non-negative
#' sequence number. Sequence `0` represents the initial candidate review.
#' Subsequent review states may use sequence `1`, `2`, and so on. Saving a
#' draft or finalising a review does not itself change the sequence.
#' The project identifier records the stable identity of the review project.
#' The filename stem is used independently to construct saved review filenames.
#'
#' @param candidate A Betwixt candidate dataset.
#' @param cols An optional named character vector containing presentation
#'   labels for candidate and context columns.
#' @param subheadings An optional named character vector containing
#'   presentation subheadings for candidate columns.
#' @param title Character string used as the review title.
#' @param description Character string containing review instructions.
#' @param filename_stem Character string used as the base name for saved review
#'   files. Review sequences greater than `0` append the sequence number.
#' @param reviewer_name Character string containing the initial reviewer name.
#' @param reviewer_iri Character string containing an IRI identifying the
#'   reviewer, such as an ORCID, ISNI, or Wikidata URI.
#' @param project_id Character string identifying the review project.
#' @param sequence A single non-negative integer identifying the review
#'   sequence. The initial candidate review has sequence `0`.
#' @param row_comment Logical. If `TRUE`, adds an optional reviewer comment
#'   field to each review row. Defaults to `FALSE`.
#' @param review_comment Logical. If `TRUE`, adds an optional comment field
#'   for the review as a whole. Defaults to `FALSE`.
#' @param path Optional directory where the standalone review HTML is written.
#'   If `NULL`, the rendered HTML is returned without writing a file.
#' @return Invisibly returns the rendered HTML when `path` is supplied;
#'   otherwise returns the HTML as a character string.
#'
#' @examples
#' candidates <- create_candidate_dataset(
#'   evidence_media_url = delini$evidence_media_url,
#'   evidence_text = delini$evidence_text,
#'   label = delini$label,
#'   description = delini$description,
#'   subject = delini$subject,
#'   subject_range = delini$subject_range,
#'   subject_definition = delini$subject_definition
#' )
#'
#' review_html <- render_review(
#'   candidates,
#'   title = "Delini review"
#' )
#' @export
render_review <- function(
  candidate,
  cols = NULL,
  subheadings = NULL,
  title = "Betwixt Review",
  description = "Please review the following claims.",
  filename_stem = "betwixt-review",
  reviewer_name = "",
  reviewer_iri = "",
  project_id = "",
  sequence = 0L,
  row_comment = FALSE,
  review_comment = FALSE,
  path = NULL
) {
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
    row_comment = row_comment
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
    if (nzchar(provenance$project_id)) {
      paste0("Project: ", escape_html(provenance$project_id))
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
    paste(provenance_items, collapse = " · "),
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
  html <- paste0(
    "<!doctype html>\n",
    '<html lang="en">\n',
    "<head>\n",
    '<meta charset="utf-8">\n',
    '<meta name="viewport" ',
    'content="width=device-width, initial-scale=1">\n',
    "<title>", escape_html(title), "</title>\n",
    "<style>\n", css, "\n</style>\n",
    "</head>\n",
    "<body>\n",
    "<main>\n",
    '<header class="review-header">\n',
    "<h1>", escape_html(title), "</h1>\n",
    '<p class="instructions">',
    escape_html(description),
    "</p>\n",
    "</header>\n",
    '<div class="table-wrap">\n',
    table_html,
    "\n</div>\n",
    '<div class="footer">\n',
    '<span id="summary"></span>\n',
    "</div>\n",
    '<section class="review-meta">\n',
    "<h2>Review metadata</h2>\n",
    '<div class="reviewer-bottom">\n',
    "<label>Reviewer",
    '<input id="reviewer-name" type="text" value="',
    escape_html(reviewer_name), '">',
    "</label>\n",
    "<label>Reviewer email",
    '<input id="reviewer-email" type="email">',
    "</label>\n",
    "<label>Reviewer IRI (e.g. ORCID, ISNI, Wikidata)",
    '<input id="reviewer-iri" class="reviewer-iri-input" ',
    'type="text" value="',
    escape_html(reviewer_iri), '">',
    "</label>\n",
    "<label>Project ID",
    '<input id="project-id" class="project-id-input" ',
    'type="text" value="',
    escape_html(project_id), '" readonly>',
    "</label>\n",
    "<label>Sequence",
    '<input id="review-sequence" type="number" value="',
    sequence, '" readonly>',
    "</label>\n",
    "<label>Original filename",
    '<input id="original-filename" type="text" value="',
    escape_html(original_filename), '" readonly>',
    "</label>\n",
    "<label>Filename",
    '<input id="review-filename" type="text" value="',
    escape_html(original_filename), '" readonly>',
    "</label>\n",
    "<label>Original created at",
    '<input id="original-created-at" type="text" value="',
    original_created_at, '" readonly>',
    "</label>\n",
    "<label>Started at",
    '<input id="review-started-at" type="text" readonly>',
    "</label>\n",
    "<label>Last saved at",
    '<input id="review-last-saved-at" type="text" readonly>',
    "</label>\n",
    "<label>Ended at",
    '<input id="review-ended-at" type="text" readonly>',
    "</label>\n",
    "<label>Status",
    '<input id="review-status" type="text" ',
    'value="in-progress" readonly>',
    "</label>\n",
    "</div>\n",

    # Add the optional review-level comment.
    review_comment_html,

    # Store the filename stem as non-editable process metadata.
    '<input id="filename-stem" type="hidden" value="',
    escape_html(filename_stem), '">\n',
    '<div class="save-actions">\n',
    '<button id="save-draft" type="button">',
    "Save draft",
    "</button>\n",
    '<button id="save-final" type="button">',
    "Finalise review",
    "</button>\n",
    "</div>\n",
    '<div id="save-status" class="save-status"></div>\n',
    "</section>\n",
    provenance_footer,
    '<footer class="site-footer">',
    "Created with Betwixt semantic review · ",
    '<a href="https://github.com/dataobservatory-eu/betwixt" ',
    'target="_blank" rel="noopener">GitHub</a>',
    " · ",
    '<a href="https://doi.org/10.5281/zenodo.22091535" ',
    'target="_blank" rel="noopener">',
    "doi:10.5281/zenodo.22091535</a>",
    "</footer>\n",
    "</main>\n",
    "<script>\n", js, "\n</script>\n",
    "</body>\n",
    "</html>\n"
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
