#' Render a Betwixt review
#'
#' @description
#' Renders a Betwixt candidate dataset as a standalone HTML review.
#'
#' A review belongs to a project, has a filename stem, and has a non-negative
#' sequence number. Sequence `0` represents the initial candidate review.
#' Subsequent review states may use sequence `1`, `2`, and so on. Saving a
#' draft or finalising a review does not itself change the sequence.
#'
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
#' @param project_id Character string identifying the review project.
#' @param sequence A single non-negative integer identifying the review
#'   sequence. The initial candidate review has sequence `0`.
#' @param path Optional directory where the standalone review HTML is written.
#'   If `NULL`, the rendered HTML is returned without writing a file.
#' @return Invisibly returns the rendered HTML when `path` is supplied;
#'   otherwise returns the HTML as a character string.
#'
#' @export
betwixt_render <- function(
  candidate,
  cols = NULL,
  subheadings = NULL,
  title = "Betwixt Review",
  description = "Please review the following claims.",
  filename_stem = "betwixt-review",
  project_id = "",
  sequence = 0L,
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

  sequence <- as.integer(sequence)

  # Prepare the candidate data for rendering.
  context <- prepare_review_context(candidate)

  # Generate the review table.
  table_html <- review_context_html(
    context,
    cols = cols,
    subheadings = subheadings
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
    '<input id="reviewer-name" type="text">',
    "</label>\n",
    "<label>Reviewer email",
    '<input id="reviewer-email" type="email">',
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
    '<footer class="site-footer">',
    "Betwixt semantic review",
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

  # Construct the sequence-specific initial review filename.
  stem <- if (sequence == 0L) {
    filename_stem
  } else {
    paste0(filename_stem, "_", sequence)
  }

  output <- file.path(
    path,
    paste0(stem, ".html")
  )

  # Write the standalone review and return it invisibly.
  writeLines(html, output, useBytes = TRUE)
  invisible(html)
}
