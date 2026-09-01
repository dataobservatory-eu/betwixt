#' Prepare candidate data for review rendering
#'
#' Converts a Betwixt candidate dataset into a renderer-neutral list describing
#' its reviewable assertions, evidence, descriptive information, contextual
#' columns, candidate ranges, and optional evidence relations.
#'
#' This function defines the internal boundary between the candidate data model
#' and presentation-specific renderers. It contains no HTML or template logic,
#' allowing the rendering implementation to change without changing how
#' candidate data are interpreted.
#'
#' @param candidate A Betwixt candidate dataset.
#'
#' @return A renderer-neutral list containing candidate and context column
#'   definitions and row-wise review information.
#'
#' @keywords internal
#' @noRd
review_context_html <- function(
    context,
    cols = NULL,
    subheadings = NULL) {

  # Escape text before inserting it into HTML.
  escape_html <- function(x) {
    x <- as.character(x)
    x <- gsub("&", "&amp;", x, fixed = TRUE)
    x <- gsub("<", "&lt;", x, fixed = TRUE)
    x <- gsub(">", "&gt;", x, fixed = TRUE)
    x <- gsub('"', "&quot;", x, fixed = TRUE)
    x
  }

  # Return a presentation label or fall back to the column name.
  column_label <- function(x, labels) {
    if (is.null(labels) || !x %in% names(labels)) return(x)
    labels[[x]]
  }

  # Render one review qualification control.
  qualification_html <- function() {
    paste0(
      '<div class="qualify">',
      '<button data-qualify="defer">Defer</button>',
      '<button data-qualify="reject">Reject</button>',
      "</div>"
    )
  }

  # Render one reviewable assertion.
  assertion_html <- function(assertion) {
    value <- escape_html(assertion$value)
    has_definition <- !is.na(assertion$definition) &&
      nzchar(assertion$definition)

    if (length(assertion$range) == 0) {

      # Link an existing entity or allow creation of an unresolved one.
      if (has_definition) {
        control <- paste0(
          '<a class="entity-link" href="',
          escape_html(assertion$definition),
          '" target="_blank" rel="noopener">',
          value,
          "</a>"
        )
      } else {
        control <- paste0(
          '<input class="subject-input" type="text" value="',
          value,
          '">',
          '<button type="button" class="create-item">',
          "Create new item",
          "</button>"
        )
      }

    } else {
      options <- vapply(assertion$range, function(option) {
        selected <- if (identical(option, assertion$value)) {
          " selected"
        } else {
          ""
        }

        option_value <- if (identical(option, "Other…")) {
          "__other__"
        } else {
          escape_html(option)
        }

        paste0(
          '<option value="', option_value, '"', selected, ">",
          escape_html(option),
          "</option>"
        )
      }, character(1))

      control <- paste0(
        '<select class="candidate-select">',
        paste(options, collapse = ""),
        "</select>",
        '<input class="write-in" placeholder="Enter another value">'
      )
    }

    definition <- ""

    # Controlled assertions retain a separate definition link.
    if (length(assertion$range) > 0 && has_definition) {
      definition <- paste0(
        '<a class="definition-link" href="',
        escape_html(assertion$definition),
        '" target="_blank" rel="noopener">Definition</a>'
      )
    }

    paste0(
      '<td class="semantic-cell" data-qualification="none">',
      control,
      definition,
      qualification_html(),
      "</td>"
    )
  }

  # Render one display-only context value.
  context_html <- function(item) {
    paste0(
      '<td class="context">',
      escape_html(item$value),
      "</td>"
    )
  }

  # Render headings for the reviewable candidate columns.
  assertion_headers <- vapply(context$candidate_columns, function(x) {
    heading <- column_label(x, cols)
    subheading <- column_label(x, subheadings)

    if (identical(subheading, x)) {
      paste0("<th>", escape_html(heading), "</th>")
    } else {
      paste0(
        '<th><div class="predicate-heading">',
        escape_html(heading),
        "<small>", escape_html(subheading), "</small>",
        "</div></th>"
      )
    }
  }, character(1))

  # Render headings for display-only context columns.
  context_headers <- vapply(context$context_columns, function(x) {
    heading <- column_label(x, cols)

    paste0(
      '<th class="context-head">',
      escape_html(heading),
      "<small>Context — not reviewed</small></th>"
    )
  }, character(1))

  header <- paste0(
    "<thead><tr>",
    '<th class="num">#</th>',
    "<th>Evidence</th>",
    "<th>Label</th>",
    "<th>Description</th>",
    paste(assertion_headers, collapse = ""),
    '<th class="finalise-head">Finalise</th>',
    paste(context_headers, collapse = ""),
    "</tr></thead>"
  )

  # Render each prepared review row.
  rows <- vapply(context$rows, function(row) {
    assertions <- vapply(
      row$assertions, assertion_html, character(1)
    )

    context_values <- vapply(
      row$context, context_html, character(1)
    )

    evidence <- paste0(
      '<td class="evidence">',
      '<a href="', escape_html(row$evidence_url),
      '" target="_blank" rel="noopener">',
      '<img src="', escape_html(row$evidence_url),
      '" alt="Evidence ', escape_html(row$evidence_text), '">',
      "</a>",
      '<div class="media-id">',
      escape_html(row$evidence_text),
      "</div></td>"
    )

    paste0(
      '<tr data-row="', row$row_number,
      '" data-finalised="false" data-outcome="accept">',
      '<td class="num">', row$row_number, "</td>",
      evidence,
      '<td class="label"><input class="text-input" value="',
      escape_html(row$label), '"></td>',
      '<td class="description"><textarea>',
      escape_html(row$description), "</textarea></td>",
      paste(assertions, collapse = ""),
      '<td class="finalise-cell">',
      '<label class="finalise-control">',
      '<input type="checkbox" class="finalise-check">',
      '<span class="finalise-mark"></span>',
      "</label></td>",
      paste(context_values, collapse = ""),
      "</tr>"
    )
  }, character(1))

  # Assemble the generated review table.
  paste0(
    '<table id="review-table">',
    header,
    "<tbody>",
    paste(rows, collapse = "\n"),
    "</tbody></table>"
  )
}

context <- prepare_review_context(delini)

table_html <- review_context_html(
  context,
  cols = c(
    col_1 = "Subject",
    col_2 = "instance of",
    col_3 = "heritage of",
    context_1 = "held by"
  ),
  subheadings = c(
    col_2 = "wdt:P31",
    col_3 = "controlled range"
  )
)

writeLines(table_html, "delini_generated_table.html")
