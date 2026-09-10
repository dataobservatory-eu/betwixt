#' Generate a Betwixt review table
#'
#' @description
#' Converts a prepared Betwixt review context into an HTML table containing
#' evidence, descriptive information, reviewable assertions, finalisation
#' controls, optional row comments, and display-only contextual information.
#'
#' Evidence media supplied through `evidence_media_url` are presented inline
#' as images, while evidence resources supplied through `evidence_url` are
#' rendered as links that can be opened by the reviewer. Multiple evidence
#' media or resource URLs may be supplied and are rendered independently.
#'
#' Primary and alternative labels and descriptions are rendered as editable
#' fields. Alternative columns are included only when corresponding values
#' are present in the review context.
#'
#' Reviewable assertions are rendered according to their available semantic
#' information. Candidate ranges are rendered as selection controls, resolved
#' entities as links, and unresolved entities as editable values. Presentation
#' labels and subheadings may be supplied independently of the candidate data
#' model.
#'
#' Original candidate values are preserved in the generated HTML independently
#' of their editable values. This allows subsequent reviewer edits to be
#' distinguished from the candidate state when a saved review is read back
#' into Betwixt.
#'
#' Contextual fields are also preserved with their column identities and values
#' in the generated HTML. They are display-only metadata: they are not editable,
#' do not receive review qualifications, and are not treated as assertions.
#'
#' This is an internal HTML generation step. Candidate data should first be
#' converted with `prepare_review_context()`.
#'
#' This is an internal HTML generation step. Candidate data should first be
#' converted with `prepare_review_context()`.
#'
#' @param context A renderer-neutral review context produced by
#'   `prepare_review_context()`.
#' @param cols An optional named character vector containing presentation
#'   labels for candidate and context columns.
#' @param subheadings An optional named character vector containing
#'   presentation subheadings for candidate columns.
#' @param row_comment Logical. If `TRUE`, adds an optional reviewer comment
#'   field to each review row. Defaults to `FALSE`.
#'
#' @return A character string containing the generated HTML review table.
#'
#' @keywords internal
#' @noRd
review_context_html <- function(
  context,
  cols = NULL,
  subheadings = NULL,
  row_comment = FALSE
) {
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
    if (is.null(labels) || !x %in% names(labels)) {
      return(x)
    }

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

  # Create the optional reviewer comment
  row_comment_header <- if (row_comment) {
    '<th class="row-comment">Reviewer comment</th>'
  } else {
    ""
  }

  # Render one reviewable assertion.
  assertion_html <- function(assertion) {
    value <- escape_html(assertion$value)
    has_definition <- !is.na(assertion$definition) &&
      nzchar(assertion$definition)
    is_url <- grepl(
      "^https?://",
      as.character(assertion$value),
      ignore.case = TRUE
    )

    if (length(assertion$range) == 0) {
      # Link a resolved entity, link a URL value, or allow creation of an
      # unresolved one.
      if (has_definition) {
        control <- paste0(
          '<a class="entity-link" href="',
          escape_html(assertion$definition),
          '" target="_blank" rel="noopener">',
          value,
          "</a>"
        )
      } else if (is_url && assertion$name != "subject") {
        control <- paste0(
          '<a class="entity-link" href="',
          escape_html(assertion$value),
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

    # Preserve the assertion identity and original candidate value.
    paste0(
      '<td class="semantic-cell" data-column="',
      escape_html(assertion$name),
      '" data-qualification="none" data-candidate="',
      value,
      '">',
      control,
      definition,
      qualification_html(),
      "</td>"
    )
  }

  # Render one display-only context value.
  context_html <- function(item) {
    paste0(
      '<td class="context" data-context="',
      escape_html(item$name),
      '" data-value="',
      escape_html(item$value),
      '">',
      escape_html(item$value),
      "</td>"
    )
  }

  # Render headings for reviewable candidate columns.
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
      "<small>Context — not reviewed</small>",
      "</th>"
    )
  }, character(1))

  # Determine whether alternative descriptive columns are required.
  has_alternative_label <- any(vapply(
    context$rows,
    function(row) {
      !is.na(row$alternative_label) &&
        nzchar(row$alternative_label)
    },
    logical(1)
  ))

  has_alternative_description <- any(vapply(
    context$rows,
    function(row) {
      !is.na(row$alternative_description) &&
        nzchar(row$alternative_description)
    },
    logical(1)
  ))

  # Render alternative descriptive headings only when required.
  alternative_headers <- paste0(
    if (has_alternative_label) {
      "<th>Alternative label</th>"
    } else {
      ""
    },
    if (has_alternative_description) {
      "<th>Alternative description</th>"
    } else {
      ""
    }
  )

  # Assemble the table header.
  header <- paste0(
    "<thead><tr>",
    '<th class="num">#</th>',
    "<th>Evidence</th>",
    "<th>Label</th>",
    "<th>Description</th>",
    alternative_headers,
    paste(assertion_headers, collapse = ""),
    '<th class="finalise-head">Finalise</th>',
    row_comment_header,
    paste(context_headers, collapse = ""),
    "</tr></thead>"
  )

  # Render each prepared review row.
  rows <- vapply(context$rows, function(row) {
    assertions <- vapply(
      row$assertions,
      assertion_html,
      character(1)
    )

    context_values <- vapply(
      row$context,
      context_html,
      character(1)
    )

    # Represent missing evidence text as an empty value.
    evidence_text <- if (is.na(row$evidence_text)) {
      ""
    } else {
      row$evidence_text
    }

    # Concatenate all media URLs first as evidence.
    media <- vapply(row$evidence_media_url, function(url) {
      paste0(
        '<a class="evidence-media-link" href="',
        escape_html(url),
        '" target="_blank" rel="noopener">',
        '<img src="',
        escape_html(url),
        '" alt="Evidence ',
        escape_html(evidence_text),
        '">',
        "</a>"
      )
    }, character(1))

    # Concatenate all generic URLs second as evidence.
    links <- vapply(row$evidence_url, function(url) {
      paste0(
        '<a class="evidence-link" href="',
        escape_html(url),
        '" target="_blank" rel="noopener">',
        escape_html(url),
        "</a>"
      )
    }, character(1))

    # Concatenate all URLs as evidence.
    evidence <- paste0(
      '<td class="evidence">',
      paste(media, collapse = ""),
      paste(links, collapse = ""),
      '<div class="media-id">',
      escape_html(evidence_text),
      "</div>",
      "</td>"
    )

    # Represent missing alternative values as empty editable fields.
    alternative_label <- if (is.na(row$alternative_label)) {
      ""
    } else {
      row$alternative_label
    }

    alternative_description <- if (
      is.na(row$alternative_description)
    ) {
      ""
    } else {
      row$alternative_description
    }

    # Render alternatives and preserve their original candidate values.
    alternative_cells <- paste0(
      if (has_alternative_label) {
        paste0(
          '<td class="label">',
          '<input class="text-input" data-field="alternative_label" value="',
          escape_html(alternative_label),
          '" data-candidate="',
          escape_html(alternative_label),
          '">',
          "</td>"
        )
      } else {
        ""
      },
      if (has_alternative_description) {
        paste0(
          '<td class="description"><textarea data-field="alternative_description" data-candidate="',
          escape_html(alternative_description),
          '">',
          escape_html(alternative_description),
          "</textarea></td>"
        )
      } else {
        ""
      }
    )

    # Render an optional reviewer comment field for this row.
    row_comment_cell <- if (row_comment) {
      paste0(
        '<td class="row-comment">',
        '<textarea placeholder="Optional comment on row"></textarea>',
        "</td>"
      )
    } else {
      ""
    }

    # Assemble the complete review row.
    paste0(
      '<tr data-row="', row$row_number,
      '" data-finalised="false" data-outcome="accept">',
      '<td class="num">', row$row_number, "</td>",
      evidence,

      # Preserve the original label as the candidate value.
      '<td class="label">',
      '<input class="text-input" data-field="label" value="',
      escape_html(row$label),
      '" data-candidate="',
      escape_html(row$label),
      '">',
      "</td>",

      # Preserve the original description as the candidate value.
      '<td class="description"><textarea data-field="description" data-candidate="',
      escape_html(row$description),
      '">',
      escape_html(row$description),
      "</textarea></td>",
      alternative_cells,
      paste(assertions, collapse = ""),

      # Row-level finalisation control.
      '<td class="finalise-cell">',
      '<label class="finalise-control">',
      '<input type="checkbox" class="finalise-check">',
      '<span class="finalise-mark"></span>',
      "</label>",
      "</td>",
      row_comment_cell,
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
    "</tbody>",
    "</table>"
  )
}
