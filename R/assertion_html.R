#' Render one reviewable assertion
#'
#' @param assertion A single assertion from a prepared review context.
#'
#' @return A character string containing HTML.
#'
#' @keywords internal
#' @noRd
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

      option_value <- if (identical(option, "Other\u2026")) {
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
