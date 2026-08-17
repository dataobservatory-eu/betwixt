# Count literal occurrences of a pattern in rendered HTML.
count_matches <- function(html, pattern) {
  matches <- gregexpr(
    pattern,
    html,
    fixed = TRUE
  )[[1]]

  if (identical(matches, -1L)) {
    return(0L)
  }

  length(matches)
}

# Wide review -------------------------------------------------------------

test_that("wide review returns HTML", {
  html <- betwixt_render(
    claim = delini_review,
    range = delini_range,
    evidence_media_file = "thumbnail_url",
    evidence_url = "evidence_url",
    descriptive = c(
      "label_en", "description_en",
      "label_hu", "description_hu",
      "inventory_number"
    ),
    assertions = "instance_of",
    context = c(
      "evidence_type",
      "held_by"
    ),
    template = "wide_review"
  )

  # Rendering returns one complete HTML document.
  expect_type(html, "character")
  expect_length(html, 1)
})


test_that("wide review renders selected descriptive metadata", {
  html <- betwixt_render(
    claim = delini_review,
    descriptive = c(
      "label_en", "description_en",
      "label_hu", "description_hu",
      "inventory_number"
    ),
    assertions = "instance_of",
    template = "wide_review"
  )

  # Explicitly selected descriptive fields are displayed.
  expect_match(html, "farmhouse (LEBM)", fixed = TRUE)
  expect_match(html, "lakóépület (LEBM)", fixed = TRUE)
  expect_match(html, "a Delini tanya lakóépülete", fixed = TRUE)
  expect_match(html, "pn 8246", fixed = TRUE)
})


test_that("wide review renders selected evidence media", {
  html <- betwixt_render(
    claim = delini_review,
    evidence_media_file = "thumbnail_url",
    assertions = "instance_of",
    template = "wide_review"
  )

  # The selected media column supplies the evidence resource.
  expect_match(
    html,
    "P7101556_thumbnail.jpg",
    fixed = TRUE
  )
})


test_that("wide review renders evidence URL when supplied", {
  html <- betwixt_render(
    claim = delini_review,
    evidence_url = "evidence_url",
    assertions = "instance_of",
    template = "wide_review"
  )

  # A supplied evidence URL is rendered as a source link.
  expect_match(
    html,
    "https://example.com/delini.html",
    fixed = TRUE
  )
})


test_that("wide review omits missing evidence URL", {
  claim <- delini_review
  claim$evidence_url <- NA_character_

  html <- betwixt_render(
    claim = claim,
    evidence_url = "evidence_url",
    assertions = "instance_of",
    template = "wide_review"
  )

  # Missing evidence URLs do not produce source links.
  expect_false(grepl(
    "https://example.com/delini.html",
    html,
    fixed = TRUE
  ))
})


test_that("wide review applies field-specific value range", {
  html <- betwixt_render(
    claim = delini_review,
    range = delini_range,
    assertions = "instance_of",
    template = "wide_review"
  )

  # A wide assertion column receives its matching value range.
  expect_match(
    html,
    "AAT:farmhouse",
    fixed = TRUE
  )
})


test_that("wide review renders only selected assertions as reviewable", {
  claim <- delini_review[1, , drop = FALSE]

  html <- betwixt_render(
    claim = claim,
    range = delini_range,
    descriptive = "inventory_number",
    assertions = "instance_of",
    template = "wide_review"
  )

  # Assertion columns receive review controls.
  expect_match(
    html,
    'name="instance_of_selected_1"',
    fixed = TRUE
  )

  # Descriptive columns do not receive review controls.
  expect_false(grepl(
    "inventory_number_selected_1",
    html,
    fixed = TRUE
  ))
})


test_that("wide review renders selected context without review controls", {
  claim <- delini_review[1, , drop = FALSE]

  html <- betwixt_render(
    claim = claim,
    assertions = "instance_of",
    context = c(
      "evidence_type",
      "held_by"
    ),
    template = "wide_review"
  )

  # Contextual fields are displayed.
  expect_match(html, "evidence_type", fixed = TRUE)
  expect_match(html, "held_by", fixed = TRUE)

  # Contextual fields do not receive review controls.
  expect_false(grepl(
    "evidence_type_selected_1",
    html,
    fixed = TRUE
  ))

  expect_false(grepl(
    "held_by_selected_1",
    html,
    fixed = TRUE
  ))
})


test_that("wide review renders missing description as editable empty value", {
  claim <- delini_review[2, , drop = FALSE]

  html <- betwixt_render(
    claim = claim,
    descriptive = "description_hu",
    assertions = "instance_of",
    template = "wide_review"
  )

  # Missing descriptive metadata remain editable.
  expect_match(
    html,
    'name="description_hu_1"',
    fixed = TRUE
  )
})


test_that("wide review accepts no descriptive metadata", {
  html <- betwixt_render(
    claim = delini_review,
    assertions = "instance_of",
    template = "wide_review"
  )

  # Descriptive metadata are optional.
  expect_type(html, "character")
  expect_length(html, 1)
})


test_that("wide review accepts no evidence", {
  html <- betwixt_render(
    claim = delini_review,
    assertions = "instance_of",
    template = "wide_review"
  )

  # Evidence is optional.
  expect_type(html, "character")
  expect_length(html, 1)
})


test_that("wide review accepts no range", {
  html <- betwixt_render(
    claim = delini_review,
    range = NULL,
    assertions = "instance_of",
    template = "wide_review"
  )

  # Assertions remain reviewable without a controlled range.
  expect_type(html, "character")
  expect_length(html, 1)
  expect_match(html, "farmhouse", fixed = TRUE)
})


# Long review -------------------------------------------------------------

test_that("long review renders selected assertion columns", {
  claim <- data.frame(
    subject = "image_001",
    predicate = "depicts",
    value = "farmhouse"
  )

  html <- betwixt_render(
    claim = claim,
    assertions = c(
      "subject",
      "predicate",
      "value"
    ),
    template = "long_review"
  )

  # Long review exposes each selected assertion component.
  expect_match(html, "image_001", fixed = TRUE)
  expect_match(html, "depicts", fixed = TRUE)
  expect_match(html, "farmhouse", fixed = TRUE)
})


test_that("long review allows an implicit subject", {
  claim <- data.frame(
    media = "farmhouse.jpg",
    predicate = "depicts",
    value = "farmhouse"
  )

  html <- betwixt_render(
    claim = claim,
    evidence_media_file = "media",
    assertions = c(
      "predicate",
      "value"
    ),
    template = "long_review"
  )

  # Evidence may supply the implicit subject of the assertion.
  expect_match(html, "farmhouse.jpg", fixed = TRUE)
  expect_match(html, "depicts", fixed = TRUE)
  expect_match(html, "farmhouse", fixed = TRUE)
})


test_that("long review allows multiple value columns", {
  claim <- data.frame(
    predicate = "date",
    value1 = "1923",
    value2 = "1924"
  )

  html <- betwixt_render(
    claim = claim,
    assertions = c(
      "predicate",
      "value1",
      "value2"
    ),
    template = "long_review"
  )

  # Multiple value components may be exposed in one long review.
  expect_match(html, "1923", fixed = TRUE)
  expect_match(html, "1924", fixed = TRUE)
})


test_that("long review applies subject range", {
  claim <- data.frame(
    subject = "[image shown]",
    predicate = "depicts",
    value = "farmhouse"
  )

  html <- betwixt_render(
    claim = claim,
    range = delini_range,
    assertions = c(
      "subject",
      "predicate",
      "value"
    ),
    template = "long_review"
  )

  # Subject components use the subject range.
  expect_match(
    html,
    'name="subject_selected_1"',
    fixed = TRUE
  )

  expect_match(
    html,
    "[image file]",
    fixed = TRUE
  )
})


test_that("long review applies predicate range", {
  claim <- data.frame(
    subject = "[image shown]",
    predicate = "instance of",
    value = "farmhouse"
  )

  html <- betwixt_render(
    claim = claim,
    range = delini_range,
    assertions = c(
      "subject",
      "predicate",
      "value"
    ),
    template = "long_review"
  )

  # Predicate components use the predicate range.
  expect_match(
    html,
    'name="predicate_selected_1"',
    fixed = TRUE
  )

  expect_match(
    html,
    "Wikidata:depicts",
    fixed = TRUE
  )
})


test_that("long review applies value range", {
  claim <- data.frame(
    subject = "[image shown]",
    predicate = "instance of",
    value = "farmhouse"
  )

  html <- betwixt_render(
    claim = claim,
    range = delini_range,
    assertions = c(
      "subject",
      "predicate",
      "value"
    ),
    template = "long_review"
  )

  # Value components use the value range.
  expect_match(
    html,
    'name="value_selected_1"',
    fixed = TRUE
  )

  expect_match(
    html,
    "tablet-woven sash",
    fixed = TRUE
  )
})


test_that("long review applies value range to numbered values", {
  claim <- data.frame(
    predicate = "depicts",
    value1 = "farmhouse",
    value2 = "bed"
  )

  html <- betwixt_render(
    claim = claim,
    range = delini_range,
    assertions = c(
      "predicate",
      "value1",
      "value2"
    ),
    template = "long_review"
  )

  # Numbered value components share value-range semantics.
  expect_match(
    html,
    'name="value1_selected_1"',
    fixed = TRUE
  )

  expect_match(
    html,
    'name="value2_selected_1"',
    fixed = TRUE
  )
})


test_that("long review renders other as an open proposal", {
  claim <- data.frame(
    predicate = "instance of",
    value = "farmhouse"
  )

  html <- betwixt_render(
    claim = claim,
    range = delini_range,
    assertions = c(
      "predicate",
      "value"
    ),
    template = "long_review"
  )

  # The reserved other range entry becomes an interface action.
  expect_match(
    html,
    '<option value="__other__"',
    fixed = TRUE
  )

  expect_match(
    html,
    "Other proposal...",
    fixed = TRUE
  )
})


# Validation --------------------------------------------------------------

test_that("renderer requires a data frame", {
  # Non-tabular claim input is rejected.
  expect_error(
    betwixt_render(
      claim = list(value = "farmhouse"),
      assertions = "value"
    ),
    "claim must be a data frame.",
    fixed = TRUE
  )
})


test_that("renderer rejects unknown template", {
  # Only the two supported review projections are accepted.
  expect_error(
    betwixt_render(
      claim = delini_review,
      assertions = "instance_of",
      template = "image_review"
    ),
    "Unknown template",
    fixed = TRUE
  )
})


test_that("renderer rejects unknown projection columns", {
  # Explicitly selected columns must exist in the input data.
  expect_error(
    betwixt_render(
      claim = delini_review,
      assertions = "does_not_exist",
      template = "wide_review"
    ),
    "Unknown column: does_not_exist",
    fixed = TRUE
  )
})


test_that("wide review preserves the Delini reference render", {
  html <- betwixt_render(
    claim = delini_review,
    range = delini_range,
    evidence_media_file = "thumbnail_url",
    evidence_url = "evidence_url",
    descriptive = c(
      "label_en",
      "description_en",
      "label_hu",
      "description_hu",
      "inventory_number"
    ),
    assertions = "instance_of",
    context = c(
      "evidence_type",
      "held_by"
    ),
    title = "Delini Farmstead Review",
    description = paste(
      "Review the proposed classification of each item.",
      "Use the evidence and contextual information to assess the proposed",
      "instance-of value, and correct the English and Hungarian descriptive",
      "metadata where necessary."
    ),
    betwixt_id = "delini-wide",
    template = "wide_review"
  )

  # The reference render contains its identifying metadata.
  expect_match(
    html,
    "Delini Farmstead Review",
    fixed = TRUE
  )
  expect_match(
    html,
    "delini-wide",
    fixed = TRUE
  )

  # The reference render contains all four interface blocks.
  expect_match(
    html,
    "P7101556_thumbnail.jpg",
    fixed = TRUE
  )
  expect_match(
    html,
    "farmhouse (LEBM)",
    fixed = TRUE
  )
  expect_match(
    html,
    "AAT:farmhouse",
    fixed = TRUE
  )
  expect_match(
    html,
    "evidence_type",
    fixed = TRUE
  )
  expect_match(
    html,
    "held_by",
    fixed = TRUE
  )
})


test_that("wide review writes the Delini reference render", {
  output <- tempfile(fileext = ".html")

  result <- betwixt_render(
    claim = delini_review,
    range = delini_range,
    evidence_media_file = "thumbnail_url",
    evidence_url = "evidence_url",
    descriptive = c(
      "label_en",
      "description_en",
      "label_hu",
      "description_hu",
      "inventory_number"
    ),
    assertions = "instance_of",
    context = c(
      "evidence_type",
      "held_by"
    ),
    title = "Delini Farmstead Review",
    description = paste(
      "Review the proposed classification of each item.",
      "Use the evidence and contextual information to assess the proposed",
      "instance-of value, and correct the English and Hungarian descriptive",
      "metadata where necessary."
    ),
    betwixt_id = "delini-wide",
    template = "wide_review",
    con = output
  )

  html <- paste(
    readLines(
      output,
      warn = FALSE,
      encoding = "UTF-8"
    ),
    collapse = "\n"
  )

  # File output returns the destination and writes the review.
  expect_identical(
    result,
    output
  )
  expect_true(
    file.exists(output)
  )
  expect_match(
    html,
    "Delini Farmstead Review",
    fixed = TRUE
  )
  expect_match(
    html,
    "AAT:farmhouse",
    fixed = TRUE
  )
})


test_that("long review does not duplicate current subject value", {
  claim <- data.frame(
    subject = "[image shown]",
    predicate = "instance of",
    value = "farmhouse"
  )

  html <- betwixt_render(
    claim = claim,
    range = delini_range,
    assertions = c(
      "subject",
      "predicate",
      "value"
    ),
    template = "long_review"
  )

  # The current subject occurs once as an option in its select control.
  expect_identical(
    count_matches(
      html,
      '<option value="[image shown]"'
    ),
    1L
  )
})


test_that("long review does not duplicate current predicate value", {
  claim <- data.frame(
    subject = "[image shown]",
    predicate = "instance of",
    value = "farmhouse"
  )

  html <- betwixt_render(
    claim = claim,
    range = delini_range,
    assertions = c(
      "subject",
      "predicate",
      "value"
    ),
    template = "long_review"
  )

  # The current predicate occurs once as an option in its select control.
  expect_identical(
    count_matches(
      html,
      '<option value="instance of"'
    ),
    1L
  )
})


test_that("long review does not duplicate current value", {
  claim <- data.frame(
    subject = "[image shown]",
    predicate = "instance of",
    value = "farmhouse"
  )

  html <- betwixt_render(
    claim = claim,
    range = delini_range,
    assertions = c(
      "subject",
      "predicate",
      "value"
    ),
    template = "long_review"
  )

  # The current value occurs once as an option in its select control.
  expect_identical(
    count_matches(
      html,
      '<option value="farmhouse"'
    ),
    1L
  )
})


test_that("wide review does not duplicate the current range value", {
  claim <- delini_review[1, , drop = FALSE]

  html <- betwixt_render(
    claim = claim,
    range = delini_range,
    assertions = "instance_of",
    template = "wide_review"
  )

  expect_identical(
    count_matches(
      html,
      '<option value="farmhouse"'
    ),
    1L
  )
})

test_that("long review does not duplicate the current range value", {
  claim <- data.frame(
    subject = "[image shown]",
    predicate = "instance of",
    value = "farmhouse"
  )

  range <- data.frame(
    type = c(
      "value",
      "value"
    ),
    rank = c(
      1,
      2
    ),
    label = c(
      "farmhouse",
      "building"
    ),
    namespace = c(
      "AAT",
      "AAT"
    ),
    url = c(
      "https://example.org/farmhouse",
      "https://example.org/building"
    )
  )

  html <- betwixt_render(
    claim = claim,
    range = range,
    assertions = c(
      "subject",
      "predicate",
      "value"
    ),
    template = "long_review"
  )

  expect_identical(
    count_matches(
      html,
      '<option value="farmhouse"'
    ),
    1L
  )
})
