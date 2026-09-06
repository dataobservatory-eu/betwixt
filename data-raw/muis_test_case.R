devtools::load_all()
library(tibble)

# -------------------------------------------------------------------
# Input data
# -------------------------------------------------------------------

review_input <- tibble::tribble(
  ~page_id, ~title, ~page_url, ~thumbnail_url,
  ~subject, ~predicate, ~value,
  "635780",
  "sweater, women's",
  "https://www.muis.ee/museaalview/635780",
  "https://www.muis.ee/digitaalhoidla/api/meedia/pisipilt?id=ebc07930-f719-44f2-a108-6698bcecc20b",
  "[image shown]",
  "depicts",
  "sweaters",
  "633053",
  "gloves",
  "https://www.muis.ee/museaalview/633053",
  "https://www.muis.ee/digitaalhoidla/api/meedia/pisipilt?id=6440f24f-eaad-4cd4-84d7-1ae7a9d44d5a",
  "[image shown]",
  "depicts",
  "gloves",
  "635778",
  "shirt, women's",
  "https://www.muis.ee/museaalview/635778",
  "https://www.muis.ee/digitaalhoidla/api/meedia/pisipilt?id=826c402e-c130-4860-b11e-9538bd403ecf",
  "[image shown]",
  "depicts",
  "shirts"
)


# -------------------------------------------------------------------
# Construct candidate dataset
# -------------------------------------------------------------------

candidates <- candidate_dataset(
  evidence_media_url = review_input$thumbnail_url,
  evidence_url = review_input$page_url,
  evidence_text = review_input$title,
  label = review_input$title,
  description = paste(
    "MuIS museum record",
    review_input$page_id
  ),
  subject = review_input$subject
)

candidates <- candidates |>
  add_candidate_column(
    value = review_input$predicate
  ) |>
  add_candidate_column(
    value = review_input$value
  ) |>
  add_candidate_column(
    value = review_input$page_url
  )

# Inspect the intermediate representation.
print(candidates)


# -------------------------------------------------------------------
# Render current wide review
# -------------------------------------------------------------------

betwixt_render(
  candidates,
  cols = c(
    col_1 = "Subject",
    col_2 = "Predicate",
    col_3 = "Value",
    col_4 = "Access point"
  ),
  title = "MuIS garment review",
  description = paste(
    "Review the candidate semantic claims derived from",
    "the museum records."
  ),
  project_id = "muis-garments",
  filename_stem = "muis-garments-review",
  sequence = 0L,
  path = "."
)
