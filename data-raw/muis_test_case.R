devtools::load_all()
library(tibble)

# -------------------------------------------------------------------
# Input data
# -------------------------------------------------------------------

review_input <- tibble::tribble(
  ~page_id, ~label_en, ~description_en, ~label_hu, ~description_hu,
  ~page_url, ~thumbnail_url,
  ~subject, ~predicate, ~value, ~value_definition,
  "635780",
  "sweater, women's",
  "MuIS museum record 635780.",
  "női kardigán", # reviewer should change to pulóver
  "A MuIS 635780 számú múzeumi rekordja.",
  "https://www.muis.ee/museaalview/635780",
  paste0(
    "https://www.muis.ee/digitaalhoidla/api/meedia/pisipilt?",
    "id=ebc07930-f719-44f2-a108-6698bcecc20b"
  ),
  paste0(
    "https://www.muis.ee/digitaalhoidla/api/meedia/pisipilt?",
    "id=ebc07930-f719-44f2-a108-6698bcecc20b"
  ),
  "depicts",
  "300209900",
  "http://vocab.getty.edu/page/aat/300209900",
  "633053",
  "gloves",
  "MuIS museum record 633053.",
  "kesztyű",
  "A MuIS 633053 számú múzeumi rekordja.",
  "https://www.muis.ee/museaalview/633053",
  paste0(
    "https://www.muis.ee/digitaalhoidla/api/meedia/pisipilt?",
    "id=6440f24f-eaad-4cd4-84d7-1ae7a9d44d5a"
  ),
  paste0(
    "https://www.muis.ee/digitaalhoidla/api/meedia/pisipilt?",
    "id=6440f24f-eaad-4cd4-84d7-1ae7a9d44d5a"
  ),
  "depicts",
  "300148821",
  "http://vocab.getty.edu/page/aat/300148821",
  "635778",
  "shirt, women's",
  "MuIS museum record 635778.",
  "női ing",
  "A MuIS 635778 számú múzeumi rekordja.",
  "https://www.muis.ee/museaalview/635778",
  paste0(
    "https://www.muis.ee/digitaalhoidla/api/meedia/pisipilt?",
    "id=826c402e-c130-4860-b11e-9538bd403ecf"
  ),
  paste0(
    "https://www.muis.ee/digitaalhoidla/api/meedia/pisipilt?",
    "id=826c402e-c130-4860-b11e-9538bd403ecf"
  ),
  "depicts",
  "300212499",
  "http://vocab.getty.edu/page/aat/300212499"
)

# sweaters: http://vocab.getty.edu/page/aat/300209900
# shirts: http://vocab.getty.edu/page/aat/300212499
# gloves: http://vocab.getty.edu/page/aat/300148821

# -------------------------------------------------------------------
# Construct candidate dataset
# -------------------------------------------------------------------

candidates <- candidate_dataset(
  evidence_media_url = review_input$thumbnail_url,
  evidence_url = review_input$page_url,
  evidence_text = review_input$label_en,
  label = review_input$label_en,
  description = review_input$description_en,
  alternative_label = review_input$label_hu,
  alternative_description = review_input$description_hu,
  subject = review_input$subject
) |>
  add_candidate_column(
    name = "predicate",
    value = review_input$predicate
  ) |>
  add_candidate_column(
    name = "value",
    value = review_input$value,
    definition = review_input$value_definition
  )

# -------------------------------------------------------------------
# Render current wide review
# -------------------------------------------------------------------

betwixt_render(
  candidates,
  cols = c(
    subject = "Digital image",
    predicate = "Relation",
    value = "AAT concept"
  ),
  title = "MuIS garment terminology review",
  description = paste(
    "Review the proposed Hungarian labels and verify that the",
    "Getty AAT concepts correctly identify the depicted garments."
  ),
  row_comment = TRUE,
  review_comment = TRUE,
  project_id = "muis-garments",
  filename_stem = "muis-garments-review",
  sequence = 0L,
  path = "."
)
