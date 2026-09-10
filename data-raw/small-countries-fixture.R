devtools::load_all()

# -------------------------------------------------------------------------
# Candidate dataset
# -------------------------------------------------------------------------

candidates <- create_candidate_dataset(
  evidence_url = rep("https://doi.org/10.2908/NAIDA_10_GDP", 6),
  label = c(
    "Iceland 2023", "Iceland 2024",
    "Malta 2023", "Malta 2024",
    "Luxembourg 2023", "Luxembourg 2024"
  ),
  description = c(
    "GDP at market prices in Iceland in the year 2023",
    "GDP at market prices in Iceland in the year 2024",
    "GDP at market prices in Malta in the year 2023",
    "GDP at market prices in Malta in the year 2024",
    "GDP at market prices in Luxembourg in the year 2023",
    "GDP at market prices in Luxembourg in the year 2024"
  ),
  subject = c(
    rep("Iceland", 2),
    rep("Malta", 2),
    rep("Luxembourg", 2)
  ),
  data_manager_name = "Daniel Antal",
  data_manager_iri = "https://orcid.org/0000-0001-7513-6760",
  data_manager_email = "daniel@example.org",
  project_id = "GDP_review"
) |>
  add_candidate_column(
    name = "country_code",
    value = c(rep("IS", 2), rep("MT", 2), rep("LU", 2)),
    definition = rep(
      "https://www.iso.org/iso-3166-country-codes.html", 6
    )
  ) |>
  add_candidate_column(
    name = "gdp",
    value = c(
      31484.4, 33156.8,
      21951.6, 23632.2,
      82115.5, 86180.3
    ),
    definition = rep(
      paste0(
        "http://dd.eionet.europa.eu/vocabulary/",
        "eurostat/na_item/B1GQ"
      ),
      6
    )
  ) |>
  dplyr::mutate(
    context_year = rep(c(2023L, 2024L), 3),
    context_unit = "CP_MEUR"
  )

# -------------------------------------------------------------------------
# Candidate definitions
# -------------------------------------------------------------------------

candidates$subject_definition <- c(
  rep("https://www.geonames.org/countries/IS/", 2),
  rep("https://www.geonames.org/countries/MT/", 2),
  rep("https://www.geonames.org/countries/LU/", 2)
)

validate_candidate_dataset(candidates)

# -------------------------------------------------------------------------
# Render fixture
# -------------------------------------------------------------------------

render_review(
  candidates,
  row_comment = TRUE,
  review_comment = TRUE,
  filename_stem = "small_countries_dataset_extended",
  reviewer_name = "Jane Doe",
  reviewer_iri = "https://orcid.org/0000-0002-1825-0097",
  project_id = "GDP_review",
  sequence = 0L,
  path = here::here("inst", "examples")
)

optional_copy <- function() {
  file.copy(
    from = here::here(
      "inst", "examples",
      "small_countries_dataset_extended.html"
    ),
    to = here::here(
      "tests", "testthat", "fixtures",
      "small_countries_dataset_extended.html"
    ),
    overwrite = TRUE
  )
}
