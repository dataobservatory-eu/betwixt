## code to prepare `small_countries_dataset` dataset goes here

small_countries_dataset <- data.frame(
  row_number = 1:4,
  evidence_url = rep(
    "https://doi.org/10.2908/NAIDA_10_GDP", 4
  ),
  label = c(
    "Liechtenstein 2023", "Liechtenstein 2024",
    "Malta 2023", "Malta 2024"
  ),
  subject = rep(c("Liechtenstein", "Malta"), each = 2),
  subject_definition = rep(c(
    "https://www.geonames.org/countries/LI/",
    "https://www.geonames.org/countries/MT/"
  ), each = 2),
  country_code = rep(c("LI", "MT"), each = 2),
  country_code_definition =
    "https://www.iso.org/iso-3166-country-codes.html",
  year = rep(c(2023L, 2024L), 2),
  gdp = c(NA_real_, NA_real_, NA_real_, NA_real_),
  gdp_definition =
    "http://dd.eionet.europa.eu/vocabulary/eurostat/na_item/B1GQ",
  unit = "CP_MEUR"
)

usethis::use_data(small_countries_dataset, overwrite = TRUE)
