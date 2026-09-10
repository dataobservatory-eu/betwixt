## code to prepare `small_countries_dataset` dataset goes here

small_countries_dataset <- data.frame(
  row_number = 1:4,
  evidence_url = rep(
    "https://doi.org/10.2908/NAIDA_10_GDP", 4
  ),
  label = c(
    "Iceland 2023", "Iceland 2024",
    "Malta 2023", "Malta 2024"
  ),
  description = c(
    "GDP at market prices in Iceland in the year 2023",
    "GDP at market prices in Iceland in the year 2024",
    "GDP at market prices in Malta in the year 2023",
    "GDP at market prices in Malta in the year 2024"
    ),
  subject = rep(c("Iceland", "Malta"), each = 2),
  subject_definition = rep(c(
    "https://www.geonames.org/countries/IS/",
    "https://www.geonames.org/countries/MT/"
  ), each = 2),
  country_code = rep(c("IS", "MT"), each = 2),
  country_code_definition =
    "https://www.iso.org/iso-3166-country-codes.html",
  gdp = c(139.157,	140.144, 139.157, 140.144),
  gdp_definition =
    "http://dd.eionet.europa.eu/vocabulary/eurostat/na_item/B1GQ",
  context_year = rep(c(2023L, 2024L), 2),
  context_unit = "CP_MEUR"
)

usethis::use_data(small_countries_dataset, overwrite = TRUE)
