## code to prepare `w3c_life_expectancy` dataset goes here

w3c_life_expectancy <- tibble::tribble(
  ~observation,
  ~area,
  ~period,
  ~sex,
  ~life_expectancy,
  "eg:o11", "Newport",         "2004-2006", "Male", 76.7,
  "eg:o12", "Cardiff",         "2004-2006", "Male", 78.7,
  "eg:o13", "Monmouthshire",   "2004-2006", "Male", 76.6,
  "eg:o14", "Merthyr Tydfil",  "2004-2006", "Male", 75.5
)


usethis::use_data(
  w3c_life_expectancy,
  overwrite = TRUE
)
