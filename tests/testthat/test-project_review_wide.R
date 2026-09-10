# Fixtures ------------------------------------------------------------------

muis_initial_path <- test_path(
  "fixtures",
  "muis-garments-review.html"
)

countries_draft_path <- test_path(
  "fixtures",
  "small_countries_dataset_extended_1-draft.html"
)

countries_final_path <- test_path(
  "fixtures",
  "small_countries_dataset_extended_1-finalised.html"
)

muis_draft_path <- test_path(
  "fixtures",
  "muis-garments-review_1-draft.html"
)

muis_final_path <- test_path(
  "fixtures",
  "muis-garments-review_1-finalised.html"
)

# Status planes -------------------------------------------------------------

test_that("wide projection preserves explicit qualifications", {
  wide <- read_review(countries_draft_path) |>
    project_review_wide()

  status <- dplyr::filter(wide, plane == "status")

  expect_equal(status$gdp[1:2], rep("deferred", 2))
  expect_equal(status$subject[1:2], rep("pending", 2))
  expect_equal(status$country_code[1:2], rep("pending", 2))
  expect_equal(status$gdp[3:6], rep("corroborated", 4))
  expect_equal(status$subject[3:6], rep("corroborated", 4))
})

test_that("wide projection derives finalised review states", {
  wide <- read_review(countries_final_path) |>
    project_review_wide()

  status <- dplyr::filter(wide, plane == "status")

  expect_equal(status$gdp[1], "rejected")
  expect_equal(status$gdp[2:6], rep("corroborated", 5))
  expect_equal(status$subject, rep("corroborated", 6))
  expect_equal(status$country_code, rep("corroborated", 6))
})


# Initial review ------------------------------------------------------------

test_that("project_review_wide() keeps initial assertions pending", {
  x <- project_review_wide(read_review(muis_initial_path))
  status <- dplyr::filter(x, plane == "status")

  expect_equal(status$label, rep("pending", 3))
  expect_equal(status$alternative_label, rep("pending", 3))
  expect_equal(status$subject, rep("pending", 3))
  expect_equal(status$depicts, rep("pending", 3))
})


# Draft review --------------------------------------------------------------

test_that("project_review_wide() preserves MuIS qualifications", {
  x <- project_review_wide(read_review(muis_draft_path))
  status <- dplyr::filter(x, plane == "status")

  expect_equal(status$depicts, c("deferred", "rejected", "corroborated"))
})


# Finalised review ----------------------------------------------------------

test_that("project_review_wide() derives MuIS finalised states", {
  x <- project_review_wide(read_review(muis_final_path))

  candidate <- dplyr::filter(x, plane == "candidate")
  reviewed <- dplyr::filter(x, plane == "reviewed")
  status <- dplyr::filter(x, plane == "status")

  expect_equal(
    status$alternative_label,
    c("corrected", "corroborated", "corroborated")
  )
  expect_equal(status$depicts, rep("corroborated", 3))

  expect_equal(candidate$alternative_label[1], "női kardigán")
  expect_equal(reviewed$alternative_label[1], "női pulóver")
  expect_equal(status$alternative_label[1], "corrected")
})


# Descriptive assertions ---------------------------------------------------

test_that("project_review_wide() derives descriptive review states", {
  review <- list(
    candidate = data.frame(
      row_number = 1:4,
      description = c(NA, "Delete", "Change", "Keep")
    ),
    reviewed = data.frame(
      row_number = 1:4,
      description = c(NA, "", "Changed", "Keep"),
      finalised = rep(TRUE, 4)
    )
  )

  x <- project_review_wide(review)
  status <- dplyr::filter(x, plane == "status")

  expect_equal(
    status$description,
    c("deferred", "rejected", "corrected", "corroborated")
  )
})


# Context ------------------------------------------------------------------

test_that("wide projection preserves row context", {
  wide <- project_review_wide(read_review(countries_draft_path))

  expect_equal(wide$context_year, rep(c("2023", "2024"), 9))
  expect_equal(wide$context_unit, rep("CP_MEUR", 18))
})


# Provenance ---------------------------------------------------------------

test_that("wide projection preserves review provenance", {
  review <- read_review(countries_draft_path)
  wide <- project_review_wide(review)

  expect_equal(attr(wide, "provenance"), review$provenance)
})


# Context ------------------------------------------------------------------

test_that("wide projection preserves row context unchanged", {
  wide <- project_review_wide(read_review(countries_draft_path))

  expect_equal(wide$context_year, rep(c("2023", "2024"), 9))
  expect_equal(wide$context_unit, rep("CP_MEUR", 18))

  status <- dplyr::filter(wide, plane == "status")
  expect_equal(status$context_year, c(
    "2023", "2024", "2023", "2024",
    "2023", "2024"
  ))
  expect_equal(status$context_unit, rep("CP_MEUR", 6))
})
