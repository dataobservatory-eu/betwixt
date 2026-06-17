test_that("claim creates a one-row claim_df", {

  ad_gdp <- claim(
    scope = "country=AD;year=2023",
    subject = "country",
    predicate = "GDP",
    value = "3.73 billion EUR"
  )

  expect_s3_class(ad_gdp, "claim_df")
  expect_s3_class(ad_gdp, "tbl_df")

  expect_equal(dim(ad_gdp), c(1, 4))

  expect_named(
    ad_gdp,
    c(
      "scope",
      "subject",
      "predicate",
      "value"
    )
  )

  expect_equal(ad_gdp$scope,"country=AD;year=2023")

  expect_equal(ad_gdp$subject, "country")

  expect_equal(ad_gdp$predicate, "GDP")

  expect_equal(ad_gdp$value, "3.73 billion EUR")
})
