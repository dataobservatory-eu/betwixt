test_that("betwixt_representation identifies wide representation", {
  x <- data.frame(
    row_id = 1:2,
    subject = c("Q1", "Q2"),
    instance_of = c("human", "human"),
    occupation = c("composer", "pianist")
  )

  expect_identical(betwixt_representation(x), "wide")
})

test_that("betwixt_representation identifies dual-wide representation", {
  x <- data.frame(
    row_id = 1:2,
    input_url = c("a.jpg", "b.jpg"),
    subject = c("Q1", "Q2"),
    instance_of = c("human", "human"),
    occupation = c("composer", "pianist")
  )

  expect_identical(betwixt_representation(x), "dual_wide")
})

test_that("betwixt_representation identifies long representation", {
  x <- data.frame(
    row_id = 1:2,
    subject = c("Q1", "Q2"),
    predicate = c("instance_of", "occupation"),
    value = c("human", "composer")
  )

  expect_identical(betwixt_representation(x), "long")
})

test_that("betwixt_representation identifies dual-long representation", {
  x <- data.frame(
    row_id = 1:2,
    input_url = c("a.jpg", "b.jpg"),
    subject = c("Q1", "Q2"),
    predicate = c("instance_of", "occupation"),
    value = c("human", "composer")
  )

  expect_identical(betwixt_representation(x), "dual_long")
})

test_that("predicate and value may be wide assertion columns", {
  x <- data.frame(
    row_id = 1:2,
    subject = c("Q1", "Q2"),
    predicate = c("P1", "P2"),
    value = c("V1", "V2"),
    instance_of = c("human", "human")
  )

  expect_identical(betwixt_representation(x), "wide")
})

test_that("input columns make a representation dual", {
  wide <- data.frame(
    row_id = 1L,
    subject = "Q1",
    instance_of = "human",
    occupation = "composer"
  )

  long <- data.frame(
    row_id = 1L,
    subject = "Q1",
    predicate = "instance_of",
    value = "human"
  )

  wide$input_label <- "Portrait"
  long$input_label <- "Portrait"

  expect_identical(betwixt_representation(wide), "dual_wide")
  expect_identical(betwixt_representation(long), "dual_long")
})

test_that("structural columns do not make a representation wide", {
  x <- data.frame(
    row_id = 1L,
    subject = "Q1",
    predicate = "instance_of",
    value = "human",
    plane = "candidate",
    comment_review = "",
    subject_label = "Bartok"
  )

  expect_identical(betwixt_representation(x), "long")
})

test_that("missing row_id is invalid", {
  x <- data.frame(
    subject = "Q1",
    instance_of = "human",
    occupation = "composer"
  )

  expect_identical(betwixt_representation(x), "invalid")
})

test_that("missing subject is invalid", {
  x <- data.frame(
    row_id = 1L,
    instance_of = "human",
    occupation = "composer"
  )

  expect_identical(betwixt_representation(x), "invalid")
})

test_that("insufficient assertion structure is invalid", {
  x <- data.frame(
    row_id = 1L,
    subject = "Q1"
  )

  expect_identical(betwixt_representation(x), "invalid")
})

test_that("non-data frames are invalid", {
  expect_identical(betwixt_representation(NULL), "invalid")
  expect_identical(betwixt_representation(list()), "invalid")
  expect_identical(betwixt_representation(matrix(1:4, 2)), "invalid")
})
