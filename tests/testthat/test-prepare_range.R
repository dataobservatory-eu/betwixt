test_that("prepare_range returns an empty list for NULL", {
  result <- prepare_range(
    range = NULL,
    type = "value"
  )

  expect_identical(
    result,
    list()
  )
})

test_that("prepare_range selects a range type", {
  result <- prepare_range(
    range = delini_range,
    type = "subject"
  )

  expect_true(
    length(result) > 0
  )

  expect_true(
    all(vapply(
      result,
      function(x) is.list(x),
      logical(1)
    ))
  )
})

test_that("prepare_range orders values by rank", {
  range <- data.frame(
    type = c(
      "value",
      "value"
    ),
    rank = c(
      2,
      1
    ),
    label = c(
      "second",
      "first"
    ),
    namespace = NA_character_,
    url = NA_character_
  )

  result <- prepare_range(
    range = range,
    type = "value"
  )

  labels <- vapply(
    result,
    function(x) x$label,
    character(1)
  )

  expect_identical(
    labels,
    c(
      "first",
      "second"
    )
  )
})

test_that("prepare_range constructs display labels", {
  range <- data.frame(
    type = "value",
    rank = 1,
    label = "farmhouse",
    namespace = "AAT",
    url = "http://vocab.getty.edu/page/aat/300005574"
  )

  result <- prepare_range(
    range = range,
    type = "value"
  )

  expect_identical(
    result[[1]]$display_label,
    "AAT:farmhouse"
  )

  expect_true(
    result[[1]]$has_url
  )
})

test_that("prepare_range recognises other", {
  range <- data.frame(
    type = "value",
    rank = 99,
    label = "other",
    namespace = NA_character_,
    url = NA_character_
  )

  result <- prepare_range(
    range = range,
    type = "value"
  )

  expect_true(
    result[[1]]$is_other
  )

  expect_identical(
    result[[1]]$display_label,
    "other"
  )
})

test_that("prepare_range selects a wide review field", {
  range <- data.frame(
    type = c(
      "value",
      "value",
      "value"
    ),
    field = c(
      "instance_of",
      "instance_of",
      "inventory_number"
    ),
    rank = c(
      1,
      2,
      1
    ),
    label = c(
      "farmhouse",
      "image",
      "pn 8246"
    ),
    namespace = c(
      "AAT",
      "DCMI",
      NA_character_
    ),
    url = c(
      "http://vocab.getty.edu/page/aat/300005574",
      "http://purl.org/dc/dcmitype/Image",
      NA_character_
    )
  )

  result <- prepare_range(
    range = range,
    type = "value",
    field = "instance_of"
  )

  labels <- vapply(
    result,
    function(x) x$label,
    character(1)
  )

  expect_length(
    result,
    2
  )

  expect_identical(
    labels,
    c(
      "farmhouse",
      "image"
    )
  )
})

test_that("prepare_range returns empty list for unknown field", {
  range <- data.frame(
    type = "value",
    field = "instance_of",
    rank = 1,
    label = "farmhouse",
    namespace = "AAT",
    url = NA_character_
  )

  result <- prepare_range(
    range = range,
    type = "value",
    field = "inventory_number"
  )

  expect_identical(
    result,
    list()
  )
})


test_that("prepare_range excludes the current value", {
  range <- data.frame(
    type = c(
      "value",
      "value",
      "value"
    ),
    rank = c(
      1,
      2,
      3
    ),
    label = c(
      "farmhouse",
      "image",
      "other"
    ),
    namespace = c(
      "AAT",
      "DCMI",
      NA_character_
    ),
    url = c(
      "http://vocab.getty.edu/page/aat/300005574",
      "http://purl.org/dc/dcmitype/Image",
      NA_character_
    )
  )

  result <- prepare_range(
    range = range,
    type = "value",
    current = "farmhouse"
  )

  labels <- vapply(
    result,
    function(x) x$label,
    character(1)
  )

  expect_identical(
    labels,
    c(
      "image",
      "other"
    )
  )
})

test_that("prepare_range excludes current value within a wide field", {
  range <- data.frame(
    type = c(
      "value",
      "value",
      "value"
    ),
    field = c(
      "instance_of",
      "instance_of",
      "inventory_number"
    ),
    rank = c(
      1,
      2,
      1
    ),
    label = c(
      "farmhouse",
      "image",
      "farmhouse"
    ),
    namespace = c(
      "AAT",
      "DCMI",
      NA_character_
    ),
    url = NA_character_
  )

  result <- prepare_range(
    range = range,
    type = "value",
    field = "instance_of",
    current = "farmhouse"
  )

  labels <- vapply(
    result,
    function(x) x$label,
    character(1)
  )

  expect_identical(
    labels,
    "image"
  )
})
