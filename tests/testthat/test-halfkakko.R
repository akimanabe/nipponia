test_that("function exists", {
  expect_type(halfkakko, "closure")
})

test_that("function properly converts wide parenthesis to half width", {
  expect_equal(halfkakko("\uff08ABCD\uff09"), "(ABCD)")
})
