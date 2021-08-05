test_that("function exists", {
  expect_type(ad2jy, "closure")
})

test_that("ad2jy() converts AD years into JP years", {
  expect_equal(
    ad2jy(c(1988, 1989, 2018, 2019, 2020)),
    c("S63", "H1", "H30", "H31", "R2"))
})

test_that("use_old_name option select using Reiwa or Heisei for 2019", {
  expect_equal(
    ad2jy(2019, use_old_name = TRUE),
    "H31"
  )

  expect_equal(
    ad2jy(2019, use_old_name = FALSE),
    "R1"
  )
})

test_that("adyaer variable only accept number", {
  expect_error(ad2jy("foo"))
})
