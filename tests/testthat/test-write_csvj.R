test_that("function exists", {
  expect_type(write_csvj, "closure")
})

test_that("function can save Japanese csv", {
  a <- c("ほげ", "ふが")
  b <- c("foo", "bar")
  dat <- tibble::tibble(a = a, b = b)
  write_csvj(dat = dat, filename = "test.csv")
    expect_equal(
      readr::read_csv("test.csv",
                      locale = readr::locale(encoding = "CP932")) %>%
        dplyr::pull(a),
      a)
})
