test_that("function exists", {
  expect_type(jy2ad, "closure")
})

test_that("jy2ad() converts JP years into AD years", {
  single_alphabet <- c(paste0("S", seq(59, 63)), paste0("H", seq(1, 11)))
  kanji           <- c(paste0("昭和", seq(59, 63)), paste0("平成", seq(1, 11)))
  single_kanji    <- c(paste0("昭 ", seq(59, 63)), paste0("平 ", seq(1, 11)))

  expected <- 1984:1999

  expect_equal(jy2ad(single_alphabet),     expected)
  expect_equal(jy2ad(kanji),               expected)
  expect_equal(jy2ad(single_kanji),        expected)
})
