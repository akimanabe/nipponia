test_that("function exists", {
  expect_type(jy2ad, "closure")
})

test_that("jy2ad() converts JP years into AD years", {
  single_alphabet <- c(paste0("S", seq(59, 63)), paste0("H", seq(1, 11)))
  kanji           <- c(paste0("\u662d\u548c", seq(59, 63)),
                       paste0("\u5e73\u6210", seq(1, 11)))
  single_kanji    <- c(paste0("\u662d", seq(59, 63)),
                       paste0("\u5e73", seq(1, 11)))

  expected <- 1984:1999

  expect_equal(jy2ad(single_alphabet),     expected)
  expect_equal(jy2ad(kanji),               expected)
  expect_equal(jy2ad(single_kanji),        expected)
})
