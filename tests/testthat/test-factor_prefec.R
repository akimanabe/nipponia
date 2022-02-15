test_that("function exists", {
  expect_type(factor_prefec, "closure")
})

test_that("function correctly factorize the prefecs", {
  prefoo <- c("静岡", "東京", "千葉", "神奈川") # Ittosanken
  expect_equal(factor_prefec(prefoo) %>% class(), "factor")
})

test_that("function correctly sort the prefecs", {
  prefoo <- c("静岡", "東京", "千葉", "神奈川") # Ittosanken
  expect_equal(factor_prefec(prefoo) %>% sort() %>% as.character(),
               c("千葉", "東京", "神奈川", "静岡"))
})
