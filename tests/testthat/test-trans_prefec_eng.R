test_that("function exists", {
  expect_type(prefec_jp2en, "closure")
})

test_that("function converts prefec names from JP to EN", {
  expect_equal(
    tibble::tibble(foo = "\u5343\u8449") %>% # chiba
    dplyr::mutate(foo = prefec_jp2en(foo)) %>%
    dplyr::pull(foo),
    "Chiba")

  expect_equal(
    tibble::tibble(foo = c("\u5343\u8449", "\u795e\u5948\u5ddd")) %>% # Chiba and Kanagawa
      dplyr::mutate(foo = prefec_jp2en(foo)) %>%
      dplyr::pull(foo),
    c("Chiba", "Kanagawa"))
})

test_that("function exists", {
  expect_type(trans_prefec_eng, "closure")
})

test_that("function converts vector and tibble into English prefecs", {
  expect_equal(
    trans_prefec_eng("\u5343\u8449"), # Chiba
    "Chiba")

  expect_equal(
    trans_prefec_eng(c("\u795e\u5948\u5ddd", "\u5343\u8449")), # Kanagawa and Chiba
    c("Kanagawa", "Chiba")
  )

  expect_equal(
    tibble::tibble(foo = "\u5343\u8449") %>% # Chiba
      dplyr::mutate(foo = trans_prefec_eng(foo)) %>%
      dplyr::pull(foo),
    "Chiba")

  expect_equal(
    tibble::tibble(foo = c("\u5343\u8449", "\u795e\u5948\u5ddd")) %>% # Chiba and Kanagawa
      dplyr::mutate(foo = trans_prefec_eng(foo)) %>%
      dplyr::pull(foo),
    c("Chiba", "Kanagawa"))
})
