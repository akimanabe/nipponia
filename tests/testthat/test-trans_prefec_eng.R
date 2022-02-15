test_that("function exists", {
  expect_type(prefec_jp2en, "closure")
})

test_that("function converts prefec names from JP to EN", {
  expect_equal(
    tibble::tibble(foo = "千葉") %>%
    dplyr::mutate(foo = prefec_jp2en(foo)) %>%
    dplyr::pull(foo),
    "Chiba")

  expect_equal(
    tibble::tibble(foo = c("千葉", "神奈川")) %>%
      dplyr::mutate(foo = prefec_jp2en(foo)) %>%
      dplyr::pull(foo),
    c("Chiba", "Kanagawa"))
})

test_that("function exists", {
  expect_type(trans_prefec_eng, "closure")
})

test_that("function converts vector and tibble into English prefecs", {
  expect_equal(
    trans_prefec_eng("千葉"),
    "Chiba")

  expect_equal(
    trans_prefec_eng(c("神奈川", "千葉")),
    c("Kanagawa", "Chiba")
  )

  expect_equal(
    tibble::tibble(foo = "千葉") %>%
      dplyr::mutate(foo = trans_prefec_eng(foo)) %>%
      dplyr::pull(foo),
    "Chiba")

  expect_equal(
    tibble::tibble(foo = c("千葉", "神奈川")) %>%
      dplyr::mutate(foo = trans_prefec_eng(foo)) %>%
      dplyr::pull(foo),
    c("Chiba", "Kanagawa"))
})
