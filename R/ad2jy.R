#' Convert AD to Japanese year
#'
#' @param adyear vector of AD year
#' @param use_old_name TRUE for 2019 = H31, FALSE for 2019 = R1
#'
#' @return japanese year in alphabetical name
#' @export
#'
#' @examples
#' \dontrun{
#' sample_year <- seq(1926, 2020,by = 1)
#' ad2jy(adyear = sample_year, use_old_name = TRUE)
#' }

ad2jy <- function(adyear, use_old_name = TRUE) {
  assertthat::assert_that(
    stringr::str_detect(adyear, "[:digit:]{4}") %>%
      unique() %>%
      isTRUE(),
    msg = "adyear must be four digit number and should not contain alphabet or punctuation" # nolint
  )

  adyear_halfwidth <-
    adyear %>%
    as.character() %>%
    stringi::stri_trans_nfkc()

  showa <-
    stringr::str_extract(adyear_halfwidth, "19[2-7][0-9]|198[0-8]") %>%
    as.numeric() %>%
    -1925 %>%
    stringr::str_c("S", ., sep = "")

  heisei <-
    stringr::str_extract(adyear_halfwidth,
                         "1989|199[0-9]|200[0-9]|201[0-8]") %>%
    as.numeric() %>%
    -1988 %>%
    stringr::str_c("H", ., sep = "")

  reiwa <-
    stringr::str_extract(adyear_halfwidth, "2019|20[2-9][0-9]") %>%
    as.numeric() %>%
    -2018 %>%
    stringr::str_c("R", ., sep = "")

  jpyear <- dplyr::coalesce(showa, heisei, reiwa)

  if (use_old_name == TRUE) {
    jpyear <-
      stringr::str_replace_all(jpyear, "R1", "H31")
  }else{
    jpyear <- jpyear
  }

  return(jpyear)

}
