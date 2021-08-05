#' Convert Japanese year to AD
#'
#' @param jpyear vector of Japanese years
#'
#' @return numeric vector
#' @export
#' @examples
#' \dontrun{
#' single_alphabet <- c(paste0("S", seq(59, 63)), paste0("H", seq(1, 11)))
#'
#' kanji <- c(paste0("昭和", seq(59, 63)), paste0("平成", seq(1, 11)))
#'
#' single_kanji <- c(paste0("昭 ", seq(59, 63)), paste0("平 ", seq(1, 11)))
#'
#' zen_single_alphabet <- c(
#' paste0("Ｓ", c("５９", "６０", "６１", "６２", "６３")),
#' paste0("Ｈ", seq(1, 11)))
#'
#' zen_kanji <- c(
#' paste0("昭和", c("５９", "６０", "６１", "６２", "６３")),
#' paste0("平成", seq(1, 11)))
#'
#' zen_single_kanji <- c(
#' paste0("昭", c("５９", "６０", "６１", "６２", "６３")),
#' paste0("平", seq(1, 11)))
#'
#' jy2ad(single_alphabet)
#' jy2ad(kanji)
#' jy2ad(single_kanji)
#' jy2ad(zen_single_alphabet)
#' jy2ad(zen_kanji)
#' jy2ad(zen_single_kanji)
#' }

jy2ad <- function(jpyear) {

  jpyear_halfwidth <- jpyear %>%
    stringi::stri_trans_nfkc() %>%
    stringr::str_replace(pattern = "元", replacement = "1")

  showa <-
    stringr::str_match(jpyear_halfwidth, "S.+|昭.+") %>%
    stringr::str_extract("\\d?\\d") %>%
    as.numeric() %>%
    + 1925

  heisei <-
    stringr::str_match(jpyear_halfwidth, "H.+|平.+") %>%
    stringr::str_extract(., "\\d?\\d") %>%
    as.numeric() %>%
    + 1988

  reiwa <-
    stringr::str_match(jpyear_halfwidth, "R.+|令.+") %>%
    stringr::str_extract(., "\\d?\\d") %>%
    as.numeric() %>%
    + 2018


  adyear <- dplyr::coalesce(showa, heisei, reiwa)

  return(adyear)

}
