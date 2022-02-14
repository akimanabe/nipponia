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
#' kanji <- c(paste0("\u662d\u548c", seq(59, 63)), paste0("\u5e73\u6210", seq(1, 11))) # showa and heisei
#'
#' single_kanji <- c(paste0("\u662d", seq(59, 63)), paste0("\u5e73", seq(1, 11))) # sho and hei
#'
#' zen_single_alphabet <- c(
#' paste0("\uff33", c("５９", "６０", "６１", "６２", "６３")), # zenkaku S
#' paste0("\uff28", seq(1, 11))) # zenkaku H
#'
#' zen_kanji <- c(
#' paste0("\u662d\u548c", c("５９", "６０", "６１", "６２", "６３")),
#' paste0("\u5e73\u6210", seq(1, 11)))
#'
#' zen_single_kanji <- c(
#' paste0("\u662d", c("５９", "６０", "６１", "６２", "６３")),
#' paste0("\u5e73", seq(1, 11)))
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
    stringr::str_replace(pattern = "\u5143", replacement = "1")

  showa <-
    stringr::str_match(jpyear_halfwidth, "S.+|\u662d.+") %>%
    stringr::str_extract("\\d?\\d") %>%
    as.numeric() %>%
    + 1925

  heisei <-
    stringr::str_match(jpyear_halfwidth, "H.+|\u5e73.+") %>%
    stringr::str_extract(., "\\d?\\d") %>%
    as.numeric() %>%
    + 1988

  reiwa <-
    stringr::str_match(jpyear_halfwidth, "R.+|\u4ee4.+") %>%
    stringr::str_extract(., "\\d?\\d") %>%
    as.numeric() %>%
    + 2018


  adyear <- dplyr::coalesce(showa, heisei, reiwa)

  return(adyear)

}
