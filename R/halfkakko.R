#' Converts wide parenthesis to halfwitdh
#'
#' @param dat string contains wide (zenkaku) parenthesis
#'
#' @return string
#' @export
#'
#' @examples
#' \dontrun{
#' halfkakko("\uff08ABCD\uff09")
#' }
halfkakko <- function(dat) {
  dat %>%
    stringr::str_replace_all(., "\\uff08", "(") %>%
    stringr::str_replace_all(., "\\uff09", ")")
}
