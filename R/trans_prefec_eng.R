#'  Convert Japanese prefecture names to English
#'
#' @param dat Japanese prefecture names in `vector` or `tibble`
#'
#' @return Anglicized Japanese prefecture names
#' @export
#'
#' @examples
#' \dontrun{
#' }
trans_prefec_eng <- function(dat) {
  if (class(dat) %in% c("character", "factor")) {
    res <-
      tibble::tibble(JPrefec = dat) %>%
      dplyr::mutate(JPrefec = trans_prefec_eng(JPrefec)) %>%
      dplyr::pull(JPrefec)
  }

  if (class(dat) %in% c("tbl_df", "tbl", "data.frame")) {
    res <-
      dplyr::mutate(dat = trans_prefec_eng(dat)) %>%
      dplyr::pull(dat)
  }
  return(res)
}

#' Convert Japanese prefec names to English
#'
#' @param dat Prefecture names in `tibble` format
#'
#' @return `tibble` Anglicized prefecture names
#' @export
#'
#' @examples
#' \dontrun{
#' }
prefec_jp2en <- function(dat) {
  dplyr::recode(dat,
                "北海道" = "Hokkaido",
                "青森" = "Aomori",
                "岩手" = "Iwate",
                "宮城" = "Miyagi",
                "秋田" = "Akita",
                "山形" = "Yamagata",
                "福島" = "Fukushima",
                "茨城" = "Ibaraki",
                "栃木" = "Tochigi",
                "群馬" = "Gunma",
                "埼玉" = "Saitama",
                "千葉" = "Chiba",
                "東京" = "Tokyo",
                "神奈川" = "Kanagawa",
                "新潟" = "Niigata",
                "富山" = "Toyama",
                "石川" = "Ishikawa",
                "福井" = "Fukui",
                "山梨" = "Yamanashi",
                "長野" = "Nagano",
                "岐阜" = "Gifu",
                "静岡" = "Shizuoka",
                "愛知" = "Aichi",
                "三重" = "Mie",
                "滋賀" = "Shiga",
                "京都" = "Kyoto",
                "大阪" = "Osaka",
                "兵庫" = "Hyogo",
                "奈良" = "Nara",
                "和歌山" = "Wakayama",
                "鳥取" = "Tottori",
                "島根" = "Shimane",
                "岡山" = "Okayama",
                "広島" = "Hiroshima",
                "山口" = "Yamaguchi",
                "徳島" = "Tokushima",
                "香川" = "Kagawa",
                "愛媛" = "Ehime",
                "高知" = "Kochi",
                "福岡" = "Fukuoka",
                "佐賀" = "Saga",
                "長崎" = "Nagasaki",
                "熊本" = "Kumamoto",
                "大分" = "Oita",
                "宮崎" = "Miyazaki",
                "鹿児島" = "Kagoshima",
                "沖縄" = "Okinawa"
  )

}

#' Factorize Japanese prefectures
#'
#' @param prefec vector of Japanese prefectures
#' @param lang
#'
#' @return
#' @export
#'
#' @examples
factor_prefec <- function(prefec) {
    factor(prefec, levels = c(
      "北海道", "青森", "岩手", "宮城", "秋田", "山形", "福島", "茨城", "栃木",
      "群馬", "埼玉", "千葉", "東京", "神奈川", "新潟", "富山", "石川", "福井",
      "山梨", "長野", "岐阜", "静岡", "愛知", "三重", "滋賀", "京都", "大阪",
      "兵庫", "奈良", "和歌山", "鳥取", "島根", "岡山", "広島", "山口", "徳島",
      "香川", "愛媛", "高知", "福岡", "佐賀", "長崎", "熊本", "大分", "宮崎",
      "鹿児島", "沖縄"))
}
