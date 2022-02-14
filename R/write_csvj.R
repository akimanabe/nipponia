#' Write csv with Japanese friendly encoding
#'
#' @param dat data
#' @param filename file name ends with .csv
#'
#' @return csv file
#' @export
#'
#' @examples
#' \dontrun{
#' write_csvj(mtcars, "mtcars.csv")}
write_csvj <-
  function(dat, filename) {
  utils::write.csv(dat, filename, fileEncoding = "CP932")
}
