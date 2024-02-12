#' Copy dataframe to clipboard for paste into Excel
#'
#' @param x The dataframe you want to copy
#' @param row.names True if you want row names
#' @param col.names False if you don't want column names
#' @param ... passed to write.table function
#'
#' @return a dataframe in your clipboard, paste it into Excel
#' @export
#'
#' @examples
clip <- function(x, row.names = FALSE, col.names = TRUE, ...){
  write.table(x, "clipboard", sep = "\t", row.names = row.names, col.names = col.names, ...)
}
