#' Logarithmic sequence
#'
#' @param from starting number for the sequence
#' @param to ending number for the sequence
#' @param length.out number of values in the sequence
#'
#' @return a sequence of length 'length.out', evenly spaced on a logarithmic scale
#' @export
#'
#' @examples
seq_log <- function(from, to, length.out){

  exp(
    seq(from = log(from), to = log(to), length.out = length.out)
  )

}
