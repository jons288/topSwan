#' Define Log 10 X-axis with proper grid settings and labels
#'
#' @param ... Arguments to be passed to the scale_x_log10 ggplot2 function
#'
#' @return Log 10 X-axis with proper grid settings and labels
#' @export
#' @import ggplot2
#'
#' @examples
scale_x_logTen <- function(...) {

  # A function factory for minor log breaks
  minor_breaks_log <- function(base) {
    # Prevents lazy evaluation
    force(base)
    # Wrap calculation in a function that the outer function returns
    function(limits) {
      ggplot2:::calc_logticks(
        base = base,
        minpow = floor(log(limits[1], base = base)),
        maxpow = ceiling(log(limits[2], base = base))
      )$value
    }
  }

  ret <- ggplot2::scale_x_log10(minor_breaks = minor_breaks_log(10),
                                breaks = c(1 %o% 10^(-10:10)),...)
  ret
}
