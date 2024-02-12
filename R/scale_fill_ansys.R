
#' ANSYS Color scale used with ggplot graphics
#'
#' @param rev number of colors used
#' @param n.breaks number of breaks
#' @param show.limits Show outer limits of legend
#' @param ... other arguments to ggplot2::scale_color_stepsn()
#'
#' @return a binned color scale to be used with ggplot graphics.
#' I recommend using theme(legend.key.height = unit(1, 'in'))) to
#' adjust the legend height
#' @export
#'
#' @import ggplot2
#'
#' @examples
#'
scale_fill_ansys <- function(rev = F,n.breaks =9,show.limits = T,...){

  cols <- c("#0000FF", "#00ABFF", "#00F1FF", "#00FFD2", "#00FF68",
            "#00FF00", "#C0FF00", "#FFF000", "#FFAB00", "#FF0000")

  #Reverse color order if desired
  if(rev){
    cols = rev(cols)
  }

  ret <- ggplot2::scale_fill_stepsn(colours = cols,
                                    n.breaks = n.breaks,
                                    show.limits = show.limits,...)
  ret
}
