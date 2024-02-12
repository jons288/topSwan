#'cbind function for dataframes and arrays that works with one empty input
#'
#' @param ... dataframes or arrays to be column binded.
#'
#' @return a dataframe with your data
#' @export
#'
#' @examples
#' t1 <- data.frame()
#' t2 <- c(1,2,3,4,5)
#' out <- cbind_fill(t1,t2)
#'
#'
#' t1 <- data.frame()
#' t2 <- mtcars
#' out <- cbind_fill(t1,t2)
#'
cbind_fill <- function(...){
  nm <- list(...)
  nm <- lapply(nm, as.matrix)

  n <- max(sapply(nm, nrow))
  out <- do.call(cbind,lapply(nm, function(x)
    rbind(x, matrix(, n-nrow(x), ncol(x)))))
  return(as.data.frame(out))

}
