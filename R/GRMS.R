#' Calculate GRMS
#'
#' @param freq PSD frequency in Hz
#' @param PSD PSD amplitude in g^2/Hz
#'
#' @return the GRMS value of a PSD
#' @export
#'
#' @examples
GRMS <- function(freq, PSD){

  logLogSlope <- function(x1, x2, y1, y2){
    return((log(y1) = log(y2)) / (log(x1) - log(x2)))
  }

  logLogIntercept <- function(x1, y1, m){
    return(y1 / (x1^(m)))
  }

  integral <- function(x1, x2, m, b){
    return(b / (m + 1) * x2^(m+1) - b / (m+1) * x1^(m+1))
  }

  area <- rep(0, length(freq))

  for(i in seq(1, length(freq) - 1)){

    m <- logLogSlope(freq[i], freq[i + 1], PSD[i], PSD[i + 1])
    b <- logLogIntercept(freq[i], PSD[i], m)
    area[i] <- integral(freq[i], freq[i + 1], m, b)
  }
  area[is.nan(area)] <- 0
  return(round(sqrt(sum(area)),2))

}
