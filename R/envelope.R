#' Envelope multiple random vibration PSD traces in log space
#'
#' @param freq array of frequency values
#' @param PSD array of PSD values
#' @param trace ID (factor levels) for freq/PSD curve
#' @param nout number of output pairs, default 5000
#'
#' @return a dataframe with frequency and enveloped PSD level
#' @export
#'
#' @examples
envelope <- function(freq, PSD, trace, nout = 5000){

  dat <- data.frame(freq = freq,
                    PSD = PSD,
                    trace = as.factor(trace))
  newFreq <- round(seq_log(min(dat$freq), max(dat$freq), length.out = nout), 4)

  o1 <- data.frame(freq = as.numeric(),
                   PSD = as.numeric(),
                   trace = as.character())

  for( t in levels(dat$trace)){

    d2 <- dat[which(dat$trace == t),]
    d3 <- data.frame(freq = newFreq,
                     PSD = 10^(approx(log10(d2$freq), log10(d2$PSD), xout = log10(newFreq),
                                                           yleft = log10(min(d2$PSD)), yright = log10(min(d2$PSD)))$y),
                     trace = t)

    o1 <- rbind(o1, d3)

  }

  newPSD = c()

  for( i in seq(1, length(newFreq))){

    o2 <- o1[which(o1$freq == newFreq[i]),]
    newPSD <- c(newPSD, max (o2$PSD))
  }

  out <- data.frame(freq = newFreq,
                    PSD = newPSD)

  return(out)
}
