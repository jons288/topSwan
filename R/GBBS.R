#' Score Great British Baking Show Guesses
#'
#' @param result Result of the show (Vector)
#' @param guess  Players Guess (Vector)
#'
#' @return Sum of the players Score
#' @export
#'
#' @examples
#' result <- sample(seq(1,10))
#' guess <- sample(seq(1,10))
#'
#' score <- GBBS(result,guess)
GBBS <- function(result,guess) {

  score <- rep(0, length(guess))
  score[which(result == guess)] <- 7
  score[which(result == guess & result == 1)] <- 15
  wrong <- which(result != guess)
  score[wrong] <- -abs(result[wrong] - guess[wrong]) + 4
  return(score)
}
