#' Time since last reproduction.
#'
#' \code{last_repro} calculates the time since the last reproduction of an individual.
#' 
#' This is a function that calculates the amount of years that has passed from the last reproductive event in the lifetime of an individual.

reproduce <- function(it_indpop){
  if(it_indpop$repro[i]==1) {
    it_indpop$tlr[i] <- 0
  } else{
    it_indpop$tlr[i] <- it_indpop$tlr[i] + 1
  }
  return(it_indpop$tlr)
}
