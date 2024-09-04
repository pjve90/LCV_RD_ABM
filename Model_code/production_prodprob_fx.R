#' Stage-specific production probabilities.
#'
#' \code{production_prob} returns the stage-specific production probabilities.
#' 
#' This is a function that defines the probabilities that an individual can produce, depending on the life cycle stage that she is. \code{max_prod_prob} is the parameter that defines the maximum production probability. \code{stage_prod_prob} is a vector with the stage-specific offset in the probabilities that an individual can produce. \code{prod_prob} calculates the stage-specific production probabilities.

production_prob <- function(max_prod_prob){
  stage_prod_prob <- c(0.2,1,1,0.7) #stage-specific offset in the probabilities of production for an individual
  prod_prob <- round(outer(stage_prod_prob,max_prod_prob,"*"),2) #stage-specific production probabilities
  return(prod_prob)
}
