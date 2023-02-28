#' Resource production (amount)
#'
#' \code{produce_a} returns the output of the production probability of the individual.
#' 
#' This is a function that gives the amount of resources produced if the individual had a successful probability outcome (1). It is is the sum of the current amount of resources available (\code{prod_a}) and the product of the probability outcome (\code{prod_o}) and the stage-specific habitat quality (\code{habitat}).

produce_a <- function(it_indpop){
  it_indpop$prod_a[i] <- it_indpop$prod_a[i]+it_indpop$prod_o[i]*habitat[it_indpop$stage[i]]
  return(it_indpop$prod_a)
}
