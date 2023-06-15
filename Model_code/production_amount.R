#' Resource production (amount)
#'
#' \code{produce_a} returns the amount of resources reproduced by the individual in a year.
#' 
#' This is a function that gives the amount of resources produced if the individual had a successful probability outcome (\code{prod_a}). It is the product of the probability outcome (\code{prod_o}) and the stage-specific habitat quality (\code{habitat}). Also, the function updates the amount of resources stored (\code{stored_a}).

produce_a <- function(it_indpop){
  it_indpop$prod_a[i] <- it_indpop$prod_o[i]*habitat[it_indpop$stage[i]]
  it_indpop$res_a[i] <- it_indpop$store_a[i]+it_indpop$prod_a[i]
  return(it_indpop)
}
