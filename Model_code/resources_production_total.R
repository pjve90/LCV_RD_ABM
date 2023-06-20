#' Total amount of resources produced.
#'
#' \code{produce_total} calculates the total amount of resources produced by an individual.
#' 
#' The total amount of resources produced by an individual throughout her whole lifespan is calculated by the sum of resources produced in each iteration.

produce_total <- function(final_ind_data){
  final_ind_data$prod_total[i] <- sum(it_data[which(it_data$id==i),"prod_a"])
  return(final_ind_data$prod_total)
}
