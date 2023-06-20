#' Total amount of resources available.
#'
#' \code{available_total} calculates the total amount of resources available of an individual.
#' 
#' The total amount of resources available of an individual throughout her whole lifespan is calculated by the sum of resources available in each iteration.

available_total <- function(final_ind_data){
  final_ind_data$available_total[i] <- sum(it_data[which(it_data$id==i),"res_a"])
  return(final_ind_data$available_total)
}
