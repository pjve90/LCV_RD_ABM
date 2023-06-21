#' Average amount of resources available.
#'
#' \code{available_av} calculates the average amount of resources available of an individual.
#' 
#' The average amount of resources available of an individual throughout her whole lifespan is calculated by the average of resources available in each iteration.

available_av <- function(final_ind_data){
  final_ind_data$available_av[i] <- mean(it_dataf[which(it_dataf$id==i),"res_a"])
  return(final_ind_data$available_av)
}
