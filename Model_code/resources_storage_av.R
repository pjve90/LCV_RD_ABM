#' Average amount of stored resources.
#'
#' \code{storage_av} calculates the average amount of stored resources of an individual.
#' 
#' The average amount of stored resources of an individual throughout her whole lifespan is calculated by the average of stored resources in each iteration.

storage_av <- function(final_ind_data){
  final_ind_data$store_av[i] <- mean(it_dataf[which(it_dataf$id==i),"store_a"])
  return(final_ind_data$store_av)
}
