#' Total amount of stored resources.
#'
#' \code{storage_total} calculates the total amount of stored resources of an individual.
#' 
#' The total amount of stored resources of an individual throughout her whole lifespan is calculated by the sum of stored resources in each iteration.

storage_total <- function(final_ind_data){
  final_ind_data$store_total[i] <- sum(it_dataf[which(it_dataf$id==i),"store_a"])
  return(final_ind_data$store_total)
}
