#' Total amount of resources received.
#'
#' \code{indegree_total} calculates the total amount of resources received by an individual.
#' 
#' The total amount of resources received by an individual throughout her whole lifespan is calculated by the sum of resources received in each iteration.

indegree_total <- function(final_ind_data){
  final_ind_data$indeg_total[i] <- sum(it_dataf[which(it_dataf$id==i),"in_degree"])
  return(final_ind_data$indeg_total)
}
