#' Average amount of resources received.
#'
#' \code{indegree_av} calculates the average amount of resources received by an individual.
#' 
#' The average amount of resources received by an individual throughout her whole lifespan is calculated by the average of resources received in each iteration.

indegree_av <- function(final_ind_data){
  final_ind_data$indeg_av[i] <- mean(it_data[which(it_data$id==i),"in_degree"])
  return(final_ind_data$indeg_av)
}
