#' Average amount of resources received.
#'
#' \code{outdegree_av} calculates the average amount of resources received by an individual.
#' 
#' The average amount of resources received by an individual throughout her whole lifespan is calculated by the average of resources received in each iteration.

indegree_av <- function(final_ind_data){
  final_ind_data$indeg_av[i] <- mean(as.numeric(rowSums(it_data[i,grep("in_degree",colnames(it_data))],na.rm = T)))
  return(final_ind_data$indeg_av)
}
