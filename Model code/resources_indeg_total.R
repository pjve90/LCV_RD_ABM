#' Total amount of resources received.
#'
#' \code{outdegree_total} calculates the total amount of resources received by an individual.
#' 
#' The total amount of resources received by an individual throughout her whole lifespan is calculated by the sum of resources received in each iteration.

indegree_total <- function(final_ind_data){
  final_ind_data$indeg_total[i] <- as.numeric(rowSums(it_data[i,grep("in_degree",colnames(it_data))],na.rm = T))
  return(final_ind_data$indeg_total)
}
