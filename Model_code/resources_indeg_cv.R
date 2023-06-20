#' Variability of resources received.
#'
#' \code{indegree_cv} calculates the variability of resources received by an individual.
#' 
#' The variability of resources received by an individual throughout her whole lifespan is calculated by the coefficient of variation of resources received in each iteration.

indegree_cv <- function(final_ind_data){
  final_ind_data$indeg_cv[i] <- sd(it_data[which(it_data$id==i),"in_degree"])/mean(it_data[which(it_data$id==i),"in_degree"])
  return(final_ind_data$indeg_cv)
}
