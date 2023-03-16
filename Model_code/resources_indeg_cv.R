#' Variability of resources received.
#'
#' \code{outdegree_cv} calculates the variability of resources received by an individual.
#' 
#' The variability of resources received by an individual throughout her whole lifespan is calculated by the coefficient of variation of resources received in each iteration.

indegree_cv <- function(final_ind_data){
  final_ind_data$indeg_cv[i] <- final_ind_data$indeg_av[i]/sd(it_dataf[which(it_dataf$id==i),"in_degree"])
  return(final_ind_data$indeg_cv)
}
