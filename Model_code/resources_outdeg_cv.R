#' Variability of resources given away.
#'
#' \code{outdegree_cv} calculates the variability of resources given away by an individual.
#' 
#' The variability of resources given away by an individual throughout her whole lifespan is calculated by the coefficient of variation of resources given away in each iteration.

outdegree_cv <- function(final_ind_data){
  final_ind_data$outdeg_cv[i] <- sd(it_data[which(it_data$id==i),"out_degree"])/mean(it_data[which(it_data$id==i),"out_degree"])
  return(final_ind_data$outdeg_cv)
}
