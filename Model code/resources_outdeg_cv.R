#' Variability of resources given away.
#'
#' \code{outdegree_cv} calculates the variability of resources given away by an individual.
#' 
#' The variability of resources given away by an individual throughout her whole lifespan is calculated by the coefficient of variation of resources given away in each iteration.

outdegree_cv <- function(final_ind_data){
  final_ind_data$outdeg_cv[i] <- final_ind_data$outdeg_av[i]/sd(as.numeric(it_data[i,grep("out_degree",colnames(it_data))]),na.rm = T)
  return(final_ind_data$outdeg_cv)
}
