#' Average amount of resources given away.
#'
#' \code{outdegree_av} calculates the average amount of resources given away by an individual.
#' 
#' The average amount of resources given away by an individual throughout her whole lifespan is calculated by the average of resources given away in each iteration.

outdegree_av <- function(final_ind_data){
  final_ind_data$outdeg_av[i] <- mean(as.numeric(rowSums(it_data[i,grep("out_degree",colnames(it_data))],na.rm = T)))
  return(final_ind_data$outdeg_av)
}
