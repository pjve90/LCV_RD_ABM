#' Total amount of resources given away.
#'
#' \code{outdegree_total} calculates the total amount of resources given away by an individual.
#' 
#' The total amount of resources given away by an individual throughout her whole lifespan is calculated by the sum of resources given away in each iteration.

outdegree_total <- function(final_ind_data){
  final_ind_data$outdeg_total[i] <- as.numeric(rowSums(it_data[i,grep("out_degree",colnames(it_data))],na.rm = T))
  return(final_ind_data$outdeg_total)
}
