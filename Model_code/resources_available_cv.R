#' Variability of resources available.
#'
#' \code{available_cv} calculates the variability of resources available of an individual.
#' 
#' The variability of resources available of an individual throughout her whole lifespan is calculated by the coefficient of variation of resources available in each iteration.

available_cv <- function(final_ind_data){
  final_ind_data$available_cv[i] <- sd(it_data[which(it_data$id==i),"res_a"])/mean(it_data[which(it_data$id==i),"res_a"])
  return(final_ind_data$available_cv)
}
