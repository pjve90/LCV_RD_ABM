#' Lifetime reproductive output (LRO).
#'
#' \code{lro} calculates the lifetime reproductive output of an individual.
#' 
#' The lifetime reproductive output of an individual is the total number of descendants that she has produced through her lifespan. It is the sum of positive reproductive outcomes (1) in \code{it_data}.

lifetime_reproductive_output <- function(final_ind_data){
  final_ind_data$lro[i] <- max(it_dataf[which(it_dataf$id==i),"lro"])
  return(final_ind_data$lro)
}
