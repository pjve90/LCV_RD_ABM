#' Lifetime reproductive output (LRO).
#'
#' \code{lro} calculates the lifetime reproductive output of an individual.
#' 
#' The lifetime reproductive output of an individual is the total number of descendants that she has produced through her lifespan. It is the sum of positive reproductive outcomes (1) in \code{it_data}.

lifetime_reproductive_output <- function(lht_list){
  lht_list[[i]]$lro <- aggregate(raw_sample[[i]],by=list(id=raw_sample[[i]]$id),max)$lro
  return(lht_list[[i]])
}
