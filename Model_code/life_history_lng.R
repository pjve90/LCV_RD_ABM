#' Longevity.
#'
#' \code{longevity} calculates the longevity of an individual.
#' 
#' The longevity of an individual is the amount of years that the individual has lived, from birth to death. It is the maximum age of an individual recorded in \code{it_dataf}.

longevity <- function(lht_list){
  lht_list[[i]]$lng <- aggregate(raw_sample[[i]],by=list(id=raw_sample[[i]]$id),max)$age
  return(lht_list[[i]])
}
