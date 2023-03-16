#' Longevity.
#'
#' \code{longevity} calculates the longevity of an individual.
#' 
#' The longevity of an individual is the amount of years that the individual has lived, from birth to death. It is the maximum age of an individual recorded in \code{it_dataf}.

longevity <- function(final_ind_data){
  final_ind_data$lng[i] <- max(it_dataf[which(it_dataf$id==i),"age"])
  return(final_ind_data$lng)
}
