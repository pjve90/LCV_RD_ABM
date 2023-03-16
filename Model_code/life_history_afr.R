#' Age at first reproduction (AFR).
#'
#' \code{afr} calculates the age at first reproduction of an individual.
#' 
#' The age at first reproduction of an individual is the age at which an individual has her first descendants, transitioning from an adult stage to a reproductive career stage.

afr <- function(final_ind_data){
  if(length(it_dataf[which(it_dataf$id==i & it_dataf$stage==3),"age"]) > 0){
  final_ind_data$afr[i] <- min(it_dataf[which(it_dataf$id==i & it_dataf$stage==3),"age"])
  }else {
    final_ind_data$afr[i] <- NA
  }
  return(final_ind_data$afr)
}
