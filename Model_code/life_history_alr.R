#' Age at last reproduction (ALR).
#'
#' \code{alr} calculates the age at last reproduction of an individual.
#' 
#' The age at last reproduction of an individual is the age at which an individual has her last descendant.

alr <- function(final_ind_data){
  if(length(it_dataf[which(it_dataf$id==i & it_dataf$repro==1),"age"]) > 0){
  final_ind_data$alr[i] <- max(it_dataf[which(it_dataf$id==i & it_dataf$repro==1),"age"]) 
  } else {
     final_ind_data$alr[i] <- NA
  }
  return(final_ind_data$alr)
}
