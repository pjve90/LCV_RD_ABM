#' Age at sexual maturity.
#'
#' \code{asm} calculates the age at sexual maturity of an individual.
#' 
#' The age at sexual maturity of an individual is the age at which an individual transitions from a juvenile to an adult.

asm <- function(final_ind_data){
  if(length(it_dataf[which(it_dataf$id==i & it_dataf$stage==2),"age"]) > 0){
  final_ind_data$asm[i] <- min(it_dataf[which(it_dataf$id==i & it_dataf$stage==2),"age"])
  } else {
    final_ind_data$asm[i] <- NA
  }
  return(final_ind_data$asm)
}
