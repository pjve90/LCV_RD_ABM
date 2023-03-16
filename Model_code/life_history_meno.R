#' Age at menopause.
#'
#' \code{menos} calculates the age at which an individual reaches menopause.
#' 
#' The age at menopause of an individual is the age at which an individual transitions from an adult or reproductive career stage into a post-reproductive stage.

meno <- function(final_ind_data){
  if(length(it_dataf[which(it_dataf$id==i & it_dataf$stage==4),"age"]) > 0){
    final_ind_data$meno[i] <- min(it_dataf[which(it_dataf$id==i & it_dataf$stage==4),"age"])
  }else {
    final_ind_data$meno[i] <- NA
  }
  return(final_ind_data$meno)
}
