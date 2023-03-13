#' Age at menopause.
#'
#' \code{menos} calculates the age at which an individual reaches menopause.
#' 
#' The age at menopause of an individual is the age at which an individual transitions from an adult or reproductive career stage into a post-reproductive stage.

meno <- function(final_ind_data){
  if(it_data[i,grep("stage",colnames(it_data))][!is.na(it_data[i,grep("stage",colnames(it_data))])][1]!=4 & min(cumsum(it_data[i,grep("surv",colnames(it_data))][!is.na(it_data[i,grep("surv",colnames(it_data))])])[it_data[i,grep("stage",colnames(it_data))][!is.na(it_data[i,grep("stage",colnames(it_data))])]==4]) != Inf){
    final_ind_data$meno[i] <- 
      min(cumsum(it_data[i,grep("surv",colnames(it_data))][!is.na(it_data[i,grep("surv",colnames(it_data))])])[it_data[i,grep("stage",colnames(it_data))][!is.na(it_data[i,grep("stage",colnames(it_data))])]==4])
  } else
    if(it_data[i,grep("stage",colnames(it_data))][!is.na(it_data[i,grep("stage",colnames(it_data))])][1]==4){
      final_ind_data$meno[i] <- 45  
    } else{
      final_ind_data$meno[i] <- NA
    }
  return(final_ind_data$meno)
}
