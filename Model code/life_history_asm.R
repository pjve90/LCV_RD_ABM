#' Age at sexual maturity.
#'
#' \code{asm} calculates the age at sexual maturity of an individual.
#' 
#' The age at sexual maturity of an individual is the age at which an individual transitions from a juvenile to an adult.

asm <- function(final_ind_data){
  if(it_data[i,grep("stage",colnames(it_data))][!is.na(it_data[i,grep("stage",colnames(it_data))])][1]==1 & as.numeric(rowSums(it_data[i,grep("stage",colnames(it_data))],na.rm = T)) > 0){
    final_ind_data$asm[i] <-
      min(cumsum(it_data[i,grep("surv",colnames(it_data))][!is.na(it_data[i,grep("surv",colnames(it_data))])])[it_data[i,grep("stage",colnames(it_data))][!is.na(it_data[i,grep("stage",colnames(it_data))])]==2])
  } else{
    final_ind_data$asm[i] <- NA
  }
  return(final_ind_data$asm)
}
