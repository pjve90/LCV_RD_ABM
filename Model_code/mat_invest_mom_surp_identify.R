#' Mother surplus (identification).
#'
#' \code{mom_surplus} identify if an individual has a surplus for maternal investment or not.
#' 
#' This is a function that defines if the individual has a surplus of resources (1) or not (0) that can be used for maternal investment for her descendants. the surplus is defined by the amount for extra resources that the individual has after covering the survival costs in a year.
#'
mom_surplus <- function(it_indpop){
  if(it_indpop$lro[i]>=1 & it_indpop$res_a[i] > surv_cost){
    it_indpop$mom_surplus[i] <- 1
  } else 
    if(it_indpop$lro[i]>=1 & it_indpop$res_a[i] <= surv_cost){
    it_indpop$mom_surplus[i] <- 0
  }else{
    it_indpop$mom_surplus[i] <- NA
  }
  return(it_indpop$mom_surplus)
}
