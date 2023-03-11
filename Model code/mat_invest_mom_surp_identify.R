#' Mother surplus (identification).
#'
#' \code{mom_surplus} identify if an individual hasa surplus for maternal investment or not.
#' 
#' This is a function that defines if the individual has a surplus of resources (1) or not (0) that can be used for maternal investment for her descendants. the surplus is defined by the amount for extra resources that the individual has after covering the survival costs in a year.
#'
mom_surplus <- function(it_indpop){
  if(it_indpop$stage[i]==3 & it_indpop$store_a[i] > surv_cost){
    it_indpop$mom_surplus[i] <- 1
  } else{
    it_indpop$mom_surplus[i] <- 0
  }
  return(it_indpop$mom_surplus)
}
