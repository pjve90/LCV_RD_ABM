#' Descendant need (amount).
#'
#' \code{desc_need_a} returns the amount of resources an individual needs from her mother to ensure her survival.
#' 
#' This is a function that calculates the amount of resources that an individual needs from her mother to ensure her survival. It is based on the product from the subtraction between the survival cost (\code{surv_effort}) and the amount of resources stored the individual has (\code{store_a}), and if the individual needs resources or not (\code{desc_need}).
#'
desc_need_a <- function(it_indpop){
  if(is.na(it_indpop$desc_need[i])==F){
    it_indpop$desc_need_a[i] <- (surv_cost - it_indpop$res_a[i])*it_indpop$desc_need[i]
  }
  return(it_indpop$desc_need_a)
}

