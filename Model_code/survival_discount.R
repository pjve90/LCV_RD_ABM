#' Discount of survival cost.
#'
#' \code{survive_c} discounts the amount of resources needed for the survival costs.
#' 
#' This is a function that discounts the survival cost from the resources available for an individual.

survive_c <- function(it_indpop) {
  it_indpop$res_a[i] <- it_indpop$res_a[i] - (surv_cost*it_indpop$surv[i])
  return(it_indpop$store_a)
}
