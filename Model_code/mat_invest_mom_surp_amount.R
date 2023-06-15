#' Mother surplus (amount).
#'
#' \code{mom_surplus_a} returns the amount of surplus that an individual has to invest in her descendant(s).
#' 
#' This is a function that calculates the amount of surplus the mother has to invest in her descendant(s). It is based on the product from the subtraction between the amount of resources stored the individual has (\code{store_a}) and the survival cost (\code{surv_effort}), and if the individual has a surplus for maternal investment or not (\code{mom_surplus}).
#'
mom_surplus_a <- function(it_indpop){
  it_indpop$mom_surplus_a[i] <- (it_indpop$res_a[i] - surv_cost)*it_indpop$mom_surplus[i]
  return(it_indpop$mom_surplus_a)
}
