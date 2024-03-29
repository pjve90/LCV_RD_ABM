#' Discount of reproductive cost.
#'
#' \code{reproduce_c} discounts the amount of resources needed for the reproductive costs.
#' 
#' This is a function that discounts the reproductive cost from the stored resources of an individual.

reproduce_c <- function(it_indpop) {
  it_indpop$res_a[i] <- it_indpop$res_a[i] - (repro_cost*it_indpop$repro[i])
  return(it_indpop$res_a)
}
