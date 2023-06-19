#' Stored resources.
#'
#' \code{store} returns the amount of resources that an individual stores for the next iteration.
#' 
#' This is a function that defines how many resources an individual carries with herself to the next iteration.

store <- function(it_indpop){
  it_indpop$store_a[i] <- it_indpop$res_a[i]
  return(it_indpop$store_a)
}

