#' Lifetime reproductive output.
#'
#' \code{lro} calculates the total number of descendants an individual produces in her lifetime.
#' 
#' This is a function that calculate the total number of descendants an individual has during her life time. The value is updated if the individual reproduces in a year.

lro <- function(it_indpop) {
  it_indpop$lro[i] <- it_indpop$lro[i] + it_indpop$repro[i]
  return(it_indpop$lro)
}
