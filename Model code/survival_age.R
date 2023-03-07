#' Age.
#'
#' \code{age} calculates the amount of years an individual is alive.
#' 
#' This is a function that adds by one year for each time the individual survives one iteration.

age <- function(it_indpop) {
  it_indpop$age[i] <- it_indpop$age[i] + it_indpop$surv[i]
  return(it_indpop$age)
}
