#' Resource production (probability outcome).
#'
#' \code{produce} returns the output of the production probability of the individual.
#' 
#' This is a function that defines if the individual produces resources that year (1) or not (0). It is based a Bernoulli distribution with \code{n} and \code{size} equal to 1, and \code{prob} based on the stage-specific values from \code{prod_prob}.

produce <- function(it_indpop){
  it_indpop$prod_a[i] <- rbinom(1, maxprod[it_indpop$stage[i]],prod_prob[it_indpop$stage[i]])
  it_indpop$res_a[i] <- it_indpop$store_a[i]+it_indpop$prod_a[i]
  return(it_indpop)
}

