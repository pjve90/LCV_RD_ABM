#' Resource production.
#'
#' \code{produce} returns the amount of resources that an individual produces.
#' 
#' This is a function that defines the amount of resources an individual produces. It is based a binomial distribution with \code{n} equal to 1, \code{size} defined as the stage-specific values from \code{maxprod}, and \code{prob} based on the stage-specific values from \code{prod_prob}.

produce <- function(it_indpop){
  it_indpop$prod_a[i] <- rbinom(1, maxprod[it_indpop$stage[i]],prod_prob[it_indpop$stage[i],d])
  it_indpop$res_a[i] <- it_indpop$store_a[i]+it_indpop$prod_a[i]
  return(it_indpop)
}

