#' Reproduction.
#'
#' \code{reproduce} defines whether an individual reproduce (1) or not (0) in that iteration.
#' 
#' This is a function that defines whether an individual reproduce or not, depending on the amount of resources available for the individual. If the individual has more resources than the reproductive cost then she will reprooduce (1), if she is below that amount then she will not (0).

reproduce <- function(it_indpop){
  if(it_indpop$stage[i]==3 & it_indpop$res_a[i] >= surv_cost+repro_cost) {
    it_indpop$repro[i] <- 1
  } else{
    it_indpop$repro[i] <- 0
  }
  return(it_indpop$repro)
}
