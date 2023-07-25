#' Surplus of resources for resource transfers (maximum out degree).
#'
#' \code{max_deg} estimates the surplus of resources that an individual can use for resource transfers. The surplus defines the maximum out degrees of an individual.
#' 
#' The definition of the surplus of resources for an individual is calculated based on the amount of extra resources that an individual has, after accounting for the costs of survival.

max_deg <- function(it_indpop){
  if(it_indpop$stage[i]==2 | it_indpop$stage[i]==3){
    it_indpop$max_deg[i] <- it_indpop$res_a[i]-(surv_cost+repro_cost)
  }else{
    it_indpop$max_deg[i] <- it_indpop$res_a[i]-surv_cost
  }
  it_indpop$max_deg[it_indpop$max_deg < 0] <- 0
  return(it_indpop$max_deg)
}
