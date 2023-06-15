#' Amount of resources transferred.
#'
#' \code{transfers} calculates the the amount of resources that are transferred to other individuals, and the amount that the individual recieves.
#' 
#' The amount resources that are received or given away are calculated based on the results from the social network. The aftermath of those dynamics are recorded in the amount of resources stored.

transfers <- function(it_indpop){
  it_indpop$out_degree <- rowSums(network)
  it_indpop$in_degree <- colSums(network)
  it_indpop$res_a[i] <- it_indpop$res_a[i] - it_indpop$out_degree[i] + it_indpop$in_degree[i]
  return(it_indpop)
}
