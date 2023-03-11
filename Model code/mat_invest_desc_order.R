#' Preparing descendants for maternal investment.
#'
#' \code{desc_order} rearrange the descendants for the resource dynamics of maternal investment.
#' 
#' This is a function that substs and rearrange the order of the descendants in the population for the resource gain and discount due to maternal investment.
#'
desc_order <- function(it_indpop){
  #subset of descendants
  it_descpop <- it_indpop[it_indpop$desc_need == 1,]
  #order the descendants by need and mother id
  it_descpop[order(-it_descpop[,"desc_need"],it_descpop[,"mom_id"])]
  return(it_descpop)
}

