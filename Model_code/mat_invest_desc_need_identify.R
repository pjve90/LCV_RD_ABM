#' Descendant need (identification).
#'
#' \code{desc_need} identify if an individual needs resources from her mother or not.
#' 
#' This is a function that defines if the individual needs resources or not from her mother to cover the costs of survival in a year.
#'
desc_need <- function(it_indpop){
  if(it_indpop$mom_id[i] %in% it_indpop$id == T & it_indpop$stage[i]==1 & it_indpop$store_a[i] < surv_cost){ #if your mother is alive, you are a juvenile, and don't have enough resources to cover the costs of survival
    it_indpop$desc_need[i] <- 1
  } if(it_indpop$mom_id[i] %in% it_indpop$id == T & it_indpop$stage[i]==1 & it_indpop$store_a[i] >= surv_cost){
    it_indpop$desc_need[i] <- 0
  } else{
    it_indpop$desc_need[i] <- NA
  }
  return(it_indpop$desc_need)
}
