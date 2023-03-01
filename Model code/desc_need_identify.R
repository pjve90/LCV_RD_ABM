#' Descendant need (identification).
#'
#' \code{desc_need} identify if an individual needs resources from her mother or not.
#' 
#' This is a function that defines if the individual needs resources or not from her mother to cover the costs of survival in a year.
#'
desc_need <- function(it_indpop){
  if(it_indpop$mom_id[i] %in% it_indpop$id & it_indpop$stage[i]==1 & it_indpop$sotre_a[i] < surv_effort){ #if your mother is alive, you are a juvenile, and don't have enough resources to cover the costs of survival
    it_indpop$desc_need[i] <- 1
  } else{
    it_indpop$desc_need[i] <- 0
  }
}
