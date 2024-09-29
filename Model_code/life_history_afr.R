#' Age at first reproduction (AFR).
#'
#' \code{afr} calculates the age at first reproduction of an individual.
#' 
#' The age at first reproduction of an individual is the age at which an individual has her first descendants, transitioning from an adult stage to a reproductive career stage.

afr <- function(lht_list){
  if(sum(is.na(raw_sample[[i]][raw_sample[[i]]$stage == 3,]$stage))>=1 & nrow(raw_sample[[i]][raw_sample[[i]]$stage == 3,]) <= 1){
    lht_list[[i]]$afr <- rep(NA,nrow(lht_list[[i]]))
  } else {
    age_min <- aggregate(age ~ id,
                         data=raw_sample[[i]][raw_sample[[i]]$stage == 3,],
                         min)
    # Initialize afr with NA
    lht_list[[i]]$afr <- rep(NA, nrow(lht_list[[i]]))
    
    # Loop over each individual in lht_list to assign minimum age or NA
    for (j in 1:nrow(lht_list[[i]])) {
      if (lht_list[[i]]$id[j] %in% age_min$id) {
        lht_list[[i]]$afr[j] <- age_min$age[age_min$id == lht_list[[i]]$id[j]]
      }
    }
  }
  return(lht_list[[i]])
}