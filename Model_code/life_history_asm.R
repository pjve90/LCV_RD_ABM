#' Age at sexual maturity.
#'
#' \code{asm} calculates the age at sexual maturity of an individual.
#' 
#' The age at sexual maturity of an individual is the age at which an individual transitions from a juvenile to an adult.

asm <- function(lht_list){
  #check if there are adult individuals in the sample
  if(sum(is.na(raw_sample[[i]][raw_sample[[i]]$stage == 2,]$stage))>=1 & nrow(raw_sample[[i]][raw_sample[[i]]$stage == 2,]) <= 1){
    lht_list[[i]]$asm <- rep(NA,nrow(lht_list[[i]]))
  } else {
    age_min <- aggregate(age ~ id,
                         data=raw_sample[[i]][raw_sample[[i]]$stage == 2,],
                         min)
    # Initialize asm with NA
    lht_list[[i]]$asm <- rep(NA, nrow(lht_list[[i]]))
    
    # Loop over each individual in lht_list to assign minimum age or NA
    for (j in 1:nrow(lht_list[[i]])) {
      if (lht_list[[i]]$id[j] %in% age_min$id) {
        lht_list[[i]]$asm[j] <- age_min$age[age_min$id == lht_list[[i]]$id[j]]
      }
    }
  }
  return(lht_list[[i]])
}


