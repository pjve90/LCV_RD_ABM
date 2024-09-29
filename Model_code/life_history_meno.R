#' Age at menopause.
#'
#' \code{menos} calculates the age at which an individual reaches menopause.
#' 
#' The age at menopause of an individual is the age at which an individual transitions from an adult or reproductive career stage into a post-reproductive stage.

meno <- function(lht_list){
  #check if there are adult individuals in the sample
  if(sum(is.na(raw_sample[[i]][raw_sample[[i]]$stage == 4,]$stage))>=1 & nrow(raw_sample[[i]][raw_sample[[i]]$stage == 4,]) <= 1){
    lht_list[[i]]$meno <- rep(NA,nrow(lht_list[[i]]))
  } else {
    age_min <- aggregate(age ~ id,
                         data=raw_sample[[i]][raw_sample[[i]]$stage == 4,],
                         min)
    # Initialize asm with NA
    lht_list[[i]]$meno <- rep(NA, nrow(lht_list[[i]]))
    
    # Loop over each individual in lht_list to assign minimum age or NA
    for (j in 1:nrow(lht_list[[i]])) {
      if (lht_list[[i]]$id[j] %in% age_min$id) {
        lht_list[[i]]$meno[j] <- age_min$age[age_min$id == lht_list[[i]]$id[j]]
      }
    }
  }
  return(lht_list[[i]])
}
