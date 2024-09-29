#' Age at last reproduction (ALR).
#'
#' \code{alr} calculates the age at last reproduction of an individual.
#' 
#' The age at last reproduction of an individual is the age at which an individual has her last descendant.

alr <- function(lht_list){
  if(sum(is.na(raw_sample[[i]][raw_sample[[i]]$lro >= 1 & raw_sample[[i]]$stage == 3,]$stage))>=1 & nrow(raw_sample[[i]][raw_sample[[i]]$stage == 3,]) <= 1){
    lht_list[[i]]$alr <- rep(NA,nrow(lht_list[[i]]))
  } else {
    age_max <- aggregate(age ~ id,
                         data=raw_sample[[i]][raw_sample[[i]]$lro >= 1& raw_sample[[i]]$stage == 3,],
                         max)
    tlr <- aggregate(tlr ~ id,
                     data=raw_sample[[i]][raw_sample[[i]]$lro >= 1& raw_sample[[i]]$stage == 3,],
                     function(x)tail(x,1))
    # Initialize alr with NA
    lht_list[[i]]$alr <- rep(NA, nrow(lht_list[[i]]))
    
    # Loop over each individual in lht_list to assign minimum age or NA
    for (j in 1:nrow(lht_list[[i]])) {
      if (lht_list[[i]]$id[j] %in% age_max$id) {
        lht_list[[i]]$alr[j] <- age_max$age[age_max$id == lht_list[[i]]$id[j]] - tlr$tlr[tlr$id == lht_list[[i]]$id[j]]
      }
    }
  }
  return(lht_list[[i]])
}
