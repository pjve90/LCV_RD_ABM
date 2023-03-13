#' Lifetime reproductive output (LRO).
#'
#' \code{lro} calculates the lifetime reproductive output of an individual.
#' 
#' The lifetime reproductive output of an individual is the total number of descendants that she has produced through her lifespan. It is the sum of positive reproductive outcomes (1) in \code{it_data}.

lro <- function(final_ind_data){
  if(it_data[i,grep("stage",colnames(it_data))][!is.na(it_data[i,grep("stage",colnames(it_data))])][1]==3){ #for reproductive career at initialisation
    final_ind_data$lro[i] <- as.numeric(rowSums(it_data[i,grep("repro",colnames(it_data))],na.rm = T)) + 1 #you must have one descendant to be in the reproductive career stage
  } else{
    final_ind_data$lro[i] <- as.numeric(rowSums(it_data[i,grep("repro",colnames(it_data))],na.rm = T))
  }
  return(final_ind_data$lro)
}
