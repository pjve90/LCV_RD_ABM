#' Longevity.
#'
#' \code{longevity} calculates the longevity of an individual.
#' 
#' The longevity of an individual is the amount of years that the individual has lived, from birth to death. It is the sum of positive survival outcomes (1) in \code{it_data}.

longevity <- function(final_ind_data){
  if(it_data[i,grep("stage",colnames(it_data))][!is.na(it_data[i,grep("stage",colnames(it_data))])][1]==2){ #longevity of adults at initialisation
    final_ind_data$lng[i] <- as.numeric(rowSums(it_data[i,grep("surv",colnames(it_data))],na.rm = T)) + 10
  } else 
    if(it_data[i,grep("stage",colnames(it_data))][!is.na(it_data[i,grep("stage",colnames(it_data))])][1]==3){ #longevity of reproductive career at initialisation
      final_ind_data$lng[i] <- as.numeric(rowSums(it_data[i,grep("surv",colnames(it_data))],na.rm = T)) + 15
    } else
      if(it_data[i,grep("stage",colnames(it_data))][!is.na(it_data[i,grep("stage",colnames(it_data))])][1]==4){ #longevity of post-reproductive at initialisation
        final_ind_data$lng[i] <- as.numeric(rowSums(it_data[i,grep("surv",colnames(it_data))],na.rm = T)) + 45  
      } else{
        final_ind_data$lng[i] <- as.numeric(rowSums(it_data[i,grep("surv",colnames(it_data))],na.rm = T))
      }
  return(final_ind_data$lng)
}
