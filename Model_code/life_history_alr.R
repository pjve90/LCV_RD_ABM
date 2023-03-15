#' Age at last reproduction (ALR).
#'
#' \code{alr} calculates the age at last reproduction of an individual.
#' 
#' The age at last reproduction of an individual is the age at which an individual has her last descendant.

alr <- function(final_ind_data){
  if(it_data[i,grep("stage",colnames(it_data))][!is.na(it_data[i,grep("stage",colnames(it_data))])][1]==2 &  as.numeric(rowSums(it_data[i,grep("repro",colnames(it_data))],na.rm = T)) > 0){ #ALR of adults at initialisation
    final_ind_data$alr[i] <-
      max(cumsum(it_data[i,grep("surv",colnames(it_data))][!is.na(it_data[i,grep("surv",colnames(it_data))])])[it_data[i,grep("repro",colnames(it_data))][!is.na(it_data[i,grep("repro",colnames(it_data))])]==1]) + 10
  } else 
    if(it_data[i,grep("stage",colnames(it_data))][!is.na(it_data[i,grep("stage",colnames(it_data))])][1]==3 &  as.numeric(rowSums(it_data[i,grep("repro",colnames(it_data))],na.rm = T)) > 0){ #ALR of reproductive career at initialisation
      final_ind_data$alr[i] <-
        max(cumsum(it_data[i,grep("surv",colnames(it_data))][!is.na(it_data[i,grep("surv",colnames(it_data))])])[it_data[i,grep("repro",colnames(it_data))][!is.na(it_data[i,grep("repro",colnames(it_data))])]==1]) + 15
    } else
      if(it_data[i,grep("stage",colnames(it_data))][!is.na(it_data[i,grep("stage",colnames(it_data))])][1]==4 &  as.numeric(rowSums(it_data[i,grep("repro",colnames(it_data))],na.rm = T)) > 0){ #ALR of post-reproductive  at initialisation
        final_ind_data$alr[i] <-
          max(cumsum(it_data[i,grep("surv",colnames(it_data))][!is.na(it_data[i,grep("surv",colnames(it_data))])])[it_data[i,grep("repro",colnames(it_data))][!is.na(it_data[i,grep("repro",colnames(it_data))])]==1]) + 45  
      } else
        if(as.numeric(rowSums(it_data[i,grep("repro",colnames(it_data))],na.rm = T)) == 0){
          final_ind_data$alr[i] <- NA 
        }else{
          final_ind_data$alr[i] <- as.numeric(rowSums(it_data[i,grep("surv",colnames(it_data))],na.rm = T))
        }
  return(final_ind_data$alr)
}
