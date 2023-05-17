#' Survival.
#'
#' \code{survive} defines whether an individual survives (1) or not (0) in that iteration.
#' 
#' This is a function that defines whether an individual survives or not, depending on the amount of resources stored by the individual. If the individual has more resources than the survival cost then she will survive (1), if she is below that amount then she will die (0).

old_age_survival_offset<-c(rep(1,70),1.2^(1:50))

survive <- function(it_indpop){
  if(it_indpop$store_a[i] >= (surv_cost*old_age_survival_offset[it_indpop$age[i]] )){
      it_indpop$surv[i] <- 1
    } else{
      it_indpop$surv[i] <- 0
    }
  return(it_indpop$surv)
}

