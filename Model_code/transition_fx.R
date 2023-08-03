#' Life cycle stage transition.
#'
#' \code{transition} defines whether an individual moves to the next life cycle stage or stays in the current one.
#' 
#' This is a function that defines when an individual transitions from juvenile (1) to adult (2), adult (2) to reproductive career (3) or post-reproductive (4), and from reproductive career (3) to post-reproductive (4).

#transition
transition <- function(it_indpop){
  if(it_indpop$stage[i]==1 & it_indpop$age[i] >= 10 & it_indpop$res_a[i] >= repro_thresh){ #transition juvenile to adult
    it_indpop$res_a[i] <- it_indpop$res_a[i] - repro_cost
    it_indpop$stage[i] <- 2
  }    else
    if(it_indpop$stage[i]==1 & it_indpop$age[i]>=18 ){ #forced transition from juvenile to adult
      it_indpop$stage[i] <- 2
    } else
    if(it_indpop$stage[i]==2 & it_indpop$res_a[i] >= repro_thresh){ #transition adult to reproductive career
        it_indpop$res_a[i] <- it_indpop$res_a[i] - repro_cost
        it_indpop$repro[i] <- 1
        it_indpop$stage[i] <- 3
        it_indpop$tlr[i] <- 0
        it_indpop$lro[i] <- it_indpop$lro[i] + it_indpop$repro[i]
      } 
     else
      if(it_indpop$stage[i]==2 & it_indpop$age[i]>=60 ){ #forced transition adult to post-reproductive
        it_indpop$stage[i] <- 4
      } else 
        if(it_indpop$stage[i]==3 & it_indpop$age[i] >=40 & it_indpop$res_a[i] <= repro_thresh & it_indpop$tlr[i] >= 10){ #transition reproductive career to post-reproductive career
          it_indpop$stage[i] <- 4
        }
         else
         if(it_indpop$stage[i]==3 & it_indpop$age[i]>=60 ){ #forced transition reproductive career to post-reproductive
           it_indpop$stage[i] <- 4
           }else {
            it_indpop$stage[i] <- it_indpop$stage[i]
           }
  return(it_indpop)
}
