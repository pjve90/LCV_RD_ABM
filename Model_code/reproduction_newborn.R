#' Adding newborns.
#'
#' \code{newborns} adds with the new individuals in the population.
#' 
#' This is a function that calculates and adds the newborns in the population, based on the amount of reproductive events that happen in that year.

newborns <- function(new_it_indpop){
  if(sum(it_indpop$repro) > 0){
    new_it_indpop <- data.frame(id=max_id+1:sum(it_indpop[which(it_indpop$repro==1),"repro"]), #id
                            stage=rep(1,length.out=sum(it_indpop[which(it_indpop$repro==1),"repro"])), #life cycle stage
                            store_a=rep(surv_cost,length.out=sum(it_indpop[which(it_indpop$repro==1),"repro"])), #stored resources
                            prod_a=rep(0,length.out=sum(it_indpop[which(it_indpop$repro==1),"repro"])), #production amount
                            mom_id=it_indpop$id[it_indpop$repro==1], #mom id
                            mom_surplus=rep(NA,length.out=sum(it_indpop[which(it_indpop$repro==1),"repro"])), #identify mom surplus
                            mom_surplus_a=rep(NA,length.out=sum(it_indpop[which(it_indpop$repro==1),"repro"])), #mom surplus amount
                            desc_need=rep(0,length.out=sum(it_indpop[which(it_indpop$repro==1),"repro"])), #identify descendant need
                            desc_need_a=rep(NA,length.out=sum(it_indpop[which(it_indpop$repro==1),"repro"])), #descendant need amount
                            max_deg=rep(NA,length.out=sum(it_indpop[which(it_indpop$repro==1),"repro"])), #surplus for resource transfers
                            in_degree=rep(NA,length.out=sum(it_indpop[which(it_indpop$repro==1),"repro"])), #amount of resources received
                            out_degree=rep(NA,length.out=sum(it_indpop[which(it_indpop$repro==1),"repro"])), #amount of resources given away
                            repro=rep(NA,length.out=sum(it_indpop[which(it_indpop$repro==1),"repro"])), #reproduction output
                            lro=rep(0,length.out=sum(it_indpop[which(it_indpop$repro==1),"repro"])), #lifetime reproductive output
                            tlr=rep(0,length.out=sum(it_indpop[which(it_indpop$repro==1),"repro"])), #time since last reproduction
                            surv=rep(1,length.out=sum(it_indpop[which(it_indpop$repro==1),"repro"])), #survival output
                            age=c(rep(0,length.out=sum(it_indpop[which(it_indpop$repro==1),"repro"]))) #age
    )
    it_indpop <- rbind(it_indpop,new_it_indpop) #combine original population with newborns
  } else{
    new_it_indpop <- data.frame(id=NA, #id
                                stage=NA, #life cycle stage
                                store_a=NA, #stored resources
                                prod_a=NA, #production amount
                                mom_id=NA, #mom id
                                mom_surplus=NA, #identify mom surplus
                                mom_surplus_a=NA, #mom surplus amount
                                desc_need=NA, #identify descendant need
                                desc_need_a=NA, #descendant need amount
                                max_deg=NA, #surplus for resource transfers
                                out_degree=NA, #amount of resources given away
                                in_degree=NA, #amount of resources received
                                repro=NA, #reproduction output
                                lro=NA, #lifetime reproductive output
                                tlr=NA, #time since last reproduction
                                surv=NA, #survival output
                                age=NA #age
    )
  }
  return(new_it_indpop)
}
