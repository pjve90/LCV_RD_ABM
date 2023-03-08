#' Adding newborns.
#'
#' \code{newborns} adds with the new individuals in the population.
#' 
#' This is a function that calculates and adds the newborns in the population, based on the amount of reproductive events that happen in that year.

newborns <- function(new_it_indpop){
  if(sum(it_indpop$repro) > 0){
    new_it_indpop <- data.frame(id=max_id+1:sum(it_indpop[which(it_indpop$repro==1),"repro"]),surv=rep(NA,length.out=sum(it_indpop[which(it_indpop$repro==1),"repro"])),age=rep(0,length.out=sum(it_indpop[which(it_indpop$repro==1),"repro"])),prod_o=rep(NA,length.out=sum(it_indpop[which(it_indpop$repro==1),"repro"])),prod_a=rep(0,length.out=sum(it_indpop[which(it_indpop$repro==1),"repro"])),store_a=rep(surv_c_age[1],length.out=sum(it_indpop[which(it_indpop$repro==1),"repro"])),repro=rep(NA,length.out=sum(it_indpop[which(it_indpop$repro==1),"repro"])),stage=rep(0,length.out=sum(it_indpop[which(it_indpop$repro==1),"repro"],mom_id=it_indpop$id[it_indpop$repro==1,]))) #create data with newborns
    it_indpop <- rbind(it_indpop,new_it_indpop) #combine original population with newborns
  } else{
    new_it_indpop <- data.frame(id=NA,surv=NA,age=NA,prod_o=NA,prod_a=NA,store_a=NA,repro=NA,stage=NA)
  }
  return(new_it_indpop)
}
