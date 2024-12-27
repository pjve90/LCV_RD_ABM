#' Initial population.
#'
#' \code{create_population} returns the initial population for the simulations.
#' 
#' This is a function that creates the initial population for the simulation. The parameter \code{popsize} is the population size, which is used to define the age- and stage-structure of the population.

create_initialpop <- function(popsize){
  it_indpop <- data.frame(id=1:popsize, #id
                          stage=c(rep(1,length.out=popsize/4),rep(2,length.out=popsize/4),rep(3,length.out=popsize/4),rep(4,length.out=popsize/4)), #life cycle stage
                          res_a=rep(0,length.out=popsize), #resources available
                          store_a=rep(0,length.out=popsize), #stored resources
                          prod_a=rep(0,length.out=popsize), #production amount
                          mom_id=rep(NA,length.out=popsize), #mom id
                          mom_surplus=rep(NA,length.out=popsize), #identify mom surplus
                          mom_surplus_a=rep(NA,length.out=popsize), #mom surplus amount
                          desc_need=rep(0,length.out=popsize), #identify descendant need
                          desc_need_a=rep(NA,length.out=popsize), #descendant need amount
                          max_deg=rep(NA,length.out=popsize), #surplus for resource transfers
                          in_degree=rep(NA,length.out=popsize), #amount of resources received
                          out_degree=rep(NA,length.out=popsize), #amount of resources given away
                          repro=rep(NA,length.out=popsize), #reproduction output
                          lro=rep(0,length.out=popsize), #lifetime reproductive output
                          tlr=rep(0,length.out=popsize), #time since last reproduction
                          surv=rep(NA,length.out=popsize), #survival output
                          #surv2=rep(NA,length.out=popsize), #survival output
                          age=c(rep(0,length.out=popsize/4),rep(10,length.out=popsize/4),rep(15,length.out=popsize/4),rep(45,length.out=popsize/4)) #age
  )
  it_indpop$mom_id[it_indpop$stage==1] <- it_indpop$id[it_indpop$stage==3]
  return(it_indpop)
}
