#' Preparing descendants for maternal investment.
#'
#' \code{desc_order} rearrange the descendants for the resource dynamics of maternal investment.
#' 
#' This is a function that substs and rearrange the order of the descendants in the population for the resource gain and discount due to maternal investment.
#'
desc_order <- function(it_indpop){
  if(sum(it_indpop$desc_need) > 0){
  #subset of descendants
  it_descpop <- it_indpop[it_indpop$desc_need == 1,]
  #order the descendants by need and mother id
  it_descpop[order(-it_descpop[,"desc_need"],it_descpop[,"mom_id"]),]
  } else{
  it_descpop <- data.frame(id=NA, #id
                                stage=NA, #life cycle stage
                                store_a=NA, #stored resources
                                prod_o=NA, #production output
                                prod_a=NA, #production amount
                                mom_id=NA, #mom id
                                mom_surplus=NA, #identify mom surplus
                                mom_surplus_a=NA, #mom surplus amount
                                desc_need=NA, #identify descendant need
                                desc_need_a=NA, #descendant need amount
                                max_deg=NA, #surplus for resource transfers
                                out_degree=NA, #amount of resources given away
                                in_degree=NA, #amount of resources received
                                out_degree=NA, #amount of resources given away
                                repro=NA, #reproduction output
                                lro=NA, #lifetime reproductive output
                                tlr=NA, #time since last reproduction
                                surv=NA, #survival output
                                age=NA #age
  )
  }
  return(it_descpop)
}

