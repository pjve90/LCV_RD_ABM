#' Maternal investment.
#'
#' \code{mat_invest} updates the amount of resources stored by individuals based on the maternal investment dynamics.
#' 
#' This is a function that updates the amount of resources an individual has stored based on the flow of resources due to maternal investment. The amounts discounted for the mother are based on the amount of surplus for maternal investment (\code{mom_surplus_a}). The amount of resources gained by a descendant is based on the amount of resources needed to cover the survival costs (\code{desc_need_a}). If the mother does not have sufficient resources to cover the needs of the current descendant, she invest in the next one, until she runs out of surplus.

mat_invest <- function(it_indpop){
  if(it_mompop$mom_surplus_a[i] > 0){ #if the current individual is a mother of any descendants currently alive that need resources, and if she has a surplus of resources
      for(desc in 1:nrow(it_descpop_momsub)){   # Loop through all of the mother's descendants, starting with the offspring with the largest need
      if(it_mompop$mom_surplus_a[i] >= it_descpop_momsub$desc_need_a[desc]) { # If the mother has enough surplus to cover the need of her descendant, she invest
        it_mompop$mom_surplus_a[i] <- it_mompop$mom_surplus_a[i] - it_descpop_momsub$desc_need_a[desc] #discount the maternal investment for the mother
        it_descpop_momsub$res_a[desc]  <- it_descpop_momsub$res_a[desc] + it_descpop_momsub$desc_need_a[desc] #add the maternal investment for the descendant
        it_descpop$res_a[it_descpop$id == it_descpop_momsub] <- it_descpop_momsub$res_a
      }
    }
    if(it_indpop$mom_surplus[it_mompop$id[i] %in% it_indpop$id]>0 ) {
    it_indpop$res_a[it_mompop$id[i] %in% it_indpop$id] <- it_mompop$mom_surplus_a[i]+surv_cost
    }
    it_indpop$res_a[it_indpop$id[i] %in% it_descpop$id] <- it_descpop$res_a[i]
  }
return(it_indpop)
}
