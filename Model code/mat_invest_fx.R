#' Maternal investment.
#'
#' \code{mat_invest} updates the amount of resources stored by individuals based on the maternal investment dynamics.
#' 
#' This is a function that updates the amount of resources an individual has stored based on the flow of resources due to maternal investment. The amounts discounted for the mother are based on the amount of surplus for maternal investment (\code{mom_surplus_a}). The amount of resources gained by a descendant is based on the amount of resources needed to cover the survival costs (\code{desc_need_a}). If the mother does not have sufficient resources to cover the needs of the current descendant, she invest in the next one, until she runs out of surplus.

mat_invest <- function(it_indpop){
  if(it_indpop$id[i] %in% it_descpop$mom_id & it_indpop$mom_surplus_a[i] > 0){#if the current individual is a mother of any descendants currently alive and if she has a surplus above zero
    it_descpop_momsub <- it_descpop[it_indpop$id[i] == it_descpop$mom_id, ] #subset the descendants of this mother
    for(desc in 1:nrow(it_descpop_momsub)){.   # Loop through all of the mother's descendants, starting with the offspring with the largest need
      if(it_indpop$mom_surplus_a[i] > it_descpop_momsub$desc_need_a[desc]) { # If the mother does not have sufficient resources to cover the needs of the current descendant, she does not invest
        it_indpop$mom_surplus_a[i] <- it_indpop$mom_surplus_a[i] - it_descpop_momsub$desc_need_a[desc,] #discount the maternal investment for the mother
        it_descpop_momsub$res_a[desc]  <- it_descpop_momsub$res_a[desc] + it_descpop_momsub$desc_need_a[desc] #add the maternal investment for the descendant
        it_indpop$store_a[it_indpop$id == it_descpop_momsub$id[desc]] <- it_descpop_momsub$res_a[desc] #update the resources available of the descendant in the original database
      }
    }
  }
  return(it_indpop)
}
