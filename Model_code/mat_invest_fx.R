#' Maternal investment.
#'
#' \code{mat_invest} updates the amount of resources stored by individuals based on the maternal investment dynamics.
#' 
#' This is a function that updates the amount of resources an individual has stored based on the flow of resources due to maternal investment. The amounts discounted for the mother are based on the amount of surplus for maternal investment (\code{mom_surplus_a}). The amount of resources gained by a descendant is based on the amount of resources needed to cover the survival costs (\code{desc_need_a}). If the mother does not have sufficient resources to cover the needs of the current descendant, she invest in the next one, until she runs out of surplus.

mat_invest <- function(it_indpop){
  if(it_indpop$id[i] %in% it_descpop$mom_id == T & it_indpop$mom_surplus_a[i] > 0){#if the current individual is a mother of any descendants currently alive that need resources, and if she has a surplus above zero
    it_descpop_momsub <- as.data.frame(it_descpop[it_indpop$id == it_descpop$mom_id[i], ]) #subset the descendants of this mother
    for(desc in 1:nrow(it_descpop_momsub)){   # Loop through all of the mother's descendants, starting with the offspring with the largest need
      if(it_indpop$mom_surplus_a[i] > it_descpop_momsub$desc_need_a[desc]) { # If the mother has enough surplus to cover the need of her descendant, she invest
        it_indpop$mom_surplus_a[i] <- it_indpop$mom_surplus_a[i] - it_descpop_momsub$desc_need_a[desc] #discount the maternal investment for the mother
        it_descpop_momsub$store_a[desc]  <- it_descpop_momsub$store_a[desc] + it_descpop_momsub$desc_need_a[desc] #add the maternal investment for the descendant
        it_indpop$store_a[it_indpop$id == it_descpop_momsub$id[desc]] <- it_descpop_momsub$store_a[desc] #update the resources available of the descendant in the original database
      }
    }
  }
  return(it_indpop)
}
