#' Total amount of resources produced.
#'
#' \code{produce_total} calculates the total amount of resources produced by an individual.
#' 
#' The total amount of resources produced by an individual throughout her whole lifespan is calculated by the sum of resources produced in each iteration.

produce_total <- function(final_ind_data){
  final_ind_data$prod_total[i] <- as.numeric(rowSums(it_data[i,grep("prod_a",colnames(it_data))],na.rm = T))
  return(final_ind_data$prod_total)
}
