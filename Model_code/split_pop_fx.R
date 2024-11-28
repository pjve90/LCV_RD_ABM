#' Population threshold.
#'
#' \code{population_thresh} returns the population after splitting due to enormous population size.
#' 
#' This is a function that samples a population from the one in the current iteration, in order to control population size in the simulation. The parameter \code{popsize} is the population size, which is used to define the age- and stage-structure of the population.

population_thresh <- function(it_indpop) {
  if (sum(it_indpop$stage %in% c(2, 3, 4)) > 5000) {
    nonjuv <- it_indpop[it_indpop$stage %in% c(2, 3, 4), ] 
    final <- nonjuv[sample(1:nrow(nonjuv), 2500), ]
    juveniles <- it_indpop[it_indpop$stage == 1 & it_indpop$mom_id %in% final$id, ]
    it_indpop <- rbind(final, juveniles)
  }
  
  return(it_indpop)  # Returns the modified or original input
}