#' Population threshold.
#'
#' \code{population_thresh} returns the population after splitting due to enormous population size.
#' 
#' This is a function that samples a population from the one in the current iteration, in order to control population size in the simulation. The parameter \code{popsize} is the population size, which is used to define the age- and stage-structure of the population.
 
population_thresh <- function(it_indpop){
  if (nrow(it_indpop) > 1000){
    #Filter and sample 125 individuals from adult, reproductive career, and post-reproductive stages
    final <- do.call(rbind, lapply(2:4, function(s) {
      stage_data <- it_indpop[which(it_indpop$stage == s), ]  # Filter once
      sampled_rows <- sample(nrow(stage_data), 125)    # Sample rows
      stage_data[sampled_rows, ]                       # Return sampled data
    }))
    #Get the descendants of the mothers in the sample
    juveniles <- it_indpop[which(it_indpop$stage == 1 & it_indpop$mom_id %in% final$id), ]
    #In case there are less than 125 descendants, sample those without mothers
    if (nrow(juveniles) < 125) {
      remaining_juv <- it_indpop[which(it_indpop$stage == 1 & !(it_indpop$mom_id %in% final$id)), ]  # Filter remaining
      additional_juv <- remaining_juv[sample(1:nrow(remaining_juv), 125 - nrow(juveniles)), ]    # Sample from remaining
      juveniles <- rbind(juveniles, additional_juv)  # Combine the kids
    }
    #Combine everything
    it_indpop <- rbind(final, juveniles)
  }
  return(it_indpop)
}
