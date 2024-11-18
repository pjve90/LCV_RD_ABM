#' Population threshold.
#'
#' \code{population_thresh} returns the population after splitting due to enormous population size.
#' 
#' This is a function that samples a population from the one in the current iteration, in order to control population size in the simulation. The parameter \code{popsize} is the population size, which is used to define the age- and stage-structure of the population.

population_thresh <- function(it_indpop){
  if (nrow(it_indpop) > 1000000){
    # Filter and sample individuals
    final <- do.call(rbind, lapply(2:4, function(s) {
      stage_data <- it_indpop[it_indpop$stage == s, ]  # Filter once
      if (nrow(stage_data) > 0) {  # Check if stage_data is not empty
        sampled_rows <- sample(nrow(stage_data), min(250000, nrow(stage_data)), replace = FALSE)  # Adjust sample size
        stage_data[sampled_rows, ]  # Return sampled data
      } else {
        NULL  # No data for this stage
      }
    }))

    # Get the descendants of the mothers in the sample
    juveniles <- it_indpop[it_indpop$stage == 1 & it_indpop$mom_id %in% final$id, ]

    # Combine everything
    it_indpop <- rbind(final, juveniles)
  }
  return(it_indpop)
}

# population_thresh <- function(it_indpop){
#   if (nrow(it_indpop) > 3000){
#     it_indpop$surv2[i] <- rbinom(1,1,surv_curve[(it_indpop$age[i]+1)])
#   }
#   return(it_indpop$surv2)
# }
