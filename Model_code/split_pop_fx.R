#' Population threshold.
#'
#' \code{population_thresh} returns the population after splitting due to enormous population size.
#' 
#' This is a function that samples a population from the one in the current iteration, in order to control population size in the simulation. The parameter \code{popsize} is the population size, which is used to define the age- and stage-structure of the population.

population_thresh <- function(it_indpop) {
  # Check if the non-juvenile population exceeds the threshold
  if (sum(it_indpop$stage %in% c(2, 3, 4)) > 5000) {
    # Sampling adults
    sampled_adults <- if (sum(it_indpop$stage == 2) > 0) {
      pop_size <- sum(it_indpop$stage == 2) #define population size
      sample_size <- if (pop_size == 1) 1 else floor(pop_size / 2) #define sample size
      it_indpop[it_indpop$stage == 2, ][sample(1:pop_size, size = sample_size), ] #sample
    } else {
      it_indpop[FALSE, ] # Return an empty data frame
    }
    # Sampling reproductive career
    sampled_reproductives <- if (sum(it_indpop$stage == 3) > 0) {
      pop_size <- sum(it_indpop$stage == 3) #define population size
      sample_size <- if (pop_size == 1) 1 else floor(pop_size / 2) #define sample size
      it_indpop[it_indpop$stage == 3, ][sample(1:pop_size, size = sample_size), ] #sample
    } else {
      it_indpop[FALSE, ]
    }
    # Sampling post-reproductive
    sampled_postreproductives <- if (sum(it_indpop$stage == 4) > 0) {
      pop_size <- sum(it_indpop$stage == 4) #define population size
      sample_size <- if (pop_size == 1) 1 else floor(pop_size / 2) #define sample size
      it_indpop[it_indpop$stage == 4, ][sample(1:pop_size, size = sample_size), ] #sample
    } else {
      it_indpop[FALSE, ]
    }
    
    # Combine sampled individuals
    final <- rbind(sampled_adults, sampled_reproductives, sampled_postreproductives)
    
    # Get juveniles dependent on sampled reproductive individuals
    juveniles <- it_indpop[it_indpop$stage == 1 & it_indpop$mom_id %in% final$id, ]
    
    # Combine all sampled individuals into the final population
    it_indpop <- rbind(final, juveniles)
  }
  
  return(it_indpop)
}
