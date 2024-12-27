#' Age at first reproduction (AFR).
#'
#' \code{afr} calculates the age at first reproduction of an individual.
#' 
#' The age at first reproduction of an individual is the age at which an individual has her first descendants, transitioning from an adult stage to a reproductive career stage.

afr <- function(lht_element, raw_sample_element) {
  # Deduplicate lht_element by id
  lht_element <- lht_element[!duplicated(lht_element$id), ]
  
  # Filter raw_sample_element for individuals at stage 3 (first reproduction)
  stage_3_data <- raw_sample_element[raw_sample_element$stage == 3, ]
  
  # Check if there are no individuals at stage 2
  if (nrow(stage_3_data) == 0) {
    lht_element$afr <- NA  # No adults, set asm to NA
    return(lht_element)
  }

    # Aggregate to find the minimum age at stage 3 for each id
  min_age <- aggregate(age ~ id, data = stage_3_data, min)
  
  # Initialize afr column in lht_element with NA
  lht_element$afr <- NA
  
  # Match ids and assign the minimum age at first reproduction
  lht_element$afr <- min_age$age[match(lht_element$id, min_age$id)]
  
  return(lht_element)
}
