#' Age at sexual maturity.
#'
#' \code{asm} calculates the age at sexual maturity of an individual.
#' 
#' The age at sexual maturity of an individual is the age at which an individual transitions from a juvenile to an adult.

asm <- function(lht_element, raw_sample_element) {
  # Deduplicate lht_element by id
  lht_element <- lht_element[!duplicated(lht_element$id), ]
  
  # Filter raw_sample_element for individuals at stage 2 (sexual maturity)
  stage_2_data <- raw_sample_element[raw_sample_element$stage == 2, ]
  
  # Check if there are no individuals at stage 2
  if (nrow(stage_2_data) == 0) {
    lht_element$asm <- NA  # No adults, set asm to NA
    return(lht_element)
  }
  
  # Aggregate to find the minimum age at stage 2 for each id
  min_age <- aggregate(age ~ id, data = stage_2_data, min)
  
  # Initialize asm column in lht_element with NA
  lht_element$asm <- NA
  
  # Match ids and assign the minimum age at sexual maturity
  lht_element$asm <- min_age$age[match(lht_element$id, min_age$id)]
  
  return(lht_element)
}

