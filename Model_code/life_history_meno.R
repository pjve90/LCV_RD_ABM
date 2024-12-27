#' Age at menopause.
#'
#' \code{menos} calculates the age at which an individual reaches menopause.
#' 
#' The age at menopause of an individual is the age at which an individual transitions from an adult or reproductive career stage into a post-reproductive stage.

meno <- function(lht_element, raw_sample_element) {
  # Deduplicate lht_element by id
  lht_element <- lht_element[!duplicated(lht_element$id), ]
  
  # Filter raw_sample_element for individuals in stage 4 (menopause)
  relevant_data <- raw_sample_element[raw_sample_element$stage == 4, ]
  
  # Check if there are no relevant individuals
  if (nrow(relevant_data) == 0) {
    lht_element$meno <- NA  # No relevant individuals, set alr to NA
    return(lht_element)
  }
  
  # Aggregate to find the minimum age for each id
  min_age <- aggregate(age ~ id, data = relevant_data, min)
  
  # Initialize meno column in lht_element with NA
  lht_element$meno <- NA
  
  # Match ids and assign the minimum age for menopause
  lht_element$meno <- min_age$age[match(lht_element$id, min_age$id)]
  
  return(lht_element)
}
