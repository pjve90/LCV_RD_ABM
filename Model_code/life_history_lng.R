#' Longevity.
#'
#' \code{longevity} calculates the longevity of an individual.
#' 
#' The longevity of an individual is the amount of years that the individual has lived, from birth to death. It is the maximum age of an individual recorded in \code{it_dataf}.

longevity <- function(lht_element, raw_sample_element) {
  # Ensure lht_element has unique rows by id
  lht_element <- lht_element[!duplicated(lht_element$id), ]
  
  # Aggregate maximum age by id
  max_age <- aggregate(age ~ id, data = raw_sample_element, max)
  
  # Initialize lng column in lht_element with NA
  lht_element$lng <- NA
  
  # Match the ids and assign the maximum age
  lht_element$lng <- max_age$age[match(lht_element$id, max_age$id)]
  
  return(lht_element)
}
