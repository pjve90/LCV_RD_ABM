#' Age at last reproduction (ALR).
#'
#' \code{alr} calculates the age at last reproduction of an individual.
#' 
#' The age at last reproduction of an individual is the age at which an individual has her last descendant.

alr <- function(lht_element, raw_sample_element) {
  # Deduplicate lht_element by id
  lht_element <- lht_element[!duplicated(lht_element$id), ]
  
  # Filter raw_sample_element for individuals with lro >= 1 and stage == 3
  relevant_data <- raw_sample_element[raw_sample_element$lro >= 1 & raw_sample_element$stage == 3, ]
  
  # Check if there are no relevant individuals
  if (nrow(relevant_data) == 0) {
    lht_element$alr <- NA  # No relevant individuals, set alr to NA
    return(lht_element)
  }

  # Aggregate to find the maximum age and last lro
  max_age <- aggregate(age ~ id, data = relevant_data, max)
  last_lro <- aggregate(tlr ~ id, data = relevant_data, function(x) tail(x, 1))
  
  # Initialize alr column in lht_element with NA
  lht_element$alr <- NA
  
  # Match ids and calculate alr as maximum age - last lro
  lht_element$alr <- max_age$age[match(lht_element$id, max_age$id)] - 
    last_lro$tlr[match(lht_element$id, last_lro$id)]
  
  return(lht_element)
}
