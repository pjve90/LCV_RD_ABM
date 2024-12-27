#' Lifetime reproductive output (LRO).
#'
#' \code{lro} calculates the lifetime reproductive output of an individual.
#' 
#' The lifetime reproductive output of an individual is the total number of descendants that she has produced through her lifespan. It is the sum of positive reproductive outcomes (1) in \code{it_data}.

# lifetime_reproductive_output <- function(lht_list){
#   lht_list[[i]]$lro <- aggregate(raw_sample[[i]],by=list(id=raw_sample[[i]]$id),max)$lro
#   return(lht_list[[i]])
# }

lifetime_reproductive_output <- function(lht_element, raw_sample_element) {
  
  # Deduplicate lht_element by id
  lht_element <- lht_element[!duplicated(lht_element$id), ]
  
  # Filter IDs for individuals in stages 2, 3, and 4 from raw_sample_element
  valid_ids <- raw_sample_element$id[raw_sample_element$stage %in% c(2, 3, 4)]
  
  # If no valid IDs, return lht_element with lro column as NA
  if (length(valid_ids) == 0) {
    lht_element$lro <- NA
    return(lht_element)
  }
  
  # Aggregate maximum lifetime reproductive output (lro) by id for valid IDs
  filtered_data <- raw_sample_element[raw_sample_element$id %in% valid_ids, ]
  max_lro <- aggregate(lro ~ id, data = filtered_data, max)
  
  # Initialize lro column in lht_element with NA
  lht_element$lro <- NA
  
  # Match and assign maximum lro to lht_element
  lht_element$lro <- max_lro$lro[match(lht_element$id, max_lro$id)]
  
  return(lht_element)
}
