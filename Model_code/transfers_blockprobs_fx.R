#' Stage-specific probabilities.
#'
#' \code{create_block_probs} returns the stage-specific probabilities of resources transfers for each individual.
#' 
#' This is a function that defines the probabilities that an individual transfers resource to other individual, depending on the life cycle stage that they both are. 

create_block_probs <- function(N, block_assignments) {
  block_sizes <- c(length(it_indpop$stage[it_indpop$stage==1]),length(it_indpop$stage[it_indpop$stage==2]),length(it_indpop$stage[it_indpop$stage==3]),length(it_indpop$stage[it_indpop$stage==4]))
  num_blocks <- length(block_sizes)
  
  vectors <- vector("list", N)
  
  for (i in 1:N) {
    block <- block_assignments[i]
    vector_length <- block_sizes[block]
    
    if (block == 1) {
      vector <- c(rep(log(0.25/(1-0.25)), vector_length),
                  rep(log(0.25/(1-0.25)), vector_length),
                  rep(log(0.25/(1-0.25)), vector_length),
                  rep(log(0.25/(1-0.25)), vector_length))
    } else if (block == 2 || block == 3) {
      vector <- c(rep(log(0.75/(1-0.75)), vector_length),
                  rep(log(0.25/(1-0.25)), vector_length),
                  rep(log(0.4/(1-0.4)), vector_length),
                  rep(log(0.5/(1-0.5)), vector_length))
    } else if (block == 4) {
      vector <- c(rep(log(0.75/(1-0.75)), vector_length),
                  rep(log(0.25/(1-0.25)), vector_length),
                  rep(log(0.5/(1-0.5)), vector_length),
                  rep(log(0.4/(1-0.4)), vector_length))
    }
    
    vectors[[i]] <- vector
  }
  
  vectors
}
