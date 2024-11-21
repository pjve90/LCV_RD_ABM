#' Individual stage-specific probabilities.
#'
#' \code{create_block_probs} returns the stage-specific probabilities of resources transfers for each individual.
#' 
#' This is a function that defines the probabilities that an individual transfers resource to other individual, depending on the life cycle stage that they both are. The parameters are the population size (\code{N}) and the life cycle stage of every individual in the population (\code{block_assignments}).

create_block_probs <- function(N, block_assignments) {
  block_sizes <- c(length(it_indpop$stage[it_indpop$stage==1]),length(it_indpop$stage[it_indpop$stage==2]),length(it_indpop$stage[it_indpop$stage==3]),length(it_indpop$stage[it_indpop$stage==4]))
  
  vectors <- vector("list", N)
  
  for (i in 1:N) {
    block <- block_assignments[i]
    
    if (block == 1) {
      vector <- c(rep(logodds_list[[m]][1,1], block_sizes[1]),
                  rep(logodds_list[[m]][1,2], block_sizes[2]),
                  rep(logodds_list[[m]][1,3], block_sizes[3]),
                  rep(logodds_list[[m]][1,4], block_sizes[4]))
    } else if (block == 2) {
      vector <- c(rep(logodds_list[[m]][2,1], block_sizes[1]),
                  rep(logodds_list[[m]][2,2], block_sizes[2]),
                  rep(logodds_list[[m]][2,3], block_sizes[3]),
                  rep(logodds_list[[m]][2,4], block_sizes[4]))
    } else if (block == 3) {
      vector <- c(rep(logodds_list[[m]][3,1], block_sizes[1]),
                  rep(logodds_list[[m]][3,2], block_sizes[2]),
                  rep(logodds_list[[m]][3,3], block_sizes[3]),
                  rep(logodds_list[[m]][3,4], block_sizes[4]))
    } else if (block == 4) {
      vector <- c(rep(logodds_list[[m]][4,1], block_sizes[1]),
                  rep(logodds_list[[m]][4,2], block_sizes[2]),
                  rep(logodds_list[[m]][4,3], block_sizes[3]),
                  rep(logodds_list[[m]][4,4], block_sizes[4]))
    }
    
    vectors[[i]] <- vector
  }
  
  vectors
}
