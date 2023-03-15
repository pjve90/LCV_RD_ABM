#' Stochastic block model with a maximum out-degree (function).
#'
#' \code{simulate_SBM_max_degree} generates the social network using a stochastic block model with a maximum out-degree.
#' 
#' This function creates the social network based on a stochastic block model with a maximum out-degree. The parameters are the population size (\code{N}), the number of individuals per life cycle stage (\code{block_sizes}), the block matrix (\code{block_probs}), and the maximum out-degree (\code{max_deg}) and  life cycle stage of each individual (\code{it_indpop$stage}).
#' 

simulate_SBM_max_degree <- function(n, block_sizes, block_probs, max_deg, block_assignments) {
  adj_matrix <- matrix(0, nrow = n, ncol = n)
  for (i in 1:n) {
    potential_out_edges <- setdiff(1:n, i)
    prob <- block_probs[block_assignments[i], block_assignments[potential_out_edges]]
    prob[is.na(prob)] <- 0
    out_edges <- sample(potential_out_edges, size = max_deg[i], replace = TRUE, prob = prob)
    adj_matrix[i, out_edges] <- 1
  }
  return(adj_matrix)
}
