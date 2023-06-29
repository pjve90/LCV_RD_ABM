#' Stochastic block model with a maximum out-degree (function).
#'
#' \code{simulate_SBM_max_degree} generates the social network using a stochastic block model with a maximum out-degree.
#' 
#' This function creates the social network based on a stochastic block model with a maximum out-degree. The parameters are the population size (\code{N}), the number of individuals per life cycle stage (\code{block_sizes}), the block matrix (\code{block_probs}), and the maximum out-degree (\code{max_deg}) and  life cycle stage of each individual (\code{it_indpop$stage}).
#' 

simulate_SBM_multinomial <- function(N, max_deg, block_probs, self_noms) {
  network <- matrix(0,N,N)	
  for(i in 1:N){
    network[i, ] <- t(rmultinom(n = 1, size = max_deg[i], prob = softmax(self_noms[[i]] + block_probs[[i]])))
  }
  kept_packets <- diag(network)
  diag(network) <- 0 			
  return(list(network = as.matrix(network), kept_packets = kept_packets))
}