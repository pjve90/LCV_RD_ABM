#' Multinomial stochastic block model with maximum out degree.
#'
#' \code{simulate_SBM_multinomial} generates the social network using a multinomial stochastic block model with a maximum out-degree.
#' 
#' This function creates the social network based on a multinomial stochastic block model with a maximum out-degree. The parameters are the population size (\code{N}), the maximum out-degree (\code{max_deg}), the individual stage-specific block probabilities (\code{block_probs}), and the probability of self-sharing (\code{self_noms}).
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