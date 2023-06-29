###########################################################################################################

# Simulate network with SBM and node-specific max degree

###########################################################################################################
# Load softmax
softmax <- function(par){
  n.par <- length(par)
  par1 <- sort(par, decreasing = TRUE)
  Lk <- par1[1]
  for (k in 1:(n.par-1)) {
    Lk <- max(par1[k+1], Lk) + log1p(exp(-abs(par1[k+1] - Lk)))
  }
  val <- exp(par - Lk)
  return(val)
}

create_self_noms <- function(N, probs) {
  vectors <- vector("list", N)
  for (i in 1:N) {
    vectors[[i]] <- rep(0, N)
    vectors[[i]][i] <- probs
  }
  return(vectors)
}

create_block_probs <- function(N, block_assignments) {
  block_sizes <- table(block_assignments)
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
# Load function
simulate_SBM_multinomial <- function(N, max_deg, block_probs, self_noms) {
		network <- matrix(0,N,N)	
			for(i in 1:N){
        		network[i, ] <- t(rmultinom(n = 1, size = max_deg[i], prob = softmax(self_noms[[i]] + block_probs[[i]])))
              			}
             kept_packets <- diag(network)
             diag(network) <- 0 			
             return(list(network = as.matrix(network), kept_packets = kept_packets))
                }

###########################################################################################################
N <- 100
food_returns <- c(rep(3, N/4), rep(5, N/4), rep(4, N/4), rep(3, N/4))
block_assignments <- rep(1:4, each=N/4)
simple_block_probs <- c(rep(0.1, N/4), rep(0.001, N/4), rep(0.001,N/4), rep(0.05, N/4))
block_probs <- create_block_probs(N, block_assignments)

self_noms <- create_self_noms(N, 1)


network <- simulate_SBM_multinomial(N, food_returns,block_probs, self_noms)
g_net <- graph_from_adjacency_matrix(network$network)
V(g_net)$stage_class <- block_assignments
V(g_net)$colour[V(g_net)$stage_class == 1] <- "darkseagreen3" #juvenile
V(g_net)$colour[V(g_net)$stage_class == 2] <- "goldenrod3" #adult
V(g_net)$colour[V(g_net)$stage_class == 3] <- "deepskyblue4" #reproductive career
V(g_net)$colour[V(g_net)$stage_class == 4] <- "darkorchid4" #post-reproductive
# Lets plot to see what the network looks like
# We'll colour by stage class
library(igraph)
plot(g_net, edge.arrow.size=1, vertex.size=5,
      vertex.label = NA, vertex.color = V(g_net)$colour, edge.curved=0.4, layout = layout_nicely)

