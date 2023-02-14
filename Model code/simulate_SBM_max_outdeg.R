###########################################################################################################

# Simulate network with SBM and node-specific max degree

###########################################################################################################
# Load packages
library(igraph)

# Load function
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

###########################################################################################################

# Specify the number of nodes and the number of blocks
N <- 100
num_blocks <- 4
block_sizes <- c(25, 25, 25, 25)

indiv <- data.frame(
			ID = 1:N, 
			block_assignments = rep(1:4, each=25)
				)

# Define maximum out-degree for each node
# Here we create a vector of max outdegrees for each block
# But you can just supply the vector of food return counts
food_returns <- c(rep(3, 25), rep(20, 25), rep(25, 25), rep(5, 25))
max_deg <- food_returns

# Specify the probability of a connection between each pair of blocks
# This is the probability of each block sharing with each other block
# e.g., think of block 1 as kids and they are very unlike to share
# row 2 could be young adults and they're likely to share most with children and elderly (i.e., elements 1 and 4 in that row)
# Need to play around with the probabilities
block_probs <- matrix(c(0.1, 0.6, 0.2, 0.1,
                        0.4, 0.05, 0.05, 0.4,
                        0.8, 0.1, 0.01, 0.8,
                        0.1, 0.5, 0.4, 0.1),
                      nrow = num_blocks, ncol = num_blocks)
block_probs

# Generate the network
<<<<<<< HEAD
network <- simulate_SBM_max_degree(N, block_sizes, block_probs, max_deg, block_assignments)
=======
network <- simulate_SBM_max_degree(N, block_sizes, block_probs, max_deg, indiv$block_assignments)

# Given eventual changing block assignments (e.g., individuals age), should probably create a unique ID for individuals and match on that
g <- graph_from_adjacency_matrix(network)
V(g)$ID <- indiv$ID
V(g)$age_class <- indiv$block_assignments[match(V(g)$ID, indiv$ID)]
V(g)$colour[V(g)$age_class == 1] <- "darkseagreen3"
V(g)$colour[V(g)$age_class == 2] <- "goldenrod3"
V(g)$colour[V(g)$age_class == 3] <- "deepskyblue4"
V(g)$colour[V(g)$age_class == 4] <- "darkorchid4"
>>>>>>> 6a3215209819179f773048d43538958c07e10a3c

# Lets plot to see what the network looks like
# We'll colour by age class 
plot(g, edge.arrow.size=0.15, vertex.size=5,
      vertex.label = NA, vertex.color = V(g)$colour, edge.curved=0.4, layout = layout_nicely)
