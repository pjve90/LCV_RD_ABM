###########################################################################################################

# Simulate network with SBM and node-specific max degree

###########################################################################################################
# Load packages
#install.packages("igraph")
library(igraph)
library(ggplot2)

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

#create a data frame for individuals and their IDs
indiv <- data.frame(
			ID = 1:N, 
			block_assignments = rep(1:4, each=25)
				)

# Specify the number of nodes and the number of blocks
N <- 100
num_blocks <- 4
block_sizes <- c(25, 25, 25, 25)

# Define maximum out-degree for each node
# Here we create a vector of max outdegrees for each block
# But you can just supply the vector of food return counts
food_returns <- c(rep(3, 25), rep(20, 25), rep(25, 25), rep(5, 25))
max_deg <- food_returns

# Specify the probability of a connection between each pair of blocks
# This is the probability of each block sharing with each other block
# e.g., think of block 1 as kids and they are very unlike to share
# column 2 could be young adults and they're likely to share most with children and elderly (i.e., elements 1 and 4 in that row)
# Need to play around with the probabilities
block_probs <- matrix(c(0.001, 0.05, 0.05, 0.05,
                        0.001, 0.01, 0.01, 0.01,
                        0.001, 0.01, 0.02, 0.03,
                        0.001, 0.03, 0.01, 0.02),
                      nrow = num_blocks, ncol = num_blocks)
block_probs

# Generate the network
network <- simulate_SBM_max_degree(N, block_sizes, block_probs, max_deg, indiv$block_assignments)

#number of ties observed in the network 
sum(network)

# Look to see who's sending and receiving ties
indiv$out_degree <- rowSums(network)
indiv$in_degree <- colSums(network)
#check how it looks like
indiv
#plot it!
#prepare data
data <- reshape(indiv, 
        direction = "long",
        varying = list(names(indiv)[3:4]),
        v.names = "value",
        idvar = c("ID", "block_assignments"),
        timevar = "degree",
        times = c("outdegree", "indegree")
       )

y_limit_max <- range(data$value)[2]

# Take a quick look at the distributions of in and out-degree
  ggplot(data, aes(fill=factor(degree), y=value, x=factor(block_assignments))) + 
    geom_violin(position="dodge", alpha=0.5) +
    scale_fill_manual(values=c("darkseagreen3", "deepskyblue4"), name="fill") +
    theme_bw()  +
    xlab("Age Class") +
    ylab("Number of Ties") +
    ylim(0,y_limit_max)

# Given eventual changing block assignments (e.g., individuals age), should probably create a unique ID for individuals and match on that
g <- graph_from_adjacency_matrix(network)
V(g)$ID <- indiv$ID
V(g)$stage_class <- indiv$block_assignments[match(V(g)$ID, indiv$ID)]
V(g)$colour[V(g)$stage_class == 1] <- "darkseagreen3" #juvenile
V(g)$colour[V(g)$stage_class == 2] <- "goldenrod3" #young adult
V(g)$colour[V(g)$stage_class == 3] <- "deepskyblue4" #reproductive career
V(g)$colour[V(g)$stage_class == 4] <- "darkorchid4" #post-reproductive

edge_density(g)
reciprocity(g)
transitivity(g)
centralization.degree(g, "out")
centralization.degree(g, "in")

# Lets plot to see what the network looks like
# We'll colour by stage class 
plot(g, edge.arrow.size=0.02, vertex.size=5,
      vertex.label = NA, vertex.color = V(g)$colour, edge.curved=0.4, layout = layout_circle)

