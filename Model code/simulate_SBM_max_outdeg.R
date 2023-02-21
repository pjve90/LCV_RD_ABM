# Simulate network with SBM and node-specific max degree ----

# Load packages
#install.packages("igraph")
library(igraph)
#install.packages("ggplot2")
library(ggplot2)

## Function to simulate SBM with max out degre ----

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

### Set the parameters ----

# Specify the number of nodes and the number of blocks
N <- 100
num_blocks <- 4
block_sizes <- c(25, 25, 25, 25)

# Given eventual changing block assignments (e.g., individuals age), should probably create a unique ID for individuals and match on that
#create a data frame for individuals and their IDs
indiv <- data.frame(
  ID = 1:N, 
  block_assignments = rep(1:4, each=25)
)

# Define maximum out-degree for each node
# Here we create a vector of max outdegrees for each block
# But you can just supply the vector of food return counts
food_returns <- c(rep(3, 25), rep(20, 25), rep(20, 25), rep(15, 25))
max_deg <- food_returns

# Specify the probability of a connection between each pair of blocks
# This is the probability of each block sharing with each other block
# e.g., think of block 1 as kids and they are very unlike to share
# column 2 could be young adults and they're likely to share most with children and elderly (i.e., elements 1 and 4 in that column)
# Need to play around with the probabilities
block_probs <- matrix(c(0.001, 0.01, 0.01, 0.01,
                        0.001, 0.001, 0.001, 0.001,
                        0.001, 0.001, 0.001, 0.001,
                        0.001, 0.01, 0.001, 0.001),
                      nrow = num_blocks, ncol = num_blocks)
block_probs

### Generate the network ----

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
    xlab("Life cycle stage") +
    ylab("Number of Ties") +
    ylim(0,y_limit_max)

#prepare network plot
g <- graph_from_adjacency_matrix(network)
V(g)$ID <- indiv$ID
V(g)$stage_class <- indiv$block_assignments[match(V(g)$ID, indiv$ID)]
V(g)$colour[V(g)$stage_class == 1] <- "darkseagreen3" #juvenile
V(g)$colour[V(g)$stage_class == 2] <- "goldenrod3" #adult
V(g)$colour[V(g)$stage_class == 3] <- "deepskyblue4" #reproductive career
V(g)$colour[V(g)$stage_class == 4] <- "darkorchid4" #post-reproductive

#check network properties
edge_density(g)
reciprocity(g)
transitivity(g)
centralization.degree(g, "out")
centralization.degree(g, "in")

# Lets plot to see what the network looks like
# We'll colour by stage class 
plot(g, edge.arrow.size=0.5, vertex.size=5,
      vertex.label = NA, vertex.color = V(g)$colour, edge.curved=0.4, layout = layout_nicely)

## Playing with the network ----

### Change the probabilities in the block matrix ----

#### Resource transfers towards juveniles ----

# Specify the number of nodes and the number of blocks
N <- 100
num_blocks <- 4
block_sizes <- c(25, 25, 25, 25)

# Given eventual changing block assignments (e.g., individuals age), should probably create a unique ID for individuals and match on that
#create a data frame for individuals and their IDs
indiv_downt <- data.frame(
  ID = 1:N, 
  block_assignments = rep(1:4, each=25)
)

# Define maximum out-degree for each node
# Here we create a vector of max outdegrees for each block
# But you can just supply the vector of food return counts
food_returns <- c(rep(3, 25), rep(20, 25), rep(20, 25), rep(20, 25))
max_deg <- food_returns

# set up a block matrix where juveniles are the ones that receive more but share the less (i.e. downward transfers) 
block_probs_downt <- matrix(c(0.1, 0.1, 0.1, 0.1,
                        0.001, 0.001, 0.001, 0.001,
                        0.001, 0.001, 0.001, 0.001,
                        0.001, 0.001, 0.001, 0.001),
                      nrow = num_blocks, ncol = num_blocks)
block_probs_downt

##### Generate the network ----

network <- simulate_SBM_max_degree(N, block_sizes, block_probs_downt, max_deg, indiv_downt$block_assignments)

#number of ties observed in the network 
sum(network)

# Look to see who's sending and receiving ties
indiv_downt$out_degree <- rowSums(network)
indiv_downt$in_degree <- colSums(network)
#check how it looks like
indiv_downt

#plot it!
#prepare data
data <- reshape(indiv_downt, 
                direction = "long",
                varying = list(names(indiv_downt)[3:4]),
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
  xlab("Life cycle stage") +
  ylab("Number of Ties") +
  ylim(0,y_limit_max)

#prepare network plot
g_downt <- graph_from_adjacency_matrix(network)
V(g_downt)$ID <- indiv_downt$ID
V(g_downt)$stage_class <- indiv_downt$block_assignments[match(V(g_downt)$ID, indiv_downt$ID)]
V(g_downt)$colour[V(g_downt)$stage_class == 1] <- "darkseagreen3" #juvenile
V(g_downt)$colour[V(g_downt)$stage_class == 2] <- "goldenrod3" #adult
V(g_downt)$colour[V(g_downt)$stage_class == 3] <- "deepskyblue4" #reproductive career
V(g_downt)$colour[V(g_downt)$stage_class == 4] <- "darkorchid4" #post-reproductive
        
#check network properties
edge_density(g_downt)
reciprocity(g_downt)
transitivity(g_downt)
centralization.degree(g_downt, "out")
centralization.degree(g_downt, "in")

# Lets plot to see what the network looks like
# We'll colour by stage class 
plot(g_downt, edge.arrow.size=0.5, vertex.size=5,
       vertex.label = NA, vertex.color = V(g_downt)$colour, edge.curved=0.4, layout = layout_nicely)

#### Resource transfers towards reproductive career ----

# Specify the number of nodes and the number of blocks
N <- 100
num_blocks <- 4
block_sizes <- c(25, 25, 25, 25)

# Given eventual changing block assignments (e.g., individuals age), should probably create a unique ID for individuals and match on that
#create a data frame for individuals and their IDs
indiv_repro <- data.frame(
  ID = 1:N, 
  block_assignments = rep(1:4, each=25)
)

# Define maximum out-degree for each node
# Here we create a vector of max outdegrees for each block
# But you can just supply the vector of food return counts
food_returns <- c(rep(3, 25), rep(20, 25), rep(20, 25), rep(15, 25))
max_deg <- food_returns

# set up a block matrix where juveniles are the ones that receive more but share the less (i.e. downward transfers) 
block_probs_repro <- matrix(c(0.001, 0.001, 0.001, 0.001,
                              0.001, 0.001, 0.001, 0.001,
                              0.1, 0.1, 0.1, 0.1,
                              0.001, 0.001, 0.001, 0.001),
                            nrow = num_blocks, ncol = num_blocks)
block_probs_repro

##### Generate the network ----

network <- simulate_SBM_max_degree(N, block_sizes, block_probs_repro, max_deg, indiv_repro$block_assignments)

#number of ties observed in the network 
sum(network)

# Look to see who's sending and receiving ties
indiv_repro$out_degree <- rowSums(network)
indiv_repro$in_degree <- colSums(network)
#check how it looks like
indiv_repro

#plot it!
#prepare data
data <- reshape(indiv_repro, 
                direction = "long",
                varying = list(names(indiv_repro)[3:4]),
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
  xlab("Life cycle stage") +
  ylab("Number of Ties") +
  ylim(0,y_limit_max)

#prepare network plot
g_repro <- graph_from_adjacency_matrix(network)
V(g_repro)$ID <- indiv_repro$ID
V(g_repro)$stage_class <- indiv_repro$block_assignments[match(V(g_repro)$ID, indiv_repro$ID)]
V(g_repro)$colour[V(g_repro)$stage_class == 1] <- "darkseagreen3" #juvenile
V(g_repro)$colour[V(g_repro)$stage_class == 2] <- "goldenrod3" #adult
V(g_repro)$colour[V(g_repro)$stage_class == 3] <- "deepskyblue4" #reproductive career
V(g_repro)$colour[V(g_repro)$stage_class == 4] <- "darkorchid4" #post-reproductive
        
#check network properties
edge_density(g_repro)
reciprocity(g_repro)
transitivity(g_repro)
centralization.degree(g_repro, "out")
centralization.degree(g_repro, "in")
      
# Lets plot to see what the network looks like
# We'll colour by stage class 
plot(g_repro, edge.arrow.size=0.5, vertex.size=5,
vertex.label = NA, vertex.color = V(g_repro)$colour, edge.curved=0.4, layout = layout_nicely)
      
#### Resource transfers towards juveniles and post-reproductive ----

# Specify the number of nodes and the number of blocks
N <- 100
num_blocks <- 4
block_sizes <- c(25, 25, 25, 25)

# Given eventual changing block assignments (e.g., individuals age), should probably create a unique ID for individuals and match on that
#create a data frame for individuals and their IDs
indiv_juvepost <- data.frame(
  ID = 1:N, 
  block_assignments = rep(1:4, each=25)
)

# Define maximum out-degree for each node
# Here we create a vector of max outdegrees for each block
# But you can just supply the vector of food return counts
food_returns <- c(rep(3, 25), rep(20, 25), rep(20, 25), rep(20, 25))
max_deg <- food_returns

# set up a block matrix where juveniles are the ones that receive more but share the less (i.e. downward transfers) 
block_probs_juvepost <- matrix(c(0.1, 0.1, 0.1, 0.1,
                              0.001, 0.001, 0.001, 0.001,
                              0.001, 0.001, 0.001, 0.001,
                              0.1, 0.1, 0.1, 0.1),
                            nrow = num_blocks, ncol = num_blocks)
block_probs_juvepost

##### Generate the network ----

network <- simulate_SBM_max_degree(N, block_sizes, block_probs_juvepost, max_deg, indiv_juvepost$block_assignments)

#number of ties observed in the network 
sum(network)

# Look to see who's sending and receiving ties
indiv_juvepost$out_degree <- rowSums(network)
indiv_juvepost$in_degree <- colSums(network)
#check how it looks like
indiv_juvepost

#plot it!
#prepare data
data <- reshape(indiv_juvepost, 
                direction = "long",
                varying = list(names(indiv_juvepost)[3:4]),
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
  xlab("Life cycle stage") +
  ylab("Number of Ties") +
  ylim(0,y_limit_max)

#prepare network plot
g_juvepost <- graph_from_adjacency_matrix(network)
V(g_juvepost)$ID <- indiv_juvepost$ID
V(g_juvepost)$stage_class <- indiv_juvepost$block_assignments[match(V(g_juvepost)$ID, indiv_juvepost$ID)]
V(g_juvepost)$colour[V(g_juvepost)$stage_class == 1] <- "darkseagreen3" #juvenile
V(g_juvepost)$colour[V(g_juvepost)$stage_class == 2] <- "goldenrod3" #adult
V(g_juvepost)$colour[V(g_juvepost)$stage_class == 3] <- "deepskyblue4" #reproductive career
V(g_juvepost)$colour[V(g_juvepost)$stage_class == 4] <- "darkorchid4" #post-reproductive
        
#check network properties
edge_density(g_juvepost)
reciprocity(g_juvepost)
transitivity(g_juvepost)
centralization.degree(g_juvepost, "out")
centralization.degree(g_juvepost, "in")
      
# Lets plot to see what the network looks like
# We'll colour by stage class 
plot(g_juvepost, edge.arrow.size=0.5, vertex.size=5,
vertex.label = NA, vertex.color = V(g_juvepost)$colour, edge.curved=0.4, layout = layout_nicely)

#### Resource transfers towards juveniles and reproductive career ----

# Specify the number of nodes and the number of blocks
N <- 100
num_blocks <- 4
block_sizes <- c(25, 25, 25, 25)

# Given eventual changing block assignments (e.g., individuals age), should probably create a unique ID for individuals and match on that
#create a data frame for individuals and their IDs
indiv_juverepro <- data.frame(
  ID = 1:N, 
  block_assignments = rep(1:4, each=25)
)

# Define maximum out-degree for each node
# Here we create a vector of max outdegrees for each block
# But you can just supply the vector of food return counts
food_returns <- c(rep(3, 25), rep(20, 25), rep(20, 25), rep(15, 25))
max_deg <- food_returns

# set up a block matrix where juveniles are the ones that receive more but share the less (i.e. downward transfers) 
block_probs_juverepro <- matrix(c(0.1, 0.1, 0.1, 0.1,
                                 0.001, 0.001, 0.001, 0.001,
                                 0.1, 0.1, 0.1, 0.1,
                                 0.001, 0.001, 0.001, 0.001),
                               nrow = num_blocks, ncol = num_blocks)
block_probs_juverepro

##### Generate the network ----

network <- simulate_SBM_max_degree(N, block_sizes, block_probs_juverepro, max_deg, indiv_juverepro$block_assignments)

#number of ties observed in the network 
sum(network)

# Look to see who's sending and receiving ties
indiv_juverepro$out_degree <- rowSums(network)
indiv_juverepro$in_degree <- colSums(network)
#check how it looks like
indiv_juverepro

#plot it!
#prepare data
data <- reshape(indiv_juverepro, 
                direction = "long",
                varying = list(names(indiv_juverepro)[3:4]),
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
  xlab("Life cycle stage") +
  ylab("Number of Ties") +
  ylim(0,y_limit_max)

#prepare network plot
g_juverepro <- graph_from_adjacency_matrix(network)
V(g_juverepro)$ID <- indiv_juverepro$ID
V(g_juverepro)$stage_class <- indiv_juverepro$block_assignments[match(V(g_juverepro)$ID, indiv_juverepro$ID)]
V(g_juverepro)$colour[V(g_juverepro)$stage_class == 1] <- "darkseagreen3" #juvenile
V(g_juverepro)$colour[V(g_juverepro)$stage_class == 2] <- "goldenrod3" #adult
V(g_juverepro)$colour[V(g_juverepro)$stage_class == 3] <- "deepskyblue4" #reproductive career
V(g_juverepro)$colour[V(g_juverepro)$stage_class == 4] <- "darkorchid4" #post-reproductive
        
#check network properties
edge_density(g_juverepro)
reciprocity(g_juverepro)
transitivity(g_juverepro)
centralization.degree(g_juverepro, "out")
centralization.degree(g_juverepro, "in")
      
# Lets plot to see what the network looks like
# We'll colour by stage class 
plot(g_juverepro, edge.arrow.size=0.5, vertex.size=5,
vertex.label = NA, vertex.color = V(g_juverepro)$colour, edge.curved=0.4, layout = layout_nicely)

#### Resource transfers towards juveniles, reproductive career, and post-reproductive ----

# Specify the number of nodes and the number of blocks
N <- 100
num_blocks <- 4
block_sizes <- c(25, 25, 25, 25)

# Given eventual changing block assignments (e.g., individuals age), should probably create a unique ID for individuals and match on that
#create a data frame for individuals and their IDs
indiv_juverepropost <- data.frame(
  ID = 1:N, 
  block_assignments = rep(1:4, each=25)
)

# Define maximum out-degree for each node
# Here we create a vector of max outdegrees for each block
# But you can just supply the vector of food return counts
food_returns <- c(rep(3, 25), rep(20, 25), rep(20, 25), rep(15, 25))
max_deg <- food_returns

# set up a block matrix where juveniles are the ones that receive more but share the less (i.e. downward transfers) 
block_probs_juverepropost <- matrix(c(0.1, 0.1, 0.1, 0.1,
                                  0.001, 0.001, 0.001, 0.001,
                                  0.1, 0.1, 0.1, 0.1,
                                  0.1, 0.1, 0.1, 0.1),
                                nrow = num_blocks, ncol = num_blocks)
block_probs_juverepropost

##### Generate the network ----

network <- simulate_SBM_max_degree(N, block_sizes, block_probs_juverepropost, max_deg, indiv_juverepropost$block_assignments)

#number of ties observed in the network 
sum(network)

# Look to see who's sending and receiving ties
indiv_juverepropost$out_degree <- rowSums(network)
indiv_juverepropost$in_degree <- colSums(network)
#check how it looks like
indiv_juverepropost

#plot it!
#prepare data
data <- reshape(indiv_juverepropost, 
                direction = "long",
                varying = list(names(indiv_juverepropost)[3:4]),
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
  xlab("Life cycle stage") +
  ylab("Number of Ties") +
  ylim(0,y_limit_max)

#prepare network plot
g_juverepropost <- graph_from_adjacency_matrix(network)
V(g_juverepropost)$ID <- indiv_juverepropost$ID
V(g_juverepropost)$stage_class <- indiv_juverepropost$block_assignments[match(V(g_juverepropost)$ID, indiv_juverepropost$ID)]
V(g_juverepropost)$colour[V(g_juverepropost)$stage_class == 1] <- "darkseagreen3" #juvenile
V(g_juverepropost)$colour[V(g_juverepropost)$stage_class == 2] <- "goldenrod3" #adult
V(g_juverepropost)$colour[V(g_juverepropost)$stage_class == 3] <- "deepskyblue4" #reproductive career
V(g_juverepropost)$colour[V(g_juverepropost)$stage_class == 4] <- "darkorchid4" #post-reproductive
        
#check network properties
edge_density(g_juverepropost)
reciprocity(g_juverepropost)
transitivity(g_juverepropost)
centralization.degree(g_juverepropost, "out")
centralization.degree(g_juverepropost, "in")
      
# Lets plot to see what the network looks like
# We'll colour by stage class 
plot(g_juverepropost, edge.arrow.size=0.5, vertex.size=5,
vertex.label = NA, vertex.color = V(g_juverepropost)$colour, edge.curved=0.4, layout = layout_nicely)

#### Resource transfers equally towards everyone ----

# Specify the number of nodes and the number of blocks
N <- 100
num_blocks <- 4
block_sizes <- c(25, 25, 25, 25)

# Given eventual changing block assignments (e.g., individuals age), should probably create a unique ID for individuals and match on that
#create a data frame for individuals and their IDs
indiv_equal <- data.frame(
  ID = 1:N, 
  block_assignments = rep(1:4, each=25)
)

# Define maximum out-degree for each node
# Here we create a vector of max outdegrees for each block
# But you can just supply the vector of food return counts
food_returns <- c(rep(3, 25), rep(20, 25), rep(20, 25), rep(15, 25))
max_deg <- food_returns

# set up a block matrix where juveniles are the ones that receive more but share the less (i.e. downward transfers) 
block_probs_equal <- matrix(c(0.1, 0.1, 0.1, 0.1,
                                      0.1, 0.1, 0.1, 0.1,
                                      0.1, 0.1, 0.1, 0.1,
                                      0.1, 0.1, 0.1, 0.1),
                                    nrow = num_blocks, ncol = num_blocks)
block_probs_equal

##### Generate the network ----

network <- simulate_SBM_max_degree(N, block_sizes, block_probs_equal, max_deg, indiv_equal$block_assignments)

#number of ties observed in the network 
sum(network)

# Look to see who's sending and receiving ties
indiv_equal$out_degree <- rowSums(network)
indiv_equal$in_degree <- colSums(network)
#check how it looks like
indiv_equal

#plot it!
#prepare data
data <- reshape(indiv_equal, 
                direction = "long",
                varying = list(names(indiv_equal)[3:4]),
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
  xlab("Life cycle stage") +
  ylab("Number of Ties") +
  ylim(0,y_limit_max)

#prepare network plot
g_equal <- graph_from_adjacency_matrix(network)
V(g_equal)$ID <- indiv_equal$ID
V(g_equal)$stage_class <- indiv_equal$block_assignments[match(V(g_equal)$ID, indiv_equal$ID)]
V(g_equal)$colour[V(g_equal)$stage_class == 1] <- "darkseagreen3" #juvenile
V(g_equal)$colour[V(g_equal)$stage_class == 2] <- "goldenrod3" #adult
V(g_equal)$colour[V(g_equal)$stage_class == 3] <- "deepskyblue4" #reproductive career
V(g_equal)$colour[V(g_equal)$stage_class == 4] <- "darkorchid4" #post-reproductive
        
#check network properties
edge_density(g_equal)
reciprocity(g_equal)
transitivity(g_equal)
centralization.degree(g_equal, "out")
centralization.degree(g_equal, "in")
      
# Lets plot to see what the network looks like
# We'll colour by stage class 
plot(g_equal, edge.arrow.size=0.5, vertex.size=5,
vertex.label = NA, vertex.color = V(g_equal)$colour, edge.curved=0.4, layout = layout_nicely)

#### Resource transfers equally scarce towards everyone ----

# Specify the number of nodes and the number of blocks
N <- 100
num_blocks <- 4
block_sizes <- c(25, 25, 25, 25)

# Given eventual changing block assignments (e.g., individuals age), should probably create a unique ID for individuals and match on that
#create a data frame for individuals and their IDs
indiv_scarce <- data.frame(
  ID = 1:N, 
  block_assignments = rep(1:4, each=25)
)

# Define maximum out-degree for each node
# Here we create a vector of max outdegrees for each block
# But you can just supply the vector of food return counts
food_returns <- c(rep(3, 25), rep(20, 25), rep(20, 25), rep(15, 25))
max_deg <- food_returns

# set up a block matrix where juveniles are the ones that receive more but share the less (i.e. downward transfers) 
block_probs_scarce <- matrix(c(0.001, 0.001, 0.001, 0.001,
                              0.001, 0.001, 0.001, 0.001,
                              0.001, 0.001, 0.001, 0.001,
                              0.001, 0.001, 0.001, 0.001),
                            nrow = num_blocks, ncol = num_blocks)
block_probs_scarce

##### Generate the network ----

network <- simulate_SBM_max_degree(N, block_sizes, block_probs_scarce, max_deg, indiv_scarce$block_assignments)

#number of ties observed in the network 
sum(network)

# Look to see who's sending and receiving ties
indiv_scarce$out_degree <- rowSums(network)
indiv_scarce$in_degree <- colSums(network)
#check how it looks like
indiv_scarce

#plot it!
#prepare data
data <- reshape(indiv_scarce, 
                direction = "long",
                varying = list(names(indiv_scarce)[3:4]),
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
  xlab("Life cycle stage") +
  ylab("Number of Ties") +
  ylim(0,y_limit_max)

#prepare network plot
g_scarce <- graph_from_adjacency_matrix(network)
V(g_scarce)$ID <- indiv_scarce$ID
V(g_scarce)$stage_class <- indiv_scarce$block_assignments[match(V(g_scarce)$ID, indiv_scarce$ID)]
V(g_scarce)$colour[V(g_scarce)$stage_class == 1] <- "darkseagreen3" #juvenile
V(g_scarce)$colour[V(g_scarce)$stage_class == 2] <- "goldenrod3" #adult
V(g_scarce)$colour[V(g_scarce)$stage_class == 3] <- "deepskyblue4" #reproductive career
V(g_scarce)$colour[V(g_scarce)$stage_class == 4] <- "darkorchid4" #post-reproductive
        
#check network properties
edge_density(g_scarce)
reciprocity(g_scarce)
transitivity(g_scarce)
centralization.degree(g_scarce, "out")
centralization.degree(g_scarce, "in")
      
# Lets plot to see what the network looks like
# We'll colour by stage class 
plot(g_scarce, edge.arrow.size=0.5, vertex.size=5,
vertex.label = NA, vertex.color = V(g_scarce)$colour, edge.curved=0.4, layout = layout_nicely)

#### Resource transfers more realistic ----

# Specify the number of nodes and the number of blocks
N <- 100
num_blocks <- 4
block_sizes <- c(25, 25, 25, 25)

# Given eventual changing block assignments (e.g., individuals age), should probably create a unique ID for individuals and match on that
#create a data frame for individuals and their IDs
indiv_real <- data.frame(
  ID = 1:N, 
  block_assignments = rep(1:4, each=25)
)

# Define maximum out-degree for each node
# Here we create a vector of max outdegrees for each block
# But you can just supply the vector of food return counts
food_returns <- c(rep(3, 25), rep(20, 25), rep(20, 25), rep(15, 25))
max_deg <- food_returns

# set up a block matrix where juveniles are the ones that receive more but share the less (i.e. downward transfers) 
block_probs_real <- matrix(c(0.001, 0.008, 0.008, 0.008,
                             0.001, 0.001, 0.001, 0.001,
                             0.001, 0.002, 0.002, 0.003,
                             0.001, 0.003, 0.003, 0.002),
                             nrow = num_blocks, ncol = num_blocks)
block_probs_real

##### Generate the network ----

network <- simulate_SBM_max_degree(N, block_sizes, block_probs_real, max_deg, indiv_real$block_assignments)

#number of ties observed in the network 
sum(network)

# Look to see who's sending and receiving ties
indiv_real$out_degree <- rowSums(network)
indiv_real$in_degree <- colSums(network)
#check how it looks like
indiv_real

#plot it!
#prepare data
data <- reshape(indiv_real, 
                direction = "long",
                varying = list(names(indiv_real)[3:4]),
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
  xlab("Life cycle stage") +
  ylab("Number of Ties") +
  ylim(0,y_limit_max)

#prepare network plot
g_real <- graph_from_adjacency_matrix(network)
V(g_real)$ID <- indiv_real$ID
V(g_real)$stage_class <- indiv_real$block_assignments[match(V(g_real)$ID, indiv_real$ID)]
V(g_real)$colour[V(g_real)$stage_class == 1] <- "darkseagreen3" #juvenile
V(g_real)$colour[V(g_real)$stage_class == 2] <- "goldenrod3" #adult
V(g_real)$colour[V(g_real)$stage_class == 3] <- "deepskyblue4" #reproductive career
V(g_real)$colour[V(g_real)$stage_class == 4] <- "darkorchid4" #post-reproductive
        
#check network properties
edge_density(g_real)
reciprocity(g_real)
transitivity(g_real)
centralization.degree(g_real, "out")
centralization.degree(g_real, "in")
      
# Lets plot to see what the network looks like
# We'll colour by stage class 
plot(g_real, edge.arrow.size=0.5, vertex.size=5,
vertex.label = NA, vertex.color = V(g_real)$colour, edge.curved=0.4, layout = layout_nicely)

#### Resource transfers more realistic: low probability ----

# Specify the number of nodes and the number of blocks
N <- 100
num_blocks <- 4
block_sizes <- c(25, 25, 25, 25)

# Given eventual changing block assignments (e.g., individuals age), should probably create a unique ID for individuals and match on that
#create a data frame for individuals and their IDs
indiv_reallow <- data.frame(
  ID = 1:N, 
  block_assignments = rep(1:4, each=25)
)

# Define maximum out-degree for each node
# Here we create a vector of max outdegrees for each block
# But you can just supply the vector of food return counts
food_returns <- c(rep(3, 25), rep(20, 25), rep(20, 25), rep(15, 25))
max_deg <- food_returns

# set up a block matrix where juveniles are the ones that receive more but share the less (i.e. downward transfers) 
block_probs_reallow <- matrix(c(0.0001, 0.0008, 0.0008, 0.0008,
                             0.0001, 0.0001, 0.0001, 0.0001,
                             0.0001, 0.0002, 0.0002, 0.0003,
                             0.0001, 0.0003, 0.0003, 0.0002),
                           nrow = num_blocks, ncol = num_blocks)
block_probs_reallow

##### Generate the network ----

network <- simulate_SBM_max_degree(N, block_sizes, block_probs_reallow, max_deg, indiv_reallow$block_assignments)

#number of ties observed in the network 
sum(network)

# Look to see who's sending and receiving ties
indiv_reallow$out_degree <- rowSums(network)
indiv_reallow$in_degree <- colSums(network)
#check how it looks like
indiv_reallow

#plot it!
#prepare data
data <- reshape(indiv_reallow, 
                direction = "long",
                varying = list(names(indiv_reallow)[3:4]),
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
  xlab("Life cycle stage") +
  ylab("Number of Ties") +
  ylim(0,y_limit_max)

#prepare network plot
g_reallow <- graph_from_adjacency_matrix(network)
V(g_reallow)$ID <- indiv_reallow$ID
V(g_reallow)$stage_class <- indiv_reallow$block_assignments[match(V(g_reallow)$ID, indiv_reallow$ID)]
V(g_reallow)$colour[V(g_reallow)$stage_class == 1] <- "darkseagreen3" #juvenile
V(g_reallow)$colour[V(g_reallow)$stage_class == 2] <- "goldenrod3" #adult
V(g_reallow)$colour[V(g_reallow)$stage_class == 3] <- "deepskyblue4" #reproductive career
V(g_reallow)$colour[V(g_reallow)$stage_class == 4] <- "darkorchid4" #post-reproductive
        
#check network properties
edge_density(g_reallow)
reciprocity(g_reallow)
transitivity(g_reallow)
centralization.degree(g_reallow, "out")
centralization.degree(g_reallow, "in")
      
# Lets plot to see what the network looks like
# We'll colour by stage class 
plot(g_reallow, edge.arrow.size=0.5, vertex.size=5,
vertex.label = NA, vertex.color = V(g_reallow)$colour, edge.curved=0.4, layout = layout_nicely)
      
#### Resource transfers more realistic: lower max out-degree  ----

# Specify the number of nodes and the number of blocks
N <- 100
num_blocks <- 4
block_sizes <- c(25, 25, 25, 25)

# Given eventual changing block assignments (e.g., individuals age), should probably create a unique ID for individuals and match on that
#create a data frame for individuals and their IDs
indiv_reallow <- data.frame(
  ID = 1:N, 
  block_assignments = rep(1:4, each=25)
)

# Define maximum out-degree for each node
# Here we create a vector of max outdegrees for each block
# But you can just supply the vector of food return counts
food_returns <- c(rep(3, 25), rep(15, 25), rep(15, 25), rep(10, 25))
max_deg <- food_returns

# set up a block matrix where juveniles are the ones that receive more but share the less (i.e. downward transfers) 
block_probs_reallow <- matrix(c(0.001, 0.008, 0.008, 0.008,
                                0.001, 0.001, 0.001, 0.001,
                                0.001, 0.002, 0.002, 0.003,
                                0.001, 0.003, 0.003, 0.002),
                              nrow = num_blocks, ncol = num_blocks)
block_probs_reallow

##### Generate the network ----

network <- simulate_SBM_max_degree(N, block_sizes, block_probs_reallow, max_deg, indiv_reallow$block_assignments)

#number of ties observed in the network 
sum(network)

# Look to see who's sending and receiving ties
indiv_reallow$out_degree <- rowSums(network)
indiv_reallow$in_degree <- colSums(network)
#check how it looks like
indiv_reallow

#plot it!
#prepare data
data <- reshape(indiv_reallow, 
                direction = "long",
                varying = list(names(indiv_reallow)[3:4]),
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
  xlab("Life cycle stage") +
  ylab("Number of Ties") +
  ylim(0,y_limit_max)

#prepare network plot
g_reallow <- graph_from_adjacency_matrix(network)
V(g_reallow)$ID <- indiv_reallow$ID
V(g_reallow)$stage_class <- indiv_reallow$block_assignments[match(V(g_reallow)$ID, indiv_reallow$ID)]
V(g_reallow)$colour[V(g_reallow)$stage_class == 1] <- "darkseagreen3" #juvenile
V(g_reallow)$colour[V(g_reallow)$stage_class == 2] <- "goldenrod3" #adult
V(g_reallow)$colour[V(g_reallow)$stage_class == 3] <- "deepskyblue4" #reproductive career
V(g_reallow)$colour[V(g_reallow)$stage_class == 4] <- "darkorchid4" #post-reproductive
        
#check network properties
edge_density(g_reallow)
reciprocity(g_reallow)
transitivity(g_reallow)
centralization.degree(g_reallow, "out")
centralization.degree(g_reallow, "in")
      
# Lets plot to see what the network looks like
# We'll colour by stage class 
plot(g_reallow, edge.arrow.size=0.5, vertex.size=5,
vertex.label = NA, vertex.color = V(g_reallow)$colour, edge.curved=0.4, layout = layout_nicely)
      
#### Resource transfers more realistic: lower max out-degree  ----

# Specify the number of nodes and the number of blocks
N <- 100
num_blocks <- 4
block_sizes <- c(25, 25, 25, 25)

# Given eventual changing block assignments (e.g., individuals age), should probably create a unique ID for individuals and match on that
#create a data frame for individuals and their IDs
indiv_reallow <- data.frame(
  ID = 1:N, 
  block_assignments = rep(1:4, each=25)
)

# Define maximum out-degree for each node
# Here we create a vector of max outdegrees for each block
# But you can just supply the vector of food return counts
food_returns <- c(rep(3, 25), rep(15, 25), rep(15, 25), rep(10, 25))
max_deg <- food_returns

# set up a block matrix where juveniles are the ones that receive more but share the less (i.e. downward transfers) 
block_probs_reallow <- matrix(c(0.0001, 0.0008, 0.0008, 0.0008,
                                0.0001, 0.0001, 0.0001, 0.0001,
                                0.0001, 0.0002, 0.0002, 0.0003,
                                0.0001, 0.0003, 0.0003, 0.0002),
                              nrow = num_blocks, ncol = num_blocks)
block_probs_reallow

##### Generate the network ----

network <- simulate_SBM_max_degree(N, block_sizes, block_probs_reallow, max_deg, indiv_reallow$block_assignments)

#number of ties observed in the network 
sum(network)

# Look to see who's sending and receiving ties
indiv_reallow$out_degree <- rowSums(network)
indiv_reallow$in_degree <- colSums(network)
#check how it looks like
indiv_reallow

#plot it!
#prepare data
data <- reshape(indiv_reallow, 
                direction = "long",
                varying = list(names(indiv_reallow)[3:4]),
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
  xlab("Life cycle stage") +
  ylab("Number of Ties") +
  ylim(0,y_limit_max)

#prepare network plot
g_reallow <- graph_from_adjacency_matrix(network)
V(g_reallow)$ID <- indiv_reallow$ID
V(g_reallow)$stage_class <- indiv_reallow$block_assignments[match(V(g_reallow)$ID, indiv_reallow$ID)]
V(g_reallow)$colour[V(g_reallow)$stage_class == 1] <- "darkseagreen3" #juvenile
V(g_reallow)$colour[V(g_reallow)$stage_class == 2] <- "goldenrod3" #adult
V(g_reallow)$colour[V(g_reallow)$stage_class == 3] <- "deepskyblue4" #reproductive career
V(g_reallow)$colour[V(g_reallow)$stage_class == 4] <- "darkorchid4" #post-reproductive
        
#check network properties
edge_density(g_reallow)
reciprocity(g_reallow)
transitivity(g_reallow)
centralization.degree(g_reallow, "out")
centralization.degree(g_reallow, "in")
      
# Lets plot to see what the network looks like
# We'll colour by stage class 
plot(g_reallow, edge.arrow.size=0.5, vertex.size=5,
vertex.label = NA, vertex.color = V(g_reallow)$colour, edge.curved=0.4, layout = layout_nicely)
      
      