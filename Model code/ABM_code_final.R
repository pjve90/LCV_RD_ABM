# Life cycle variation and resource dynamics ABM: Code ----

#Load packages
#tidyverse
install.packages("tidyverse")
library(tidyverse)

## Auxiliary variables ----

### Resource dynamics ----

#Habitat quality
#stage-specific amount of resources that can be produced
prod_n <- c(1,2,2,1) #juvenile, adult, reproductive career, post-reproductive

#Production
#stage-specific probabilities of production
prod_prob <- c(0.5,0.8,0.75,0.6)  #juvenile, adult, reproductive career, post-reproductive

#Resource transfers
#block matrix with stage-specific probabilities of resources transfers
#each column is the stage-specific probabilities (juvenile, adult, reproductive career, post-reproductive)
block_matrix <- matrix(c(0.001, 0.008, 0.008, 0.008,
                         0.001, 0.001, 0.001, 0.001,
                         0.001, 0.002, 0.002, 0.003,
                         0.001, 0.003, 0.003, 0.002),
                         nrow = num_blocks, ncol = num_blocks)
block_matrix

### Life cycle ----

#Survival cost
surv_cost <- 1

#Reproductive cost
repro_cost <- 4

#Number of descendants per reproduction
repro_n <- 1

#Sexual maturity
sex_mat <- surv_cost + repro_cost * 10

## State variables ----

### Resource dynamics ----


### Life cycle ----

