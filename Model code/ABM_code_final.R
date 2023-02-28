# Life cycle variation and resource dynamics ABM: Code ----

#Load packages
#tidyverse
install.packages("tidyverse")
library(tidyverse)

### Resource dynamics ----

#Habitat quality
source("https://raw.githubusercontent.com/pjve90/LCV_RD_ABM/main/Model%20code/habitat_quality.R?token=GHSAT0AAAAAAB5C6IJOMCE44OBQPKQB65L4Y76D2DQ")

#Production
#stage-specific probabilities of production
source("https://raw.githubusercontent.com/pjve90/LCV_RD_ABM/main/Model%20code/production_prob.R?token=GHSAT0AAAAAAB5C6IJORPEBWDQGTXPUB4E6Y76FKPQ")
#production function
source()
#production amount
source()

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

