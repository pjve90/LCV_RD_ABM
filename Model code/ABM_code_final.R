# Life cycle variation and resource dynamics ABM: Code ----

## Resource dynamics ----

#Habitat quality
source("https://raw.githubusercontent.com/pjve90/LCV_RD_ABM/main/Model%20code/habitat_quality.R?token=GHSAT0AAAAAAB5C6IJP4LPU5FW5TFDKNVNUY77EEBQ")

#Production
#stage-specific probabilities of production
source("https://raw.githubusercontent.com/pjve90/LCV_RD_ABM/main/Model%20code/production_prob.R?token=GHSAT0AAAAAAB5C6IJOW6AVVENY37LYDMQAY77EEGQ")
#production function
source("https://raw.githubusercontent.com/pjve90/LCV_RD_ABM/main/Model%20code/production_fx.R?token=GHSAT0AAAAAAB5C6IJOSYCPF5WEIB7RJOZUY77EEMA")
#production amount
source("https://raw.githubusercontent.com/pjve90/LCV_RD_ABM/main/Model%20code/production_amount.R?token=GHSAT0AAAAAAB5C6IJPPWOGQQYX7E5W6JK4Y77EEQQ")

## Maternal investment ----

#Identify if the mother has surplus of resources
source()

#Identify the amount of surplus
source()

#Identify if the descendants need resources
source()

#Identify the amount of need for each descendant
source()

#Order the descendants by need and mother id
source()

#Mother invest in her descendants
source()

## Resource transfers ----

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

