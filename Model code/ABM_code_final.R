# Life cycle variation and resource dynamics ABM: Code ----

## R settings ----

#Install packages

#tidyverse
#install.packages("tidyverse")
library(tidyverse)

## Resource production ----

#Habitat quality
habitat <- c(1,2,2,1)
names(habitat) <- c("juvenile","adult","reproductive career", "post-reproductive")

#Stage-specific probabilities of production
prod_prob <- c(0.5,0.8,0.75,0.6)
names(prod_prob) <- c("juvenile","adult","reproductive career", "post-reproductive")

#Production function
source("https://raw.githubusercontent.com/pjve90/LCV_RD_ABM/main/Model%20code/production_fx.R?token=GHSAT0AAAAAAB5C6IJPTTBDHAP66WSMVNGQZAHJ5TA")

#Production amount
source("https://raw.githubusercontent.com/pjve90/LCV_RD_ABM/main/Model%20code/production_amount.R?token=GHSAT0AAAAAAB5C6IJPHSD6FZ5T4LUBK5XSZAHJ5NA")

## Maternal investment ----

#Identify if the mother has surplus of resources
source("https://raw.githubusercontent.com/pjve90/LCV_RD_ABM/main/Model%20code/mat_invest_mom_surp_identify.R?token=GHSAT0AAAAAAB5C6IJPMFWT5XPBUGSQMVM4ZAHJ7TA")

#Identify the amount of surplus of the mother
source("https://raw.githubusercontent.com/pjve90/LCV_RD_ABM/main/Model%20code/mat_invest_mom_surp_amount.R?token=GHSAT0AAAAAAB5C6IJPZIEAHCWDQ74N4HWKZAHJ7JQ")

#Identify if the descendants need resources
source("https://raw.githubusercontent.com/pjve90/LCV_RD_ABM/main/Model%20code/mat_invest_desc_need_identify.R?token=GHSAT0AAAAAAB5C6IJPTCAB7PBQ7YGUZ6JOZAHKACQ")

#Identify the amount of need for each descendant
source("https://raw.githubusercontent.com/pjve90/LCV_RD_ABM/main/Model%20code/mat_invest_desc_need_amount.R?token=GHSAT0AAAAAAB5C6IJPAD5AHTTHV544RUEKZAHKAUA")

#Order the descendants by need and mother id
source("https://raw.githubusercontent.com/pjve90/LCV_RD_ABM/main/Model%20code/mat_invest_desc_order.R?token=GHSAT0AAAAAAB5C6IJOKHUDEMRGIGXPRBTKZAHJ66Q")

#Mother invest in her descendants
source("https://raw.githubusercontent.com/pjve90/LCV_RD_ABM/main/Model%20code/mat_invest_fx.R?token=GHSAT0AAAAAAB5C6IJOKGTVL4OSATFFDYZUZAHJ7DA")

## Resource transfers ----

# Specify the number of nodes and the number of blocks
N <- nrow(it_indpop)
num_blocks <- length(levels(as.factor(it_indpop$stage)))
block_sizes <- as.vector(table(it_indpop$stage))

#Define the block matrix
block_probs <- matrix(c(1e-10, 8e-10, 8e-10, 8e-10,
                        1e-10, 1e-10, 1e-10, 1e-10,
                        1e-10, 2e-10, 2e-10, 3e-10,
                        1e-10, 3e-10, 3e-10, 2e-10),
                      nrow = num_blocks, ncol = num_blocks)

#Define the surplus for transfers (max out degree in the network)
source("https://raw.githubusercontent.com/pjve90/LCV_RD_ABM/main/Model%20code/transfers_surplus_sbm.R?token=GHSAT0AAAAAAB5C6IJO3JYAV2AWHRAAUBMMZAHKDFQ")

#Generate the network
source("https://raw.githubusercontent.com/pjve90/LCV_RD_ABM/main/Model%20code/transfers_sbm_fx.R?token=GHSAT0AAAAAAB5C6IJOPZBDSJDSP5OIFA4QZAHKDPQ")

#Record the resources transferred
source("https://raw.githubusercontent.com/pjve90/LCV_RD_ABM/main/Model%20code/transfers_amount.R?token=GHSAT0AAAAAAB5C6IJP6VFYIFHPEAQM45F4ZAHKD4A") 

## Reproduction ----

#Number of descendants per reproduction
n_desc <- 1

#Reproductive threshold
repro_thresh <- surv_cost*10

#Reproductive cost
repro_cost <- surv_cost

#Reproduction
source("https://raw.githubusercontent.com/pjve90/LCV_RD_ABM/main/Model%20code/reproduction_reproduce_fx.R?token=GHSAT0AAAAAAB5C6IJPSKXKWH3GD4C5WGA2ZAIS36A")

#Discount of reproductive cost
source("https://raw.githubusercontent.com/pjve90/LCV_RD_ABM/main/Model%20code/reproduction_discount.R?token=GHSAT0AAAAAAB5C6IJP2BI7WMYN4SPU7CCAZAIS4HQ")

#Lifetime reproductive output
source("https://raw.githubusercontent.com/pjve90/LCV_RD_ABM/main/Model%20code/reproduction_lro.R?token=GHSAT0AAAAAAB5C6IJPIQUQDOUCHHI2K6MAZAIS4XA")

#Add newborns...not finished
source("https://raw.githubusercontent.com/pjve90/LCV_RD_ABM/main/Model%20code/reproduction_newborn.R?token=GHSAT0AAAAAAB5C6IJOSYZGQ4Y6EKPR35UQZAIS47A")

## Transition ----

#Time since last birth
source("https://raw.githubusercontent.com/pjve90/LCV_RD_ABM/main/Model%20code/transition_tlr.R?token=GHSAT0AAAAAAB5C6IJPOVH3VJS5IKIESFH4ZAIS6DA")

#Transition
source("https://raw.githubusercontent.com/pjve90/LCV_RD_ABM/main/Model%20code/transition.R?token=GHSAT0AAAAAAB5C6IJOWUYXSYL3IAMFEZJ6ZAIS5WQ")

## Survival ----

#Survival cost
surv_cost <- 1

#Survival
source("https://raw.githubusercontent.com/pjve90/LCV_RD_ABM/main/Model%20code/transition_tlr.R?token=GHSAT0AAAAAAB5C6IJPG23RPBW3MON3VB5QZAIS5QA")

#Discount of survival cost
source("https://raw.githubusercontent.com/pjve90/LCV_RD_ABM/main/Model%20code/survival_discount.R?token=GHSAT0AAAAAAB5C6IJPZT5RBB5GOEI6MQYUZAIS6UA")

#Age
source("https://raw.githubusercontent.com/pjve90/LCV_RD_ABM/main/Model%20code/survival_age.R?token=GHSAT0AAAAAAB5C6IJOYF4EE2JLWCYKH2JQZAIS65Q")

## Initial population ----

#create population
it_indpop <- data.frame(id=1:100, #id
                        stage=c(rep(1,length.out=25),rep(2,length.out=25),rep(3,length.out=25),rep(4,length.out=25)), #life cycle stage
                        store_a=rep(surv_cost,length.out=100), #stored resources
                        prod_o=rep(NA,length.out=100), #production output
                        prod_a=rep(0,length.out=100), #production amount
                        mom_id=rep(NA,length.out=100), #mom id
                        mom_surplus=rep(NA,length.out=100), #identify mom surplus
                        mom_surplus_a=rep(NA,length.out=100), #mom surplus amount
                        desc_need=rep(NA,length.out=100), #identify descendant need
                        desc_need_a=rep(NA,length.out=100), #descendant need amount
                        max_deg=rep(NA,length.out=100), #surplus for resource transfers
                        out_degree=rep(NA,length.out=100), #amount of resources given away
                        in_degree=rep(NA,length.out=100), #amount of resources received
                        out_degree=rep(NA,length.out=100), #amount of resources given away
                        repro=rep(NA,length.out=100), #reproduction output
                        lro=rep(0,length.out=100), #lifetime reproductive output
                        tlr=rep(0,length.out=100), #time since last reproduction
                        surv=rep(NA,length.out=100), #survival output
                        age=c(rep(0,length.out=25),rep(10,length.out=25),rep(15,length.out=25),rep(45,length.out=25)) #age
                        )
#assign mom id in initial population
it_indpop$mom_id[it_indpop$stage==1] <- it_indpop$id[it_indpop$stage==3]
#check mom id
it_indpop$mom_id

#create data frame to record the dynamics in each iteration
it_data <- data.frame(id=1:nrow(it_indpop))

#run one iteration for all the population

