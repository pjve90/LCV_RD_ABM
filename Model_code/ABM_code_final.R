# Life cycle variation and resource dynamics ABM: Code ----

## R settings ----

#set working directory
getwd()
setwd("./LCV_RD_ABM/Model_code")

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
source("production_fx.R")

#Production amount
source("production_amount.R")

## Maternal investment ----

#Identify if the mother has surplus of resources
source("mat_invest_mom_surp_identify.R")

#Identify the amount of surplus of the mother
source("mat_invest_mom_surp_amount.R")

#Identify if the descendants need resources
source("mat_invest_desc_need_identify.R")

#Identify the amount of need for each descendant
source("mat_invest_desc_need_amount.R")

#Order the descendants by need and mother id
source("mat_invest_desc_order.R")

#Mother invest in her descendants
source("mat_invest_fx.R")

## Resource transfers ----

# Specify the number of nodes and the number of blocks
num_blocks <- length(levels(as.factor(it_indpop$stage)))

#Define the block matrix
block_probs <- matrix(c(1e-10, 8e-10, 8e-10, 8e-10,
                        1e-10, 1e-10, 1e-10, 1e-10,
                        1e-10, 2e-10, 2e-10, 3e-10,
                        1e-10, 3e-10, 3e-10, 2e-10),
                      nrow = num_blocks, ncol = num_blocks)

#Define the surplus for transfers (max out degree in the network)
source("transfers_surplus_sbm.R")

#Generate the network
source("transfers_sbm_fx.R")

#Record the resources transferred
source("transfers_amount.R") 

## Reproduction ----

#Number of descendants per reproduction
n_desc <- 1

#Reproductive threshold
repro_thresh <- surv_cost*10

#Reproductive cost
repro_cost <- surv_cost

#Reproduction
source("reproduction_reproduce_fx.R")

#Discount of reproductive cost
source("reproduction_discount.R")

#Lifetime reproductive output
source("reproduction_lro.R")

#Add newborns
source("reproduction_newborn.R")

## Transition ----

#Time since last birth
source("transition_tlr.R")

#Transition
source("transition_fx.R")

## Survival ----

#Survival cost
surv_cost <- 1

#Survival
source("survival_survive_fx.R")

#Discount of survival cost
source("survival_discount.R")

#Age
source("survival_age.R")

## Run one iteration ----

### Initial population ----

#create population
it_indpop <- data.frame(id=1:100, #id
                        stage=c(rep(1,length.out=25),rep(2,length.out=25),rep(3,length.out=25),rep(4,length.out=25)), #life cycle stage
                        store_a=rep(surv_cost,length.out=100), #stored resources
                        prod_o=rep(NA,length.out=100), #production output
                        prod_a=rep(0,length.out=100), #production amount
                        mom_id=rep(NA,length.out=100), #mom id
                        mom_surplus=rep(NA,length.out=100), #identify mom surplus
                        mom_surplus_a=rep(NA,length.out=100), #mom surplus amount
                        desc_need=rep(0,length.out=100), #identify descendant need
                        desc_need_a=rep(NA,length.out=100), #descendant need amount
                        max_deg=rep(NA,length.out=100), #surplus for resource transfers
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
it_descpop <- data.frame(id=1:nrow(it_indpop))

### Run one iteration for all the population ----

#record the maximum id
max_id <- max(it_indpop$id)
  
#production
for (i in 1:nrow(it_indpop)){
  #production outcome
  it_indpop$prod_o <- produce(it_indpop)
  #production amount
  it_indpop <- produce_a(it_indpop)
}
#record production outcome and amount
resource_data <- it_indpop[,c("id","prod_o","prod_a")]

#maternal investment
for (i in 1:nrow(it_indpop)){
  #identify mom surplus
  it_indpop$mom_surplus <- mom_surplus(it_indpop)
  #mom surplus amount
  it_indpop$mom_surplus_a <- mom_surplus_a(it_indpop)
  #identify descendant need
  it_indpop$desc_need <- desc_need(it_indpop)
  #descendant need amount
  it_indpop$desc_need_a <- desc_need_a(it_indpop)
  #order descendants
  it_descpop <- desc_order(it_indpop)
  #maternal investment
  it_indpop <- mat_invest(it_indpop)
}
#record maternal investment
mat_invest_data <- it_indpop[,c("id","mom_surplus_a","desc_need_a")]
  
#resource transfers
for (i in 1:nrow(it_indpop)){
  #define surplus
  it_indpop$max_deg <- max_deg(it_indpop)
}
#generate network
network <- simulate_SBM_max_degree(nrow(it_indpop), #number of individuals
                                   as.vector(table(it_indpop$stage)), #number of individuals per life cycle stage
                                   block_probs, #block matrix
                                   it_indpop$max_deg, #maximum out degree per individual
                                   it_indpop$stage #life cycle stage of each individual
                                   )
for (i in 1:nrow(it_indpop)){
 #record resource transfers
  it_indpop <- transfers(it_indpop)
}
#record resource transfers and stored resources
transfers_data <- it_indpop[,c("id","out_degree","in_degree","store_a")]
 
#reproduction
for (i in 1:nrow(it_indpop)){
  #reproduction probability
  it_indpop$repro <- reproduce(it_indpop)
  #reproductive cost
  it_indpop$store_a <- reproduce_c(it_indpop)
  #lifetime reproductive output
  it_indpop$lro <- lro(it_indpop)
}
#record reproduction
repro_data <- it_indpop[,c("id","repro","lro")]
#add newborns
new_it_indpop <- newborns(new_it_indpop)
  
#transition
for (i in 1:nrow(it_indpop)){
  #time since last reproduction
  it_indpop$tlr <- tlr(it_indpop)
  # life cycle transition
  it_indpop$stage <- transition(it_indpop)
}
#record time since last reproduction and transition
transition_data <- it_indpop[,c("id","tlr","stage")]

#survival
for (i in 1:nrow(it_indpop)){
  #survival
  it_indpop$surv <- survive(it_indpop)
  #survival cost
  it_indpop$store_a <- survive_c(it_indpop)
  #age
  it_indpop$age <- age(it_indpop)
}
#record survival and age
surv_data <- it_indpop[,c("id","surv","age")]

#combine original population with newborns
it_indpop <- rbind(it_indpop,new_it_indpop)
#remove NA in id
it_indpop <- it_indpop[!is.na(it_indpop$id),]

#merge iteration records
it_data <- reduce(list(it_data,
                       resource_data,
                       mat_invest_data,
                       transfers_data,
                       repro_data,
                       transfers_data,
                       surv_data
),full_join,by="id",suffix=c("0","1"))

#check the population by the end of the iteration
head(it_indpop)
#check the recorded data by the end of the iteration
head(it_data)

## Run 100 iterations ----

### Initial population ----

#create population
it_indpop <- data.frame(id=1:100, #id
                        stage=c(rep(1,length.out=25),rep(2,length.out=25),rep(3,length.out=25),rep(4,length.out=25)), #life cycle stage
                        store_a=rep(surv_cost,length.out=100), #stored resources
                        prod_o=rep(NA,length.out=100), #production output
                        prod_a=rep(0,length.out=100), #production amount
                        mom_id=rep(NA,length.out=100), #mom id
                        mom_surplus=rep(NA,length.out=100), #identify mom surplus
                        mom_surplus_a=rep(NA,length.out=100), #mom surplus amount
                        desc_need=rep(0,length.out=100), #identify descendant need
                        desc_need_a=rep(NA,length.out=100), #descendant need amount
                        max_deg=rep(NA,length.out=100), #surplus for resource transfers
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
it_descpop <- data.frame(id=1:nrow(it_indpop))

### Run 100 iterations for all the population ----

for (b in 1:100){
  
  #record the maximum id
  max_id <- max(it_indpop$id)
  
  #production
  for (i in 1:nrow(it_indpop)){
    #production outcome
    it_indpop$prod_o <- produce(it_indpop)
    #production amount
    it_indpop <- produce_a(it_indpop)
  }
  #record production outcome and amount
  resource_data <- it_indpop[,c("id","prod_o","prod_a")]
  
  #maternal investment
  for (i in 1:nrow(it_indpop)){
    #identify mom surplus
    it_indpop$mom_surplus <- mom_surplus(it_indpop)
    #mom surplus amount
    it_indpop$mom_surplus_a <- mom_surplus_a(it_indpop)
    #identify descendant need
    it_indpop$desc_need <- desc_need(it_indpop)
    #descendant need amount
    it_indpop$desc_need_a <- desc_need_a(it_indpop)
    #order descendants
    it_descpop <- desc_order(it_indpop)
    #maternal investment
    it_indpop <- mat_invest(it_indpop)
  }
  #record maternal investment
  mat_invest_data <- it_indpop[,c("id","mom_surplus_a","desc_need_a")]
  
  #resource transfers
  for (i in 1:nrow(it_indpop)){
    #define surplus
    it_indpop$max_deg <- max_deg(it_indpop)
  }
  #generate network
  network <- simulate_SBM_max_degree(nrow(it_indpop),as.vector(table(it_indpop$stage)),block_probs,it_indpop$max_deg,it_indpop$stage)
  for (i in 1:nrow(it_indpop)){
    #record resource transfers
    it_indpop <- transfers(it_indpop)
  }
  #record resource transfers and stored resources
  transfers_data <- it_indpop[,c("id","out_degree","in_degree","store_a")]
  
  #reproduction
  for (i in 1:nrow(it_indpop)){
    #reproduction probability
    it_indpop$repro <- reproduce(it_indpop)
    #reproductive cost
    it_indpop$store_a <- reproduce_c(it_indpop)
    #lifetime reproductive output
    it_indpop$lro <- lro(it_indpop)
  }
  #record reproduction
  repro_data <- it_indpop[,c("id","repro","lro")]
  #add newborns
  new_it_indpop <- newborns(new_it_indpop)
  
  #transition
  for (i in 1:nrow(it_indpop)){
    #time since last reproduction
    it_indpop$tlr <- tlr(it_indpop)
    # life cycle transition
    it_indpop$stage <- transition(it_indpop)
  }
  #record time since last reproduction and transition
  transition_data <- it_indpop[,c("id","tlr","stage")]
  
  #survival
  for (i in 1:nrow(it_indpop)){
    #survival
    it_indpop$surv <- survive(it_indpop)
    #survival cost
    it_indpop$store_a <- survive_c(it_indpop)
    #age
    it_indpop$age <- age(it_indpop)
  }
  #record survival and age
  surv_data <- it_indpop[,c("id","surv","age")]
  
  #combine original population with newborns
  it_indpop <- rbind(it_indpop,new_it_indpop)
  #remove NA in id
  it_indpop <- it_indpop[!is.na(it_indpop$id),]
  
  #merge iteration records
  it_data <- reduce(list(it_data,
                         resource_data,
                         mat_invest_data,
                         transfers_data,
                         repro_data,
                         transfers_data,
                         surv_data
  ),full_join,by="id",suffix=c("b-1","b"))
}

#check the population by the end of the iteration
head(it_indpop)
#check the recorded data by the end of the iteration
head(it_data)

## Life history traits ----

#longevity
source("life_history_lng.R")

#lifetime reproductive output
source("life_history_lro.R")

#age at sexual maturity
source("life_history_asm.R")

#age at first reproduction
source("life_history_afr.R")

#age at last reproduction
source("life_history_alr.R")

#age at menopause
source("life_history_meno.R")

### Calculate life history traits ----

#create dataset
final_ind_data <- data.frame(id=1:nrow(it_data))

#calculate life history traits
for(i in 1:nrow(it_data)){
  #life-history traits
  #longevity
  final_ind_data$lng <- longevity(final_ind_data)
  #lro
  final_ind_data$lro <- lro(final_ind_data)
  #asm
  final_ind_data$asm <- asm(final_ind_data)
  #afr
  final_ind_data$afr <- afr(final_ind_data)
  #alr
  final_ind_data$alr <- alr(final_ind_data)
  #menopause
  final_ind_data$meno <- meno(final_ind_data)
}

#check the dataset
head(final_ind_data)

#longevity
source("life_history_lng.R")

#lifetime reproductive output
source("life_history_lro.R")

#age at sexual maturity
source("life_history_asm.R")

#age at first reproduction
source("life_history_afr.R")

#age at last reproduction
source("life_history_alr.R")

#age at menopause
source("life_history_meno.R")

## Resource dynamics ----

### Storage ----

#total amount of stored resources
source("resources_storage_total.R")

#average amount of stored resources
source("resources_storage_av.R")

#variability of stored resources
source("resources_storage_cv.R")

### Production ----

#total amount of production
source("resources_production_total.R")

#average amount of production
source("resources_production_av.R")

#variability of amount of production
source("resources_production_cv.R")

### Resource transfers ----

#### Given away ----

#total amount of resources given away
source("resources_outdeg_total.R")

#average amount of resources given away
source("resources_outdeg_av.R")

#variability of amount of resources given away
source("resources_outdeg_cv.R")

#### Received ----

#total amount of resources received
source("resources_indeg_total.R")

#average amount of resources received
source("resources_indeg_av.R")

#variability of amount of resources received
source("resources_indeg_cv.R")

### Calculate resource dynamics ----

#calculate resource dynamics
for(i in 1:nrow(it_data)){
  #resource dynamics
  #storage
  #total
  final_ind_data$store_total <- storage_total(final_ind_data)
  #average
  final_ind_data$store_av <- storage_av(final_ind_data)
  #variability
  final_ind_data$store_cv <- storage_cv(final_ind_data)
  #production
  #total
  final_ind_data$prod_total <- produce_total(final_ind_data)
  #average
  final_ind_data$prod_av <- produce_av(final_ind_data)
  #variability
  final_ind_data$prod_cv <- produce_cv(final_ind_data)
  #given away
  #total
  final_ind_data$outdeg_total <- outdegree_total(final_ind_data)
  #average
  final_ind_data$outdeg_av <- outdegree_av(final_ind_data)
  #variability
  final_ind_data$outdeg_cv <- outdegree_cv(final_ind_data)
  #received
  #total
  final_ind_data$indeg_total <- indegree_total(final_ind_data)
  #average
  final_ind_data$indeg_av <- indegree_av(final_ind_data)
  #variability
  final_ind_data$indeg_cv <- indegree_cv(final_ind_data)
}

#check dataset
head(final_ind_data)

## Plot it! ----


