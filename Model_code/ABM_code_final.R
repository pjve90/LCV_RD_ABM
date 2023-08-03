# Life cycle variation and resource dynamics ABM: Code ----

## R settings ----

#set working directory
getwd()
setwd("./LCV_RD_ABM/Model_code")

## Sourcing functions ----

#In this section, you source the different R functions needed to set up the state and auxiliary variables for the model.

### Initial population ----

#create population
source("initial_pop_fx.R")

### Resource production ----

#Stage-specific maximum amount of resource production 
source("production_maxprod_fx.R")

#Stage-specific production probabilities
source("production_prodprob_fx.R")

#Production function
source("production_fx.R")

### Maternal investment ----

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

### Resource transfers ----

#Softmax function
source("transfers_softmax_fx.R")

#Self nominations
source("transfers_self_nom.R")

#Define the stage-specific probabilities for each individual
source("transfers_blockprobs_fx.R")

#Define the surplus for transfers (max out degree in the network)
source("transfers_surplus_maxdeg.R")

#Generate the network
source("transfers_sbm_multinom_fx.R")

#Record the resources transferred
source("transfers_amount.R") 

### Reproduction ----

#Reproduction
source("reproduction_reproduce_fx.R")

#Discount of reproductive cost
source("reproduction_discount.R")

#Lifetime reproductive output
source("reproduction_lro.R")

#Add newborns
source("reproduction_newborn.R")

### Transition ----

#Time since last birth
source("transition_tlr.R")

#Transition
source("transition_fx.R")

#Transition
source("transition_fx.R")

### Survival ----

#Survival
source("survival_survive_fx.R")

#Discount of survival cost
source("survival_discount.R")

#Age
source("survival_age.R")

### Resource storing ----

#Store resources
source("storage_fx.R")

## Run one iteration ----

### Define auxiliary variables and parameters for initialisation ----

#### Initial population ----
#Here, you define the population size, create a data frame for the initial population, another one to record the output of each iteration, and one to record the need of descendants, for the maternal investment dynamics.

#population size = 100
popsize <- 100
#create population
it_indpop<-create_initialpop(popsize)

#create data frame to record the individual dynamics in each iteration
it_data <- data.frame(id=1:popsize)

#create data frame to record need of descendants, necessary for maternal investment
it_descpop <- data.frame(id=1:nrow(it_indpop))

#### Resource production ----
#Here, you define the habitat quality and the maximum production probability

#habitat quality
habitat_quality <- 4
#stage-specific maximum amount of resource production.
maxprod <- max_production(habitat_quality)

#maximum production probability
max_prod_prob <- 0.5
#stage-specific production probabilities.
prod_prob <- production_prob(max_prod_prob)

#### Resource transfers ----
#Here you define the number of life cycle stages, and the block matrix with its original probabilities, and log-odds transformation.

#number of life cycle stages
num_stages <- 4

#block matrix
#original probabilities
blockmatrix <- matrix(c(
  0.25,0.75,0.75,0.75, #juvenile column
  0.25,0.25,0.25,0.25, #adult column
  0.25,0.4,0.4,0.5, #reproductive career column
  0.25,0.5,0.5,0.4 #post-reproductive career column
),
nrow=num_stages,ncol=num_stages #matrix dimensions
)
#transform into log-odds
logodds_blockm <- log(blockmatrix/(1-blockmatrix))

#### Survival ----
#Here, you define the survival cost, which is the amount of resources necessary to cover individual maintenance and survive until the next iteration.

#survival cost
surv_cost <- 1

#### Reproduction ----
#Here, you define the number of descendants an individual can have per reproductive event, as well as the reproductive cost, which is the amount of resources that an individual needs to produce a new descendant.

#number of descendants per reproduction
n_desc <- 1

#reproductive cost
repro_cost <- surv_cost*2

#reproductive threshold
repro_thresh <- repro_cost*5

### Run one iteration for all the population ----

#Here, you run the different functions to check that everything is working well for one iteration

#Maximum id
#you record the maximum id so the id of the new individuals start after the existing one
max_id <- max(it_data$id)

#production
#you run the production function for every individual in the population, and record the amount of resources produced and available
for (i in 1:nrow(it_indpop)){
  #resource production
  it_indpop <- produce(it_indpop)
}
#record the amount of resources produced by each individual
resource_data <- it_indpop[,c("id","prod_a")]

#maternal investment
#you run the different functions necessary for the need-based maternal investment, and record the surplus of the mother and the need of each descendant
#mothers
for (i in 1:nrow(it_indpop)){
  #identify mom surplus
  it_indpop$mom_surplus <- mom_surplus(it_indpop)
  #mom surplus amount
  it_indpop$mom_surplus_a <- mom_surplus_a(it_indpop)
  #subset mothers
  it_mompop <- it_indpop[is.na(it_indpop$mom_surplus_a)==F,]
}  
#descendants  
for (i in 1:nrow(it_indpop)){
  #identify descendant need
  it_indpop$desc_need <- desc_need(it_indpop)
  #descendant need amount
  it_indpop$desc_need_a <- desc_need_a(it_indpop)
  #order descendants
  it_descpop <- desc_order(it_indpop)
}
#maternal investment
for (i in 1:nrow(it_mompop)){
  #subset descendants of mother
  it_descpop_momsub <- it_descpop[it_mompop$id[i] == it_descpop$mom_id,]
  #maternal investment
  if(nrow(it_descpop_momsub)>0){
  it_indpop <- mat_invest(it_indpop)}
}

#record maternal investment
mat_invest_data <- it_indpop[,c("id","mom_surplus_a","desc_need_a")]

#resource transfers
#you run the functions for resource transfers. First you define the resource surplus of each individual, which is the upper limit for the number of ties an individual can make, then you generate the social netwnork, and then record the resource transfers

#define surplus
for (i in 1:nrow(it_indpop)){
  it_indpop$max_deg <- max_deg(it_indpop)
}
#define self nominations
self_noms <- create_self_noms(nrow(it_indpop), #population size
                              1 #self nomination
                              )
#define individual block probabilities
block_probs <- create_block_probs(nrow(it_indpop), #population size
                                  it_indpop$stage #life cycle stages of individuals in the population
                                  )
#generate network
network <- simulate_SBM_multinomial(nrow(it_indpop), #number of individuals
                                   it_indpop$max_deg, #maximum out degree per individual
                                   block_probs, #individual block matrices
                                   self_noms #self nominations
                                   )
#record resource transfers
for (i in 1:nrow(it_indpop)){
  it_indpop <- transfers(it_indpop)
}
#record resource transfers and resources available
transfers_data <- it_indpop[,c("id","out_degree","in_degree","res_a")]

#Reproduction
#you run the functions for reproduction, record the outcomes, and generate a data frame with the newborns

for (i in 1:nrow(it_indpop)){
  #reproduction probability
  it_indpop$repro <- reproduce(it_indpop)
  #reproductive cost
  it_indpop$res_a <- reproduce_c(it_indpop)
  #lifetime reproductive output
  it_indpop$lro <- lro(it_indpop)
}
#record reproduction
repro_data <- it_indpop[,c("id","repro","lro")]
#add newborns
new_it_indpop <- newborns(new_it_indpop)
  
#Transition
#you update the time since last reproduction, evaluate if the individual transitions to the next life cycle stage, and record the outcomes

for (i in 1:nrow(it_indpop)){
  #time since last reproduction
  it_indpop$tlr <- tlr(it_indpop)
  # life cycle transition
  it_indpop <- transition(it_indpop)
}
#record time since last reproduction and transition
transition_data <- it_indpop[,c("id","tlr","stage")]
#record reproduction again to update in case of transition to reproductive career
repro_data <- it_indpop[,c("id","repro","lro")]

#Survival
#you evaluate if the individuals have enough resources to cover the survival costs, update the amount of resources available, and individuals age.

for (i in 1:nrow(it_indpop)){
  #survival
  it_indpop$surv <- survive(it_indpop)
  #survival cost
  it_indpop$res_a <- survive_c(it_indpop)
  #age
  it_indpop$age <- age(it_indpop)
}
#record survival and age
surv_data <- it_indpop[,c("id","surv","age")]

#Storage
#Here, you are storing the resources available by the end of the iteration to take them to the next iteration, and recording them

#store
for (i in 1:nrow(it_indpop)){
  #store resources
  it_indpop$store_a <- store(it_indpop)
}
#record stored resources
store_data <- it_indpop[,c("id","store_a")]

#Update datasets at the end of iteration
#Here, you update the population and record the individual dynamics at the end of the iteration. In the end you remove the individuals who died to have the population ready for the next iteration

#Update the population
#combine original population with newborns
it_indpop <- rbind(it_indpop,new_it_indpop)
#remove NA in id
it_indpop <- it_indpop[!is.na(it_indpop$id),]

#Update iteration record
#merge iteration records
it_data <- it_indpop[,c("id",
                        "prod_a",
                        "mom_id",
                        "mom_surplus_a",
                        "desc_need_a",
                        "out_degree",
                        "in_degree",
                        "res_a",
                        "repro",
                        "lro",
                        "tlr",
                        "stage",
                        "surv",
                        "age",
                        "store_a"
)]
#record iteration
it_data$year <- rep(1,length.out=nrow(it_data))

#Update the population for next iteration
#Update the maximum id
max_id <- max(it_data$id)
#remove individual who died
it_indpop <- it_indpop[!it_indpop$surv==0,]

#Check that the population and iteration record look fine

#check the population by the end of the iteration
head(it_indpop)
#check the recorded data by the end of the iteration
head(it_data)




## Run 100 iterations ----

### Define auxiliary variables and parameters for initialisation ----

#### Initial population ----
#Here, you define the population size, create a data frame for the initial population, another one to record the output of each iteration, and one to record the need of descendants, for the maternal investment dynamics.

#population size = 100
popsize <- 100
#create population
it_indpop<-create_initialpop(popsize)

#create data frame to record the individual dynamics in each iteration
it_data <- data.frame(id=1:popsize)

#create data frame to record need of descendants, necessary for maternal investment
it_descpop <- data.frame(id=1:nrow(it_indpop))

#### Resource production ----
#Here, you define the habitat quality and the maximum production probability

#habitat quality
habitat_quality <- 6
#stage-specific maximum amount of resource production.
maxprod <- max_production(habitat_quality)

#maximum production probability
max_prod_prob <- 0.5
#stage-specific production probabilities.
prod_prob <- production_prob(max_prod_prob)

#### Resource transfers ----
#Here you define the number of life cycle stages, and the block matrix with its original probabilities, and log-odds transformation.

#number of life cycle stages
num_stages <- 4

#block matrix
#original probabilities
blockmatrix <- matrix(c(
  0.25,0.75,0.75,0.75, #juvenile column
  0.25,0.25,0.25,0.25, #adult column
  0.25,0.4,0.4,0.5, #reproductive career column
  0.25,0.5,0.5,0.4 #post-reproductive career column
),
nrow=num_stages,ncol=num_stages #matrix dimensions
)
#transform into log-odds
logodds_blockm <- log(blockmatrix/(1-blockmatrix))

#### Survival ----
#Here, you define the survival cost, which is the amount of resources necessary to cover individual maintenance and survive until the next iteration.

#survival cost
surv_cost <- 1

#### Reproduction ----
#Here, you define the number of descendants an individual can have per reproductive event, as well as the reproductive cost, which is the amount of resources that an individual needs to produce a new descendant.

#number of descendants per reproduction
n_desc <- 1

#reproductive cost
repro_cost <- surv_cost*2

#reproductive threshold
repro_thresh <- repro_cost*5

### Run 100 iterations for all the population ----

#Here, you run the simulation for a 100 iterations, resembling 100 years.

#Define the number of years (iterations) you want to run the simulation
years<-300
#Maximum id
#you record the maximum id so the id of the new individuals start after the existing one
max_id <- max(it_data$id)

#Run the simulation
for (b in 1:years){

  #production
  #you run the production function for every individual in the population, and record the amount of resources produced and available
  for (i in 1:nrow(it_indpop)){
    #resource production
    it_indpop <- produce(it_indpop)
  }
  #record the amount of resources produced by each individual
  resource_data <- it_indpop[,c("id","prod_a")]
  
  #maternal investment
  #you run the different functions necessary for the need-based maternal investment, and record the surplus of the mother and the need of each descendant
  #mothers
  for (i in 1:nrow(it_indpop)){
    #identify mom surplus
    it_indpop$mom_surplus <- mom_surplus(it_indpop)
    #mom surplus amount
    it_indpop$mom_surplus_a <- mom_surplus_a(it_indpop)
    #subset mothers
    it_mompop <- it_indpop[is.na(it_indpop$mom_surplus_a)==F,]
  }  
  #descendants  
  for (i in 1:nrow(it_indpop)){
    #identify descendant need
    it_indpop$desc_need <- desc_need(it_indpop)
    #descendant need amount
    it_indpop$desc_need_a <- desc_need_a(it_indpop)
    #order descendants
    it_descpop <- desc_order(it_indpop)
  }
  #maternal investment
  for (i in 1:nrow(it_mompop)){
    #subset descendants of mother
    it_descpop_momsub <- it_descpop[it_mompop$id[i] == it_descpop$mom_id,]
    #maternal investment
    if(nrow(it_descpop_momsub)>0){
      it_indpop <- mat_invest(it_indpop)}
  }
  
  #record maternal investment
  mat_invest_data <- it_indpop[,c("id","mom_surplus_a","desc_need_a")]
  
  #resource transfers
  #you run the functions for resource transfers. First you define the resource surplus of each individual, which is the upper limit for the number of ties an individual can make, then you generate the social netwnork, and then record the resource transfers
  
  #define surplus
  for (i in 1:nrow(it_indpop)){
    it_indpop$max_deg <- max_deg(it_indpop)
  }
  #define self nominations
  self_noms <- create_self_noms(nrow(it_indpop), #population size
                                1 #self nomination
  )
  #define individual block probabilities
  block_probs <- create_block_probs(nrow(it_indpop), #population size
                                    it_indpop$stage #life cycle stages of individuals in the population
  )
  #generate network
  network <- simulate_SBM_multinomial(nrow(it_indpop), #number of individuals
                                      it_indpop$max_deg, #maximum out degree per individual
                                      block_probs, #individual block matrices
                                      self_noms #self nominations
  )
  #record resource transfers
  for (i in 1:nrow(it_indpop)){
    it_indpop <- transfers(it_indpop)
  }
  #record resource transfers and resources available
  transfers_data <- it_indpop[,c("id","out_degree","in_degree","res_a")]
  
  #Reproduction
  #you run the functions for reproduction, record the outcomes, and generate a data frame with the newborns
  
  for (i in 1:nrow(it_indpop)){
    #reproduction probability
    it_indpop$repro <- reproduce(it_indpop)
    #reproductive cost
    it_indpop$res_a <- reproduce_c(it_indpop)
    #lifetime reproductive output
    it_indpop$lro <- lro(it_indpop)
  }
  #record reproduction
  repro_data <- it_indpop[,c("id","repro","lro")]
  #add newborns
  new_it_indpop <- newborns(new_it_indpop)
  
  #Transition
  #you update the time since last reproduction, evaluate if the individual transitions to the next life cycle stage, and record the outcomes
  
  for (i in 1:nrow(it_indpop)){
    #time since last reproduction
    it_indpop$tlr <- tlr(it_indpop)
    # life cycle transition
    it_indpop <- transition(it_indpop)
  }
  #record time since last reproduction and transition
  transition_data <- it_indpop[,c("id","tlr","stage")]
  #record reproduction again to update in case of transition to reproductive career
  repro_data <- it_indpop[,c("id","repro","lro")]
  
  #Survival
  #you evaluate if the individuals have enough resources to cover the survival costs, update the amount of resources available, and individuals age.
  
  for (i in 1:nrow(it_indpop)){
    #survival
    it_indpop$surv <- survive(it_indpop)
    #survival cost
    it_indpop$res_a <- survive_c(it_indpop)
    #age
    it_indpop$age <- age(it_indpop)
  }
  #record survival and age
  surv_data <- it_indpop[,c("id","surv","age")]
  
  #Storage
  #Here, you are storing the resources available by the end of the iteration to take them to the next iteration, and recording them
  
  #store
  for (i in 1:nrow(it_indpop)){
    #store resources
    it_indpop$store_a <- store(it_indpop)
  }
  #record stored resources
  store_data <- it_indpop[,c("id","store_a")]
  
  #Update datasets at the end of iteration
  #Here, you update the population and record the individual dynamics at the end of the iteration. In the end you remove the individuals who died to have the population ready for the next iteration
  
  #Update the population
  #combine original population with newborns
  it_indpop <- rbind(it_indpop,new_it_indpop)
  #remove NA in id
  it_indpop <- it_indpop[!is.na(it_indpop$id),]
  
  #merge iteration records
  #Here you differentiate the ways you record the outcomes of the first iteration from the next ones, this way you can keep identify what is happening in each iteration
  if(b==1){ #first iteration
  it_data <- it_indpop[,c("id",
                          "prod_a",
                          "mom_id",
                          "mom_surplus_a",
                          "desc_need_a",
                          "out_degree",
                          "in_degree",
                          "res_a",
                          "repro",
                          "lro",
                          "tlr",
                          "stage",
                          "surv",
                          "age",
                          "store_a"
                          )]
  #record iteration
  it_data$year <- rep(b,length.out=nrow(it_data))
  #update it_dataf for rbind
  it_dataf <- it_data
  }else{ #other iterations
    it_data <- it_indpop[,c("id",
                            "prod_a",
                            "mom_id",
                            "mom_surplus_a",
                            "desc_need_a",
                            "out_degree",
                            "in_degree",
                            "res_a",
                            "repro",
                            "lro",
                            "tlr",
                            "stage",
                            "surv",
                            "age",
                            "store_a"
    )]
    #record iteration
    it_data$year <- rep(b,length.out=nrow(it_data))
    #merge with previous iteration records
    it_dataf <- rbind(it_dataf,it_data)
    it_dataf <- it_dataf[order(it_dataf$id),]
  }
  
  #Update the maximum id
  max_id <- max(it_dataf$id)
  #remove individuals that died
  it_indpop <- it_indpop[!it_indpop$surv==0,]
  print(b)
}

#check the population by the end of the iteration
head(it_indpop)
#check the recorded data by the end of the iteration
head(it_data)

#Check change in number of individuals in the population over time
library(dplyr)
as.data.frame(it_dataf %>% group_by(year) %>% summarize(n()))

#Check the change in average age over the years (shouldn't change if population stable)
as.data.frame(it_dataf %>% group_by(year) %>% summarize(mean(age)))

## Calculate outcomes from the model ----

#Now that the simulation is done and you have all the individual information from all the iterations, it is possible to calculate the different outcomes that are used to understand how the resource dynamics influente the variability of life cycles within a population.

#The section is divided in two sub sections: first, the life history traits, and second by the resource dynamics

### Life history traits ----

#Here you source the functions to calculate the different life history traits of interest: longevity, lifetime reproductive output, age at sexual maturity, first and last reproduction, and age at menopause. 

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

#In this section you run the different functions to calculate the different life history traits.

#create dataset
final_ind_data <- data.frame(id=1:nrow(it_data))

#calculate life history traits
for(i in 1:max(it_data$id)){
  #life-history traits
  #longevity
  final_ind_data$lng <- longevity(final_ind_data)
  #lro
  final_ind_data$lro <- lifetime_reproductive_output(final_ind_data)
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

### Resource dynamics ----

#Here you source the functions to calculate the different resource dynamics - available, storage, production, and transfers -, and for each of them you are calculating the total, the average, and the coefficient of variation

#### Resources available ----

#total amount of resources available
source("resources_available_total.R")

#average amount of resources available
source("resources_available_av.R")

#variability of resources available
source("resources_available_cv.R")

#### Storage ----

#total amount of stored resources
source("resources_storage_total.R")

#average amount of stored resources
source("resources_storage_av.R")

#variability of stored resources
source("resources_storage_cv.R")

#### Production ----

#total amount of production
source("resources_production_total.R")

#average amount of production
source("resources_production_av.R")

#variability of amount of production
source("resources_production_cv.R")

#### Resource transfers ----

##### Given away ----

#total amount of resources given away
source("resources_outdeg_total.R")

#average amount of resources given away
source("resources_outdeg_av.R")

#variability of amount of resources given away
source("resources_outdeg_cv.R")

##### Received ----

#total amount of resources received
source("resources_indeg_total.R")

#average amount of resources received
source("resources_indeg_av.R")

#variability of amount of resources received
source("resources_indeg_cv.R")

### Calculate resource dynamics ----

#In this section you run the different functions to calculate the resource dynamics

#calculate resource dynamics
for(i in 1:max(it_data$id)){
  #resource dynamics
  #available
  #total
  final_ind_data$available_total <- available_total(final_ind_data)
  #average
  final_ind_data$available_av <- available_av(final_ind_data)
  #variability
  final_ind_data$available_cv <- available_cv(final_ind_data)
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

#change NAs in standard deviation of resource dynamics into 0
#available
final_ind_data$available_cv[which(is.na(final_ind_data$available_cv)==TRUE)] <- 0
#storage
final_ind_data$store_cv[which(is.na(final_ind_data$store_cv)==TRUE)] <- 0
#production
final_ind_data$prod_cv[which(is.na(final_ind_data$prod_cv)==TRUE)] <- 0
#given away
final_ind_data$outdeg_cv[which(is.na(final_ind_data$outdeg_cv)==TRUE)] <- 0
#received
final_ind_data$indeg_cv[which(is.na(final_ind_data$indeg_cv)==TRUE)] <- 0

## Descriptive statistics ----

### Life history traits ----

#longevity
summary(final_ind_data$lng[1:100])
sd(final_ind_data$lng[1:100])
#mean=
#median=
#min=
#max=
#sd=

#lro
summary(final_ind_data$lro[1:100])
sd(final_ind_data$lro[1:100])
#mean=
#median=
#min=
#max=
#sd=

#asm
summary(final_ind_data$asm[1:100])
sd(final_ind_data$asm[1:100])
#mean=
#median=
#min=
#max=
#sd=

#afr
summary(final_ind_data$afr[1:100])
sd(final_ind_data$afr[1:100],na.rm=T)
#mean=
#median=
#min=
#max=
#sd=
#NA=

#alr
summary(final_ind_data$alr[1:100])
sd(final_ind_data$alr[1:100],na.rm=T)
#mean=
#median=
#min=
#max=
#sd=
#NA=

#meno
summary(final_ind_data$meno[1:100])
sd(final_ind_data$meno[1:100],na.rm=T)
#mean=
#median=
#min=
#max=
#sd=
#NA=

### Resource dynamics ----

#### Storage ----

#total
summary(final_ind_data$store_total[1:100])
sd(final_ind_data$store_total[1:100],na.rm=T)
#mean=
#median=
#min=
#max=
#sd=

#average
summary(final_ind_data$store_av[1:100])
sd(final_ind_data$store_av[1:100],na.rm=T)
#mean=
#median=
#min=
#max=
#sd=

#sd
summary(final_ind_data$store_sd[1:100])
sd(final_ind_data$store_sd[1:100],na.rm=T)
#mean=
#median=
#min=
#max=
#sd=

#### Production ----

#total
summary(final_ind_data$prod_total[1:100])
sd(final_ind_data$prod_total[1:100],na.rm=T)
#mean=
#median=
#min=
#max=
#sd=

#average
summary(final_ind_data$prod_av[1:100])
sd(final_ind_data$prod_av[1:100],na.rm=T)
#mean=
#median=
#min=
#max=
#sd=

#sd
summary(final_ind_data$prod_sd[1:100])
sd(final_ind_data$prod_sd[1:100],na.rm=T)
#mean=
#median=
#min=
#max=
#sd=

#### Given away ----

#total
summary(final_ind_data$outdeg_total[1:100])
sd(final_ind_data$outdeg_total[1:100],na.rm=T)
#mean=
#median=
#min=
#max=
#sd=

#average
summary(final_ind_data$outdeg_av[1:100])
sd(final_ind_data$outdeg_av[1:100],na.rm=T)
#mean=
#median=
#min=
#max=
#sd=

#sd
summary(final_ind_data$outdeg_sd[1:100])
sd(final_ind_data$outdeg_sd[1:100],na.rm=T)
#mean=
#median=
#min=
#max=
#sd=

#### Received ----

#total
summary(final_ind_data$indeg_total[1:100])
sd(final_ind_data$indeg_total[1:100],na.rm=T)
#mean=
#median=
#min=
#max=
#sd=

#average
summary(final_ind_data$indeg_av[1:100])
sd(final_ind_data$indeg_av[1:100],na.rm=T)
#mean=
#median=
#min=
#max=
#sd=

#sd
summary(final_ind_data$indeg_sd[1:100])
sd(final_ind_data$indeg_sd[1:100],na.rm=T)
#mean=
#median=
#min=
#max=
#sd=

## Plot it! ----

### Descriptive plots ----

#### Life history traits ----

#longevity
par(mfrow=c(1,2))
hist(final_ind_data$lng[1:100],breaks=20)
plot(final_ind_data$lng[1:100]~final_ind_data$id[1:100])

#lro
par(mfrow=c(1,2))
hist(final_ind_data$lro[1:100],breaks=20)
plot(final_ind_data$lro[1:100]~final_ind_data$id[1:100])

#asm
par(mfrow=c(1,2))
hist(final_ind_data$asm[1:100],breaks=20)
plot(final_ind_data$asm[1:100]~final_ind_data$id[1:100])

#afr
par(mfrow=c(1,2))
hist(final_ind_data$afr[1:100],breaks=20)
plot(final_ind_data$afr[1:100]~final_ind_data$id[1:100])

#alr
par(mfrow=c(1,2))
hist(final_ind_data$alr[1:100],breaks=20)
plot(final_ind_data$alr[1:100]~final_ind_data$id[1:100])

#meno
par(mfrow=c(1,2))
hist(final_ind_data$meno[1:100],breaks=20)
plot(final_ind_data$meno[1:100]~final_ind_data$id[1:100])

#### Resource dynamics ----

##### Storage ----

#total
par(mfrow=c(1,2))
hist(final_ind_data$store_total[1:100],breaks=20)
plot(final_ind_data$store_total[1:100]~final_ind_data$id[1:100])

#average
par(mfrow=c(1,2))
hist(final_ind_data$store_av[1:100],breaks=20)
plot(final_ind_data$store_av[1:100]~final_ind_data$id[1:100])

#sd
par(mfrow=c(1,2))
hist(final_ind_data$store_sd[1:100],breaks=20)
plot(final_ind_data$store_sd[1:100]~final_ind_data$id[1:100])

##### Production ----

#total
par(mfrow=c(1,2))
hist(final_ind_data$prod_total[1:100],breaks=20)
plot(final_ind_data$prod_total[1:100]~final_ind_data$id[1:100])

#average
par(mfrow=c(1,2))
hist(final_ind_data$prod_av[1:100],breaks=20)
plot(final_ind_data$prod_av[1:100]~final_ind_data$id[1:100])

#sd
par(mfrow=c(1,2))
hist(final_ind_data$prod_sd[1:100],breaks=20)
plot(final_ind_data$prod_sd[1:100]~final_ind_data$id[1:100])

##### Given away ----

#total
par(mfrow=c(1,2))
hist(final_ind_data$outdeg_total[1:100],breaks=20)
plot(final_ind_data$outdeg_total[1:100]~final_ind_data$id[1:100])

#average
par(mfrow=c(1,2))
hist(final_ind_data$outdeg_av[1:100],breaks=20)
plot(final_ind_data$outdeg_av[1:100]~final_ind_data$id[1:100])

#sd
par(mfrow=c(1,2))
hist(final_ind_data$outdeg_sd[1:100],breaks=20)
plot(final_ind_data$outdeg_sd[1:100]~final_ind_data$id[1:100])

##### Received ----

#total
par(mfrow=c(1,2))
hist(final_ind_data$indeg_total[1:100],breaks=20)
plot(final_ind_data$indeg_total[1:100]~final_ind_data$id[1:100])

#average
par(mfrow=c(1,2))
hist(final_ind_data$indeg_av[1:100],breaks=20)
plot(final_ind_data$indeg_av[1:100]~final_ind_data$id[1:100])

#sd
par(mfrow=c(1,2))
hist(final_ind_data$indeg_sd[1:100],breaks=20)
plot(final_ind_data$indeg_sd[1:100]~final_ind_data$id[1:100])

#### All together ----

##### Life history traits ----

#longevity
p <- ggplot(final_ind_data[1:100,],aes(x=lng))+
  geom_histogram(binwidth = 5,fill="#af8dc3")+
  xlab("Longevity")+
  theme_classic()

#lro
o <- ggplot(final_ind_data[1:100,],aes(x=lro))+
  geom_histogram(binwidth = 1,fill="#7fbf7b")+
  xlab("LRO")+
  theme_classic()

##### Resource dynamics ----

#stored average
z <- ggplot(final_ind_data[1:100,],aes(x=store_av))+
  geom_histogram(binwidth = 0.1,fill="#d7191c")+
  xlab("Mean stored resources")+
  theme_classic()

#stored sd
x <- ggplot(final_ind_data[1:100,],aes(x=store_sd))+
  geom_histogram(binwidth = 0.1,fill="#d7191c")+
  xlab("SD stored resources")+
  theme_classic()

#produced average
c <- ggplot(final_ind_data[1:100,],aes(x=prod_av))+
  geom_histogram(binwidth = 0.1,fill="#abd9e9")+
  xlab("Mean resources produced")+
  theme_classic()

#produced sd
v <- ggplot(final_ind_data[1:100,],aes(x=prod_sd))+
  geom_histogram(binwidth = 0.1,fill="#abd9e9")+
  xlab("SD resources produced")+
  theme_classic()

#shared average
b <- ggplot(final_ind_data[1:100,])+
  geom_histogram(data=as.data.frame(final_ind_data$outdeg_av[1:100]),aes(x=final_ind_data$outdeg_av[1:100]),fill="#fdae61",alpha=0.7,binwidth=0.1)+
  geom_histogram(data=as.data.frame(final_ind_data$indeg_av[1:100]),aes(x=final_ind_data$indeg_av[1:100]),fill="#2c7bb6",alpha=0.7,binwidth=0.1)+
  xlab("Mean shared resources")+
  theme_classic()

#shared sd
n <- ggplot(final_ind_data[1:100,])+
  geom_histogram(data=as.data.frame(final_ind_data$outdeg_sd[1:100]),aes(x=final_ind_data$outdeg_sd[1:100]),fill="#fdae61",alpha=0.7,binwidth=0.1)+
  geom_histogram(data=as.data.frame(final_ind_data$indeg_sd[1:100]),aes(x=final_ind_data$indeg_sd[1:100]),fill="#2c7bb6",alpha=0.7,binwidth=0.1)+
  xlab("Mean shared resources")+
  theme_classic()

ggarrange(p,z,c,b,o,x,v,n,ncol=4,nrow=2)

### Relationships ----

#### Longevity ~ Storage ----
par(mfrow=c(3,1))
#longevity~total storage
plot(final_ind_data$lng[1:100]~final_ind_data$store_total[1:100],main="longevity")
#longevity~average storage
plot(final_ind_data$lng[1:100]~final_ind_data$store_av[1:100],main="longevity")
#longevity~cv storage
plot(final_ind_data$lng[1:100]~final_ind_data$store_sd[1:100],main="longevity")

#### LRO ~ Storage ----
par(mfrow=c(3,1))
#lro~total storage
plot(final_ind_data$lro[1:100]~final_ind_data$store_total[1:100],main="LRO")
#lro~average storage
plot(final_ind_data$lro[1:100]~final_ind_data$store_av[1:100],main="LRO")
#lro~cv storage
plot(final_ind_data$lro[1:100]~final_ind_data$store_sd[1:100],main="LRO")

#### Longevity ~ Production ----
par(mfrow=c(3,1))
#longevity~total production
plot(final_ind_data$lng[1:100]~final_ind_data$prod_total[1:100],main="longevity")
#longevity~average production
plot(final_ind_data$lng[1:100]~final_ind_data$prod_av[1:100],main="longevity")
#longevity~cv production
plot(final_ind_data$lng[1:100]~final_ind_data$prod_sd[1:100],main="longevity")

#### LRO ~ Production ----
par(mfrow=c(3,1))
#lro~total production
plot(final_ind_data$lro[1:100]~final_ind_data$prod_total[1:100],main="LRO")
#lro~average production
plot(final_ind_data$lro[1:100]~final_ind_data$prod_av[1:100],main="LRO")
#lro~cv production
plot(final_ind_data$lro[1:100]~final_ind_data$prod_sd[1:100],main="LRO")

#### Longevity ~ Sharing ----
par(mfrow=c(3,1))
#longevity~total given away
plot(final_ind_data$lng[1:100]~final_ind_data$outdeg_total[1:100],main="longevity",xlim=c(0,610))
#adding received
points(final_ind_data$lng[1:100]~final_ind_data$indeg_total[1:100],col="red")
#longevity~average given away
plot(final_ind_data$lng[1:100]~final_ind_data$outdeg_av[1:100],main="longevity",xlim=c(0,6))
#adding received
points(final_ind_data$lng[1:100]~final_ind_data$indeg_av[1:100],col="red")
#longevity~cv given away
plot(final_ind_data$lng[1:100]~final_ind_data$outdeg_sd[1:100],main="longevity",xlim=c(0,7))
#adding received
points(final_ind_data$lng[1:100]~final_ind_data$indeg_sd[1:100],col="red")

#### LRO ~ Sharing ----
par(mfrow=c(3,1))
#lro~total given away
plot(final_ind_data$lro[1:100]~final_ind_data$outdeg_total[1:100],main="LRO",xlim=c(0,610))
#adding received
points(final_ind_data$lro[1:100]~final_ind_data$indeg_total[1:100],col="red")
#lro~average given away
plot(final_ind_data$lro[1:100]~final_ind_data$outdeg_av[1:100],main="LRO",xlim=c(0,6))
#adding received
points(final_ind_data$lro[1:100]~final_ind_data$indeg_av[1:100],col="red")
#lro~cv given away
plot(final_ind_data$lro[1:100]~final_ind_data$outdeg_sd[1:100],main="LRO",xlim=c(0,7))
#adding received
points(final_ind_data$lro[1:100]~final_ind_data$indeg_sd[1:100],col="red")

#### Longevity ~ SD resource dynamics ----
par(mfrow=c(3,1))
#longevity~sd storage
lo <- loess(final_ind_data$lng[1:100]~final_ind_data$store_sd[1:100])
plot(final_ind_data$lng[1:100]~final_ind_data$store_sd[1:100],main="Longevity",xlab="SD of stored resources",ylab="Longevity",pch=16)
lines(predict(lo),col="blue",lwd=2)
#longevity~sd production
lo <- loess(final_ind_data$lng[1:100]~final_ind_data$prod_sd[1:100])
plot(final_ind_data$lng[1:100]~final_ind_data$prod_sd[1:100],main="Longevity",xlab="SD of resources produced",ylab="Longevity",pch=16)
lines(predict(lo),col="blue",lwd=2)
#longevity~sd sharing
lo <- loess(final_ind_data$lng[1:100]~final_ind_data$outdeg_sd[1:100])
plot(final_ind_data$lng[1:100]~final_ind_data$outdeg_sd[1:100],main="Longevity",xlab="SD of resources shared",ylab="Longevity",pch=16,ylim=c(-2,120))
lines(predict(lo),col="blue",lwd=2)
#adding received
lo <- loess(final_ind_data$lng[1:100]~final_ind_data$indeg_sd[1:100])
points(final_ind_data$lng[1:100]~final_ind_data$indeg_sd[1:100],col="red",pch=16)
lines(predict(lo),col="gold",lwd=2)

#### LRO ~ SD resource dynamics ----
par(mfrow=c(3,1))
#longevity~cv storage
plot(final_ind_data$lro[1:100]~final_ind_data$store_sd[1:100],main="LRO",xlab="SD of stored resources",ylab="LRO",pch=16)
#longevity~cv production
plot(final_ind_data$lro[1:100]~final_ind_data$prod_sd[1:100],main="LRO",xlab="SD of resources produced",ylab="LRO",pch=16)
#longevity~cv sharing
plot(final_ind_data$lro[1:100]~final_ind_data$outdeg_sd[1:100],main="LRO",xlab="SD of resources shared",ylab="LRO",pch=16)
#adding received
points(final_ind_data$lro[1:100]~final_ind_data$indeg_sd[1:100],col="red",pch=16)

#### Longevity and LRO ~ Mean and SD ----
par(mfrow=c(3,4))
palette(hcl.colors(4,"spectral",alpha=0.5))
#longevity~mean storage
plot(final_ind_data$lng[1:100]~final_ind_data$store_av[1:100],xlab="Mean stored resources",ylab="Longevity",pch=16,col=1,main="A.1",cex.lab=1.2,cex.main=1.2,cex=1.7)
#longevity~sd storage
plot(final_ind_data$lng[1:100]~final_ind_data$store_sd[1:100],xlab="SD stored resources",ylab="Longevity",pch=16,col=1,main="A.2",cex.lab=1.2,cex.main=1.2,cex=1.7)
#lro~mean storage
plot(final_ind_data$lro[1:100]~final_ind_data$store_av[1:100],xlab="Mean stored resources",ylab="LRO",pch=16,col=1,main="B.1",cex.lab=1.2,cex.main=1.2,cex=1.7)
#lro~sd storage
plot(final_ind_data$lro[1:100]~final_ind_data$store_sd[1:100],xlab="SD stored resources",ylab="LRO",pch=16,col=1,main="B.2",cex.lab=1.2,cex.main=1.2,cex=1.7)
#longevity~mean production
plot(final_ind_data$lng[1:100]~final_ind_data$prod_av[1:100],xlab="Mean resources produced",ylab="Longevity",pch=16,col=2,main="C.1",cex.lab=1.2,cex.main=1.2,cex=1.7)
#longevity~sd production
plot(final_ind_data$lng[1:100]~final_ind_data$prod_sd[1:100],xlab="SD resources produced",ylab="Longevity",pch=16,col=2,main="C.2",cex.lab=1.2,cex.main=1.2,cex=1.7)
#lro~mean production
plot(final_ind_data$lro[1:100]~final_ind_data$prod_av[1:100],xlab="Mean resourcs produced",ylab="LRO",pch=16,col=2,main="D.1",cex.lab=1.2,cex.main=1.2,cex=1.7)
#lro~sd production
plot(final_ind_data$lro[1:100]~final_ind_data$prod_sd[1:100],xlab="SD resources produced",ylab="LRO",pch=16,col=2,main="D.2",cex.lab=1.2,cex.main=1.2,cex=1.7)
#longevity~mean sharing
plot(final_ind_data$lng[1:100]~final_ind_data$outdeg_av[1:100],xlab="Mean shared resources",ylab="Longevity",pch=16,col=3,main="E.1",cex.lab=1.2,cex.main=1.2,cex=1.7)
#adding received
points(final_ind_data$lng[1:100]~final_ind_data$indeg_av[1:100],pch=16,col=4,cex=1.7)
#longevity~sd sharing
plot(final_ind_data$lng[1:100]~final_ind_data$outdeg_sd[1:100],xlab="SD shared resources",ylab="Longevity",pch=16,col=3,main="E.2",cex.lab=1.2,cex.main=1.2,cex=1.7)
#adding received
points(final_ind_data$lng[1:100]~final_ind_data$indeg_sd[1:100],pch=16,col=4,cex=1.7)
#lro~mean sharing
plot(final_ind_data$lro[1:100]~final_ind_data$outdeg_av[1:100],xlab="Mean shared resources",ylab="LRO",pch=16,col=3,main="F.1",cex.lab=1.2,cex.main=1.2,cex=1.7)
#adding received
points(final_ind_data$lro[1:100]~final_ind_data$indeg_av[1:100],pch=16,col=4,cex=1.7)
#lro~sd sharing
plot(final_ind_data$lro[1:100]~final_ind_data$outdeg_sd[1:100],xlab="SD shared resources",ylab="LRO",pch=16,col=3,main="F.2",cex.lab=1.2,cex.main=1.2,cex=1.7)
#adding received
points(final_ind_data$lro[1:100]~final_ind_data$indeg_sd[1:100],pch=16,col=4,cex=1.7)



#### Ggplot version of relationships ----

##### Average ----

#lng~store_av
a <- ggplot(final_ind_data[1:100,],aes(x=store_av,y=lng))+
  geom_point(color="#d7191c",alpha=0.5,size=3)+
  scale_color_brewer(palette="RdYlBu") +
  xlab("Mean stored resources") +
  ylab("Longevity") +
  theme_classic()
#lro~store_av
s <- ggplot(final_ind_data[1:100,],aes(x=store_av,y=lro))+
  geom_point(color="#d7191c",alpha=0.5,size=3)+
  xlab("Mean stored resources") +
  ylab("LRO") +
  theme_classic()
#lng~prod_av
d <- ggplot(final_ind_data[1:100,],aes(x=prod_av,y=lng))+
  geom_point(color="#abd9e9",alpha=0.5,size=3)+
  xlab("Mean resources produced") +
  ylab("Longevity") +
  theme_classic()
#lro~prod_av
f <- ggplot(final_ind_data[1:100,],aes(x=prod_av,y=lro))+
  geom_point(color="#abd9e9",alpha=0.5,size=3)+
  xlab("Mean resources produced") +
  ylab("LRO") +
  theme_classic()
#lng~sharing sdc
g <- ggplot(final_ind_data[1:100,],aes(x=outdeg_av,y=lng))+
  geom_point(color="#fdae61",alpha=0.5,size=3)+
  geom_point(aes(x=indeg_av,y=lng),color="#2c7bb6",alpha=0.5,size=3)+
  xlab("Mean shared resources") +
  ylab("Longevity") +
  theme_classic()
#lro~sharing sd
h <- ggplot(final_ind_data[1:100,],aes(x=outdeg_av,y=lro))+
  geom_point(color="#fdae61",alpha=0.5,size=3)+
  geom_point(aes(x=indeg_av,y=lro),color="#2c7bb6",alpha=0.5,size=3)+
  xlab("Mean shared resources") +
  ylab("LRO") +
  theme_classic()

#ggarrange(a,s,d,f,g,h,ncol=2,nrow=3,labels=c("A.1","B.1","C.1","D.1","E.1","F.1"))

##### Variability ----

#lng~store_sd
q <- ggplot(final_ind_data[1:100,],aes(x=store_sd,y=lng))+
  geom_point(color="#d7191c",alpha=0.5,size=3)+
  xlab("SD stored resources") +
  ylab("Longevity") +
  theme_classic()
#lro~store_sd
w <- ggplot(final_ind_data[1:100,],aes(x=store_sd,y=lro))+
  geom_point(color="#d7191c",alpha=0.5,size=3)+
  xlab("SD stored resources") +
  ylab("LRO") +
  theme_classic()
#lng~prod_sd
e <- ggplot(final_ind_data[1:100,],aes(x=prod_sd,y=lng))+
  geom_point(color="#abd9e9",alpha=0.5,size=3)+
  xlab("SD resources produced") +
  ylab("Longevity") +
  theme_classic()
#lro~prod_sd
r <- ggplot(final_ind_data[1:100,],aes(x=prod_sd,y=lro))+
  geom_point(color="#abd9e9",alpha=0.5,size=3)+
  xlab("SD resources produced") +
  ylab("LRO") +
  theme_classic()
#lng~sharing sd
t <- ggplot(final_ind_data[1:100,],aes(x=outdeg_sd,y=lng))+
  geom_point(color="#fdae61",alpha=0.5,size=3)+
  geom_point(aes(x=indeg_sd,y=lng),color="#2c7bb6",alpha=0.5,size=3)+
  xlab("SD shared resources") +
  ylab("Longevity") +
  theme_classic()
#lro~sharing sd
y <- ggplot(final_ind_data[1:100,],aes(x=outdeg_sd,y=lro))+
  scale_color_brewer(palette = "RdBu") +
  geom_point(color="#fdae61",alpha=0.5,size=3)+
  geom_point(aes(x=indeg_sd,y=lro),color="#2c7bb6",alpha=0.5,size=3)+
  xlab("SD shared resources") +
  ylab("LRO") +
  theme_classic()

#ggarrange(q,w,e,r,t,y,ncol=2,nrow=3,labels=c("A.2","B.2","C.2","D.2","E.2","F.2"),common.legend = TRUE,legend="bottom")

##### All together ----

ggarrange(a,q,s,w,d,e,f,r,g,t,h,y,ncol=4,nrow=3)

ggarrange(z,x,a,q,s,w,c,v,d,e,f,r,b,n,g,t,h,y,ncol=6,nrow=3)
