# Life cycle variation and resource dynamics ABM: Code ----

## R settings ----

#set working directory
getwd()
setwd("./LCV_RD_ABM/Model_code")

## Initial population ----

#create population
it_indpop <- data.frame(id=1:100, #id
                        stage=rep(2,length.out=100), #life cycle stage
                        store_a=rep(0,length.out=100), #stored resources
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
                        age=rep(10,length.out=25) #age
)

#create data frame to record the dynamics in each iteration
it_data <- data.frame(id=1:nrow(it_indpop))

#create data frame to record need of descendants
it_descpop <- data.frame(id=1:nrow(it_indpop))

## Resource production ----

#Habitat quality
habitat <- c(1,4,4,1)
names(habitat) <- c("juvenile","adult","reproductive career", "post-reproductive")

#Stage-specific probabilities of production
prod_prob <- c(0.1,0.5,0.5,0.1)
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

# Specify the number of of blocks
num_blocks <- 4

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


## Survival ----

#Survival cost
surv_cost <- 1

#Survival
source("survival_survive_fx.R")

#Discount of survival cost
source("survival_discount.R")

#Age
source("survival_age.R")

## Reproduction ----

#Number of descendants per reproduction
n_desc <- 1

#Reproductive threshold
repro_thresh <- surv_cost*10

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

## Run one iteration ----

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
#separate data
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
#separate data
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
  it_indpop <- transition(it_indpop)
}
#record time since last reproduction and transition
transition_data <- it_indpop[,c("id","tlr","stage")]
#record reproduction again to update in case of transition to reproductive career
repro_data <- it_indpop[,c("id","repro","lro")]

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
it_data <- Reduce(function(x,y)merge(x,y,all=TRUE),list(
  it_data,
  resource_data,
  mat_invest_data,
  transfers_data,
  repro_data,
  transition_data,
  surv_data
))
#record iteration
it_data$year <- rep(1,length.out=nrow(it_data))

#remove individual who died
it_indpop <- it_indpop[!it_indpop$surv==0,]

#check the population by the end of the iteration
head(it_indpop)
#check the recorded data by the end of the iteration
head(it_data)

## Run 100 iterations ----

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
    it_indpop <- transition(it_indpop)
  }
  #record time since last reproduction, life cycle stage, reproduction, and lro
  transition_data <- it_indpop[,c("id","tlr","stage")]
  #record reproduction again to update in case of transition to reproductive career
  repro_data <- it_indpop[,c("id","repro","lro")]
  
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
  if(b==1){
  it_data <- Reduce(function(x,y)merge(x,y,all=TRUE),list(
    resource_data,
    mat_invest_data,
    transfers_data,
    repro_data,
    transition_data,
    surv_data
  ))
  #record iteration
  it_data$year <- rep(b,length.out=nrow(it_data))
  }else{
    it_data2 <- Reduce(function(x,y)merge(x,y,all=TRUE),list(
      resource_data,
      mat_invest_data,
      transfers_data,
      repro_data,
      transition_data,
      surv_data
    ))
    #record iteration
    it_data2$year <- rep(b,length.out=nrow(it_data2))
    #merge with previous iteration records
    it_dataf <- rbind(it_data,it_data2)
    it_dataf <- it_dataf[order(it_dataf$id),]
    #update it_data for rbind
    it_data <- it_dataf
  }
  #remove individuals that died
  it_indpop <- it_indpop[!it_indpop$surv==0,]

}

#check the population by the end of the iteration
head(it_indpop)
#check the recorded data by the end of the iteration
head(it_dataf)

## Calculate outcomes from the model ----

### Life history traits ----

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
final_ind_data <- data.frame(id=1:max(it_dataf$id))

#calculate life history traits
for(i in 1:max(it_dataf$id)){
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

#calculate resource dynamics
for(i in 1:max(it_dataf$id)){
  #resource dynamics
  #storage
  #total
  final_ind_data$store_total <- storage_total(final_ind_data)
  #average
  final_ind_data$store_av <- storage_av(final_ind_data)
  #variability
  final_ind_data$store_sd <- storage_cv(final_ind_data)
  #production
  #total
  final_ind_data$prod_total <- produce_total(final_ind_data)
  #average
  final_ind_data$prod_av <- produce_av(final_ind_data)
  #variability
  final_ind_data$prod_sd <- produce_cv(final_ind_data)
  #given away
  #total
  final_ind_data$outdeg_total <- outdegree_total(final_ind_data)
  #average
  final_ind_data$outdeg_av <- outdegree_av(final_ind_data)
  #variability
  final_ind_data$outdeg_sd <- outdegree_cv(final_ind_data)
  #received
  #total
  final_ind_data$indeg_total <- indegree_total(final_ind_data)
  #average
  final_ind_data$indeg_av <- indegree_av(final_ind_data)
  #variability
  final_ind_data$indeg_sd <- indegree_cv(final_ind_data)
}

#check dataset
head(final_ind_data)

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
lo <- loess(final_ind_data$lro[1:100]~final_ind_data$store_sd[1:100])
plot(final_ind_data$lro[1:100]~final_ind_data$store_sd[1:100],main="LRO",xlab="SD of stored resources",ylab="LRO",pch=16)
lines(predict(lo),col="blue",lwd=2)
#longevity~cv production
lo <- loess(final_ind_data$lro[1:100]~final_ind_data$prod_sd[1:100])
plot(final_ind_data$lro[1:100]~final_ind_data$prod_sd[1:100],main="LRO",xlab="SD of resources produced",ylab="LRO",pch=16)
lines(predict(lo),col="blue",lwd=2)
#longevity~cv sharing
lo <- loess(final_ind_data$lro[1:100]~final_ind_data$outdeg_sd[1:100])
plot(final_ind_data$lro[1:100]~final_ind_data$outdeg_sd[1:100],main="LRO",xlab="SD of resources shared",ylab="LRO",pch=16)
lines(predict(lo),col="blue",lwd=2)
#adding received
lo <- loess(final_ind_data$lro[1:100]~final_ind_data$indeg_sd[1:100])
points(final_ind_data$lro[1:100]~final_ind_data$indeg_sd[1:100],col="red",pch=16)
lines(predict(lo),col="gold",lwd=2)

#### Ggplot version of relationships ----

#lng~store_sd
q <- ggplot(final_ind_data[1:100,],aes(x=store_sd,y=lng))+
  scale_color_brewer(palette = "RdBu") +
  geom_point(color=1,alpha=0.5)+
  geom_smooth(method = "lm", formula = y ~ splines::bs(x, 4), se = FALSE,color=1)+
  coord_cartesian(xlim=c(0,8),ylim=c(0,110)) +
  xlab("SD stored resources") +
  ylab("Longevity") +
  theme_classic()
#lro~store_sd
w <- ggplot(final_ind_data[1:100,],aes(x=store_sd,y=lro))+
  scale_color_brewer(palette = "RdBu") +
  geom_point(color=1,alpha=0.5)+
  geom_smooth(method = "lm", formula = y ~ splines::bs(x, 3), se = FALSE,color=1)+
  coord_cartesian(xlim=c(0,8),ylim=c(-1.5,13)) +
  xlab("SD stored resources") +
  ylab("LRO") +
  theme_classic()
#lng~prod_sd
e <- ggplot(final_ind_data[1:100,],aes(x=prod_sd,y=lng))+
  scale_color_brewer(palette = "RdBu") +
  geom_point(color=2,alpha=0.5)+
  geom_smooth(method = "lm", formula = y ~ splines::bs(x, 5), se = FALSE,color=2)+
  coord_cartesian(xlim=c(0,2.3),ylim=c(-2,110)) +
  xlab("SD resources produced") +
  ylab("Longevity") +
  theme_classic()
#lro~prod_sd
r <- ggplot(final_ind_data[1:100,],aes(x=prod_sd,y=lro))+
  scale_color_brewer(palette = "RdBu") +
  geom_point(color=2,alpha=0.5)+
  geom_smooth(method = "lm", formula = y ~ splines::bs(x, 3), se = FALSE,color=2)+
  coord_cartesian(xlim=c(0,2.3),ylim=c(-0.5,13)) +
  xlab("SD resources produced") +
  ylab("LRO") +
  theme_classic()
#lng~sharing sdc
t <- ggplot(final_ind_data[1:100,],aes(x=outdeg_sd,y=lng))+
  scale_color_brewer(palette = "RdBu") +
  geom_point(color=3,alpha=0.5)+
  geom_point(aes(x=indeg_sd,y=lng),show.legend = FALSE,color=4,alpha=0.5)+
  geom_smooth(method = "lm", formula = y ~ splines::bs(x, 6), se = FALSE,color=3)+
  geom_smooth(aes(x=indeg_sd,y=lng),method = "lm", formula = y ~ splines::bs(x, 4), se = FALSE,color=4)+
  coord_cartesian(ylim=c(-0.5,110)) +
  xlab("SD shared resources") +
  ylab("Longevity") +
  theme_classic()
#lro~sharing sd
y <- ggplot(final_ind_data[1:100,],aes(x=outdeg_sd,y=lro))+
  scale_color_brewer(palette = "RdBu") +
  geom_point(color=3,alpha=0.5)+
  geom_point(aes(x=indeg_sd,y=lro),show.legend = FALSE,color=4,alpha=0.5)+
  geom_smooth(method = "lm", formula = y ~ splines::bs(x, 4), se = FALSE,color=3)+
  geom_smooth(aes(x=indeg_sd,y=lro),method = "lm", formula = y ~ splines::bs(x, 3), se = FALSE,color=4)+
  coord_cartesian(ylim=c(-0.5,13)) +
  xlab("SD shared resources") +
  ylab("LRO") +
  theme_classic()

ggarrange(q,w,e,r,t,y,ncol=2,nrow=3,labels=c("A.2","B.2","C.2","D.2","E.2","F.2"),common.legend = TRUE,legend="bottom")

#lng~store_av
a <- ggplot(final_ind_data[1:100,],aes(x=store_av,y=lng))+
  scale_color_brewer(palette = "RdBu") +
  geom_point(color=1,alpha=0.5)+
  geom_smooth(method = "lm", formula = y ~ splines::bs(x, 3), se = FALSE,color=1)+
  coord_cartesian(xlim=c(0,13),ylim=c(0,110)) +
  xlab("Mean stored resources") +
  ylab("Longevity") +
  theme_classic()
#lro~store_av
s <- ggplot(final_ind_data[1:100,],aes(x=store_av,y=lro))+
  scale_color_brewer(palette = "RdBu") +
  geom_point(color=1,alpha=0.5)+
  geom_smooth(method = "lm", formula = y ~ splines::bs(x, 3), se = FALSE,color=1)+
  coord_cartesian(xlim=c(0,13),ylim=c(-2.5,13)) +
  xlab("Mean stored resources") +
  ylab("LRO") +
  theme_classic()
#lng~prod_av
d <- ggplot(final_ind_data[1:100,],aes(x=prod_av,y=lng))+
  scale_color_brewer(palette = "RdBu") +
  geom_point(color=2,alpha=0.5)+
  geom_smooth(method = "lm", formula = y ~ splines::bs(x, 5), se = FALSE,color=2)+
  coord_cartesian(ylim=c(-2,110)) +
  xlab("Mean resources produced") +
  ylab("Longevity") +
  theme_classic()
#lro~prod_av
f <- ggplot(final_ind_data[1:100,],aes(x=prod_av,y=lro))+
  scale_color_brewer(palette = "RdBu") +
  geom_point(color=2,alpha=0.5)+
  geom_smooth(method = "lm", formula = y ~ splines::bs(x, 5), se = FALSE,color=2)+
  coord_cartesian(ylim=c(-0.5,13)) +
  xlab("Mean resources produced") +
  ylab("LRO") +
  theme_classic()
#lng~sharing sdc
g <- ggplot(final_ind_data[1:100,],aes(x=outdeg_av,y=lng))+
  scale_color_brewer(palette = "RdBu") +
  geom_point(color=3,alpha=0.5)+
  geom_point(aes(x=indeg_av,y=lng),color=4,alpha=0.5)+
  geom_smooth(method = "lm", formula = y ~ splines::bs(x, 3), se = FALSE,color=3)+
  geom_smooth(aes(x=indeg_av,y=lng),method = "lm", formula = y ~ splines::bs(x, 3), se = FALSE,color=4)+
  coord_cartesian(ylim=c(-0.5,110)) +
  xlab("Mean shared resources") +
  ylab("Longevity") +
  theme_classic()
#lro~sharing sd
h <- ggplot(final_ind_data[1:100,],aes(x=outdeg_av,y=lro))+
  scale_color_brewer(palette = "RdBu") +
  geom_point(color=3,alpha=0.5)+
  geom_point(aes(x=indeg_av,y=lro),show.legend = FALSE,color=4,alpha=0.5)+
  geom_smooth(method = "lm", formula = y ~ splines::bs(x, 3), se = FALSE,color=3)+
  geom_smooth(aes(x=indeg_av,y=lro),method = "lm", formula = y ~ splines::bs(x, 4), se = FALSE,color=4)+
  coord_cartesian(ylim=c(-0.5,13)) +
  xlab("Mean shared resources") +
  ylab("LRO") +
  theme_classic()

ggarrange(a,s,d,f,g,h,ncol=2,nrow=3,labels=c("A.1","B.1","C.1","D.1","E.1","F.1"))

ggarrange(a,q,s,w,d,e,f,r,g,t,h,y,ncol=4,nrow=3,labels=c("A.1","A.2","B.1","B.2","C.1","C.2","D.1","D.2","E.1","E.2","F.1","F.2"),common.legend=T,legend="bottom")
