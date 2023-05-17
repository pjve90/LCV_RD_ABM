# Life cycle variation and resource dynamics ABM: Code ----

## R settings ----

#set working directory
getwd()
setwd("./LCV_RD_ABM/Model_code")

## Initial population ----

#create population
create_initialpop<-function(populationsize){
it_indpop <- data.frame(id=1:populationsize, #id
                        stage=rep(2,length.out=populationsize), #life cycle stage
                        store_a=rep(0,length.out=populationsize), #stored resources
                        prod_a=rep(0,length.out=populationsize), #production amount
                        mom_id=rep(NA,length.out=populationsize), #mom id
                        mom_surplus=rep(NA,length.out=populationsize), #identify mom surplus
                        mom_surplus_a=rep(NA,length.out=populationsize), #mom surplus amount
                        desc_need=rep(0,length.out=populationsize), #identify descendant need
                        desc_need_a=rep(NA,length.out=populationsize), #descendant need amount
                        max_deg=rep(NA,length.out=populationsize), #surplus for resource transfers
                        in_degree=rep(NA,length.out=populationsize), #amount of resources received
                        out_degree=rep(NA,length.out=populationsize), #amount of resources given away
                        repro=rep(NA,length.out=populationsize), #reproduction output
                        lro=rep(0,length.out=populationsize), #lifetime reproductive output
                        tlr=rep(0,length.out=populationsize), #time since last reproduction
                        surv=rep(NA,length.out=populationsize), #survival output
                        age=rep(10,length.out=populationsize) #age
)
return(it_indpop)
}

it_indpop<-create_initialpop(100)

#create data frame to record the dynamics in each iteration
it_data <- data.frame(id=1:nrow(it_indpop))

#create data frame to record need of descendants
it_descpop <- data.frame(id=1:nrow(it_indpop))

## Resource production ----

#Habitat quality
habitat_quality<-4
age_specific_abilities<-c(0.25,1,1,0.33)
habitat <- round(habitat_quality * age_specific_abilities,0) 
names(habitat) <- c("juvenile","adult","reproductive career", "post-reproductive")

#Stage-specific probabilities of production
prod_prob <- c(0.1,0.5,0.5,0.4)
names(prod_prob) <- c("juvenile","adult","reproductive career", "post-reproductive")

#Production function
source("production_fx.R")

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
# overall tendency to share
prob_ties <- 1e-10

# weights of sharing directionality
block_offsets<-matrix(c(1,8,8,8,
                        1,1,1,1,
                        1,2,2,3,
                        1,3,3,2),
nrow=num_blocks,ncol=num_blocks)

block_probs<-prob_ties*block_offsets 


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
repro_thresh <- surv_cost*10+surv_cost

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
  it_indpop <- produce(it_indpop)
}
#record production outcome and amount
#separate data
resource_data <- it_indpop[,c("id","prod_a")]

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

it_indpop<-create_initialpop(100)

for (b in 1:100){
  
  #record the maximum id
  max_id <- max(it_indpop$id)
  
  #production
  for (i in 1:nrow(it_indpop)){
    #production
    it_indpop <- produce(it_indpop)
  }
  #record production outcome and amount
  resource_data <- it_indpop[,c("id","prod_a")]
  
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

#change NAs in standard deviation of resource dynamics into 0
#storage
final_ind_data$store_sd[which(is.na(final_ind_data$store_sd)==TRUE)] <- 0
#production
final_ind_data$prod_sd[which(is.na(final_ind_data$prod_sd)==TRUE)] <- 0
#given away
final_ind_data$outdeg_sd[which(is.na(final_ind_data$outdeg_sd)==TRUE)] <- 0
#received
final_ind_data$indeg_sd[which(is.na(final_ind_data$indeg_sd)==TRUE)] <- 0

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
