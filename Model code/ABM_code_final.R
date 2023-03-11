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
source("https://raw.githubusercontent.com/pjve90/LCV_RD_ABM/main/Model%20code/production_fx.R?token=GHSAT0AAAAAAB5C6IJP7OVLDTQ47RB3B2JSZAMNEFA")

#Production amount
source("https://raw.githubusercontent.com/pjve90/LCV_RD_ABM/main/Model%20code/production_amount.R?token=GHSAT0AAAAAAB5C6IJOWV645R37WB7RDO24ZAMQDMA")

## Maternal investment ----

#Identify if the mother has surplus of resources
source("https://raw.githubusercontent.com/pjve90/LCV_RD_ABM/main/Model%20code/mat_invest_mom_surp_identify.R?token=GHSAT0AAAAAAB5C6IJPNYXCAMEJOMQOA6G2ZAMNE3A")

#Identify the amount of surplus of the mother
source("https://raw.githubusercontent.com/pjve90/LCV_RD_ABM/main/Model%20code/mat_invest_mom_surp_amount.R?token=GHSAT0AAAAAAB5C6IJPSKBSAVNCSCSCMOD2ZAMNFDQ")

#Identify if the descendants need resources
source("https://raw.githubusercontent.com/pjve90/LCV_RD_ABM/main/Model%20code/mat_invest_desc_need_identify.R?token=GHSAT0AAAAAAB5C6IJODS7JEK3OE42R5WQCZAMNFLQ")

#Identify the amount of need for each descendant
source("https://raw.githubusercontent.com/pjve90/LCV_RD_ABM/main/Model%20code/mat_invest_desc_need_amount.R?token=GHSAT0AAAAAAB5C6IJO5IKJETEVDQMLHGVIZAMNFVA")

#Order the descendants by need and mother id
source("https://raw.githubusercontent.com/pjve90/LCV_RD_ABM/main/Model%20code/mat_invest_desc_order.R?token=GHSAT0AAAAAAB5C6IJPDZXJHSUXITIQUHXGZAMNF5A")

#Mother invest in her descendants
source("https://raw.githubusercontent.com/pjve90/LCV_RD_ABM/main/Model%20code/mat_invest_fx.R?token=GHSAT0AAAAAAB5C6IJOICVAPJXBZMKAJL2MZAMNGGA")

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
source("https://raw.githubusercontent.com/pjve90/LCV_RD_ABM/main/Model%20code/transfers_surplus_sbm.R?token=GHSAT0AAAAAAB5C6IJOVVLMHPJUKOVKPTJWZAMNHCQ")

#Generate the network
source("https://raw.githubusercontent.com/pjve90/LCV_RD_ABM/main/Model%20code/transfers_sbm_fx.R?token=GHSAT0AAAAAAB5C6IJOTLIYIORRFC4SN26YZAMNHQA")

#Record the resources transferred
source("https://raw.githubusercontent.com/pjve90/LCV_RD_ABM/main/Model%20code/transfers_amount.R?token=GHSAT0AAAAAAB5C6IJPG2T63PCMGJJCXY6IZAMNH2A") 

## Reproduction ----

#Number of descendants per reproduction
n_desc <- 1

#Reproductive threshold
repro_thresh <- surv_cost*10

#Reproductive cost
repro_cost <- surv_cost

#Reproduction
source("https://raw.githubusercontent.com/pjve90/LCV_RD_ABM/main/Model%20code/reproduction_reproduce_fx.R?token=GHSAT0AAAAAAB5C6IJOEFYKZJMK7GOWHX66ZAMNJRA")

#Discount of reproductive cost
source("https://raw.githubusercontent.com/pjve90/LCV_RD_ABM/main/Model%20code/reproduction_discount.R?token=GHSAT0AAAAAAB5C6IJPGAWTF5IVP7TZC7ZOZAMNJXQ")

#Lifetime reproductive output
source("https://raw.githubusercontent.com/pjve90/LCV_RD_ABM/main/Model%20code/reproduction_lro.R?token=GHSAT0AAAAAAB5C6IJP2AGT55O4MWTU7D4GZAMNJ6Q")

#Add newborns
source("https://raw.githubusercontent.com/pjve90/LCV_RD_ABM/main/Model%20code/reproduction_newborn.R?token=GHSAT0AAAAAAB5C6IJO3TAXKZYPIDOV754WZAMNKHQ")

## Transition ----

#Time since last birth
source("https://raw.githubusercontent.com/pjve90/LCV_RD_ABM/main/Model%20code/transition_tlr.R?token=GHSAT0AAAAAAB5C6IJPVKRFUXAFE3FECBTGZAMNKSA")

#Transition
source("https://raw.githubusercontent.com/pjve90/LCV_RD_ABM/main/Model%20code/transition_fx.R?token=GHSAT0AAAAAAB5C6IJPA3TGXKPUHKX2PGLSZAMNKYA")

## Survival ----

#Survival cost
surv_cost <- 1

#Survival
source("https://raw.githubusercontent.com/pjve90/LCV_RD_ABM/main/Model%20code/survival_survive_fx.R?token=GHSAT0AAAAAAB5C6IJODQ6WPQYVNHP3ZJRYZAMNUQA")

#Discount of survival cost
source("https://raw.githubusercontent.com/pjve90/LCV_RD_ABM/main/Model%20code/survival_discount.R?token=GHSAT0AAAAAAB5C6IJPYWOFKVFIDB5C5HXIZAMNUVA")

#Age
source("https://raw.githubusercontent.com/pjve90/LCV_RD_ABM/main/Model%20code/survival_age.R?token=GHSAT0AAAAAAB5C6IJORB67LEXIEATX7OP6ZAMNU3A")

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
  #desc_order(it_indpop)
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
network <- simulate_SBM_max_degree(N,block_sizes,block_probs,it_indpop$max_deg,it_indpop$stage)
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
surv_data <- it_indpop[,c("id","surv","age","store_a")]

#combine original population with newborns
it_indpop <- rbind(it_indpop,new_it_indpop)
#remove NA in id
it_indpop <- it_indpop[!is.na(it_indpop$id),]

#merge iteration records
it_data <- reduce(list(it_data,surv_data,repro_data,resource_data,trans_data),full_join,by="id",suffix=c("0","1"))

#check the population by the end of the iteration
head(it_indpop)
#check the recorded data by the end of the iteration
head(it_data)

