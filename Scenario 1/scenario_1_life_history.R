# Life cycle variation and resource dynamics ABM: Scenario 1 - life history calculation ----

#Here is the code to calculate the life history tratis from the simulation of Scenario 1, which aims to understand the influence of resource production on the variability of life cycles.

#Production: parameter sweep between 0.1 and 0.9 (17 values)
#Transfers: null
#Habitat quality: baseline

#R settings ----

getwd()
setwd("./LCV_RD_ABM")

#Data wrangling ----

#import raw results from the simulation
raw_sim <- readRDS("./Scenario 1/raw_simulation.RData")
#create empty list
raw_sample <- list()
#loop through every simulation to get the sample
for(i in 1:length(raw_sim)){
  #not born before year 100
  raw_sample[[i]] <- raw_sim[[i]]$it_dataf[which(!(raw_sim[[i]]$it_dataf$id %in% raw_sim[[i]]$it_dataf$id[which(raw_sim[[i]]$it_dataf$year <100 )])),]
  #not born after year 200
  raw_sample[[i]] <- raw_sample[[i]][which(!(raw_sample[[i]]$id %in% raw_sample[[i]]$id[which(raw_sample[[i]]$year > 200 & raw_sample[[i]]$age == 0 )])),]
  
}

#Import functions to calculate life history traits ----


#longevity
source("./Model_code/life_history_lng.R")

#lifetime reproductive output
source("./Model_code/life_history_lro.R")

#age at sexual maturity
source("./Model_code/life_history_asm.R")

#age at first reproduction
source("./Model_code/life_history_afr.R")

#age at last reproduction
source("./Model_code/life_history_alr.R")

#age at menopause
source("./Model_code/life_history_meno.R")

#Longevity ----

for (i in 1:length(raw_sample)){
  if(sum(is.na(raw_sample[[i]])) != 16 & nrow(raw_sample[[i]]) > 1){
    
  }
  
}

max(raw_sample[[1]][which(raw_sample[[1]]$id==i),"age"])


