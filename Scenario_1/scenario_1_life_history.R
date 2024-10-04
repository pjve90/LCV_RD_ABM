# Life cycle variation and resource dynamics ABM: Scenario 1 - life history calculation ----

#Here is the code to calculate the life history tratis from the simulation of Scenario 1, which aims to understand the influence of resource production on the variability of life cycles.

#Production: parameter sweep between 0.1 and 0.9 (17 values)
#Transfers: null
#Habitat quality: baseline

#R settings ----

getwd()
#setwd("./LCV_RD_ABM")

#Data wrangling ----

#import raw results from the simulation
raw_sim <- readRDS("./Scenario_1/raw_simulation.RData")
#create empty list
raw_sample <- vector("list", 170)
#loop through every simulation to get the sample
for(i in 1:length(raw_sim)){
  #not born before year 100
  raw_sample[[i]] <- raw_sim[[i]][which(!(raw_sim[[i]]$id %in% raw_sim[[i]]$id[which(raw_sim[[i]]$year <100 )])),]
  #not born after year 200
  raw_sample[[i]] <- raw_sample[[i]][which(!(raw_sample[[i]]$id %in% raw_sample[[i]]$id[which(raw_sample[[i]]$year > 200 & raw_sample[[i]]$age == 0 )])),]
  
}
#match the original names with the sample
names(raw_sample) <- names(raw_sim)
names(raw_sample)

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

# Calculate life history traits ----

#create data set
#create list
lht_list <- vector("list", 170)
#match the original names with the list
names(lht_list) <- names(raw_sim)
names(lht_list)
#create data frame within each element of the list
for(i in 1:length(lht_list)){
  lht_list[[i]] <- data.frame(id=levels(as.factor(raw_sample[[i]]$id)))
}
#check
head(lht_list)

#Longevity ----

for (i in 1:length(lht_list)){
  if (nrow(lht_list[[i]]) == 0){
    lht_list[[i]] <- lht_list[[i]]
  } else{
    lht_list[[i]] <- longevity(lht_list)
  }
}

#Lifetime reproductive output ----

for (i in 1:length(lht_list)){
  if (nrow(lht_list[[i]]) == 0){
    lht_list[[i]] <- lht_list[[i]]
  } else{
    lht_list[[i]] <- lifetime_reproductive_output(lht_list)
  }
}

#Age at sexual maturity ----

for (i in 1:length(lht_list)){
  if (nrow(lht_list[[i]]) == 0){
    lht_list[[i]] <- lht_list[[i]]
  } else{
    lht_list[[i]] <- asm(lht_list)
  }
}

#Age at first reproduction ----

for (i in 1:length(lht_list)){
  if (nrow(lht_list[[i]]) == 0){
    lht_list[[i]] <- lht_list[[i]]
  } else{
    lht_list[[i]] <- afr(lht_list)
  }
}

#Age at last reproduction ----

for (i in 1:length(lht_list)){
  if (nrow(lht_list[[i]]) == 0){
    lht_list[[i]] <- lht_list[[i]]
  } else{
    lht_list[[i]] <- alr(lht_list)
  }
}

#Age at menopause ----

for (i in 1:length(lht_list)){
  if (nrow(lht_list[[i]]) == 0){
    lht_list[[i]] <- lht_list[[i]]
  } else{
    lht_list[[i]] <- meno(lht_list)
  }
}

#save the data ----

saveRDS(lht_list,file="./Scenario_1/lht_list.RData")
