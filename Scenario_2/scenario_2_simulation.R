# Life cycle variation and resource dynamics ABM: Scenario 2 - Code ----

#Here is the code to set up the simulation of Scenario 2, which aims to understand the influence of habitat quality on the variability of life cycles.

#Production: parameter sweep between 0.1 and 0.9 (17 values)
#Transfers: null
#Habitat quality: baseline

## R settings ----

#set working directory
getwd()
#setwd("./LCV_RD_ABM")

#install packages
#parallel package
#install.packages("parallel")
library(parallel)
#doParallel
#install.packages("doParrallel")
library(doParallel)
#foreach package
#install.packages("foreach")
library(foreach)
#fst package
#install.packages("fst")
library(fst)

## Sourcing functions ----

#In this section, you source the different R functions needed to set up the state and auxiliary variables for the model.

### Initial population ----

#create population
source("./Model_code/initial_pop_fx.R")

### Resource production ----

#Stage-specific maximum amount of resource production 
source("./Model_code/production_maxprod_fx.R")

#Stage-specific production probabilities
source("./Model_code/production_prodprob_fx.R")

#Production function
#production function of scenario 2 has no d value, since the production probabilities are set to baseline
source("./Model_code/production_fx_s2.R")

### Maternal investment ----

#Identify if the mother has surplus of resources
source("./Model_code/mat_invest_mom_surp_identify.R")

#Identify the amount of surplus of the mother
source("./Model_code/mat_invest_mom_surp_amount.R")

#Identify if the descendants need resources
source("./Model_code/mat_invest_desc_need_identify.R")

#Identify the amount of need for each descendant
source("./Model_code/mat_invest_desc_need_amount.R")

#Order the descendants by need and mother id
source("./Model_code/mat_invest_desc_order.R")

#Mother invest in her descendants
source("./Model_code/mat_invest_fx.R")

### Reproduction ----

#Reproduction
source("./Model_code/reproduction_reproduce_fx.R")

#Discount of reproductive cost
source("./Model_code/reproduction_discount.R")

#Lifetime reproductive output
source("./Model_code/reproduction_lro.R")

#Add newborns
source("./Model_code/reproduction_newborn.R")

### Transition ----

#Time since last birth
source("./Model_code/transition_tlr.R")

#Transition
source("./Model_code/transition_fx.R")

#Transition
source("./Model_code/transition_fx.R")

### Survival ----

#Survival
source("./Model_code/survival_survive_fx.R")

#Discount of survival cost
source("./Model_code/survival_discount.R")

#Age
source("./Model_code/survival_age.R")

### Resource storing ----

#Store resources
source("./Model_code/storage_fx.R")

### Population splitting ----

source("./Model_code/split_pop_fx.R")

# Run for 300 iterations ----

#Here, you run the simulation for a 300 iterations, resembling 300 years.

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
habitat_quality <- 8
#stage-specific maximum amount of resource production.
maxprod <- max_production(habitat_quality)

#maximum production probability
max_prod_prob <- 0.5
#stage-specific production probabilities.
prod_prob <- production_prob(max_prod_prob)

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

### Parallelisation settings -----

unregister_dopar <- function() {
  env <- foreach:::.foreachGlobals
  rm(list = ls(name = env), pos = env)
}

unregister_dopar()

# Set number of cores to 100 if dynamic_cores is less than 100
num_cores <- 100

# Check the number of cores to use
cat("Number of cores to use:", num_cores, "\n")

#create the cluster
my_cluster <- makeCluster(
  num_cores,
  type="FORK"
) # Use 'FORK' for Unix-based systems (Linux/macOS)

#register the cluster
registerDoParallel(cl = my_cluster)

#check if cluster is registered
getDoParRegistered() # If the cluster is registered
getDoParWorkers() # Number of cores registered

#### Parameter sweep ----

#set seed
set.seed(1992)

# Directory to save .fst files
output_dir <- "Scenario_2/fst_results"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

#number of repetitions
reps <- 10

# Calculate total number of tasks
total_tasks <- reps # 10 repetitions

# Dynamically calculate batch size
batch_size <- ceiling(total_tasks / num_cores)  # Batch size based on total tasks and available cores

# Parallelise the parameter sweep
foreach(batch = 1:ceiling(total_tasks / batch_size)) %dopar% {

#load fst package
library(fst)

# Calculate the range of tasks for this batch
start_task <- (batch - 1) * batch_size + 1
end_task <- min(batch * batch_size, total_tasks)
                          
batch_results <- list()  # To store the results for this batch

for (task_idx in start_task:end_task) {
  r <- task_idx  # Directly use task_idx as repetition index
    
  # Use unique log file for each parameter value (d)
  log_file <- file.path(output_dir, paste0("log_", d, "_", r, ".txt"))
                            
    sink(log_file, append = TRUE)
    cat(paste("Starting simulation for repetition =", r, "at", Sys.time(), "\n"))
    sink()
    start_sim <- Sys.time()  
                            
    #Define the number of years (iterations) you want to run the simulation
    years<-300
    #Maximum id
    #you record the maximum id so the id of the new individuals start after the existing one
    max_id <- max(it_data$id)
    #initial population
    it_indpop<-create_initialpop(popsize)
    #storing it_dataf
    it_dataf <- data.frame()
    
    #Run the simulation
    for (b in 1:years){
      if(nrow(it_indpop)==0){
        sink(log_file, append = TRUE)
        cat(paste("No individuals in it_indpop for year", b, "in repetition =", r, "at", Sys.time(), "\n"))
        sink()
        # Create empty it_data with consistent structure (for the current iteration)
        it_data <- data.frame(id = NA, prod_a = NA, mom_id = NA,
                              mom_surplus_a = NA, desc_need_a = NA,
                              out_degree = NA, in_degree = NA,
                              res_a = NA, repro = NA, lro = NA,
                              tlr = NA, stage = NA, surv = NA,
                              age = NA, store_a = NA, year = NA)
                                
        # Combine the previous data with the empty one
        it_dataf <- rbind(it_dataf, it_data)
                                
        break # skip the rest of the loop for this iteration
        } else{ #production
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
                                
            # Log the time at every 10 iterations
            if (b %% 10 == 0) {
              sink(log_file, append = TRUE)
              cat(paste("Year", b, "completed for repetition =", r, "at", Sys.time(), "\n"))
              cat(paste("Population size =", nrow(it_indpop), "in repetition =", r, "in year =", b, "\n"))
              sink()
            }
                                
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
                                
              # Log the population size when splitting happens
              if (nrow(it_indpop[it_indpop$stage == 2 | it_indpop$stage == 3 | it_indpop$stage == 4,]) > 5000) {
                sink(log_file, append = TRUE)
                cat(paste("Splitting population in year =", b, "in repetition =", r, "with non-juvenile population size =", nrow(it_indpop[it_indpop$stage == 2 | it_indpop$stage == 3 | it_indpop$stage == 4,]), "\n"))
                sink()  
              }
              
              #see who stochastically survives if n > 3000
              it_indpop <- population_thresh(it_indpop)
              
        }
      }
                            
      # Final logging after the simulation completes
      sink(log_file, append = TRUE)
      cat(paste("Completed simulation for repetition =", r, "at", Sys.time(), "\n"))
      sink()
                            
      end_sim <- Sys.time()
                            
      time_sim <- difftime(end_sim,start_sim,units=c("mins"))
                            
      sink(log_file, append = TRUE)
      cat(paste("Length of simulation for repetition =", r, "is", time_sim, "minutes", "\n"))
      sink()
                            
      # Save results to .fst file
      output_file <- file.path(output_dir, paste0("results_d", d, "_r", r, ".fst"))
      write_fst(it_dataf, output_file)
}
                          
}

# Stop the cluster after computation
stopCluster(my_cluster)

# #Save data ----
# 
# # Flatten the list into one large data frame
# flattened_data_s2 <- do.call(rbind, results_10_2)
# #save flattened data to a .fst file
# write.fst(flattened_data_s2,"./Scenario_2/raw_simulation_s2.fst")
# #get the row counts for later use
# row_counts_s2 <- sapply(results_10_2, nrow)
# #save the row counts
# saveRDS(row_counts_s2, "./Scenario_2/row_counts_s2.RData")
