# Life cycle variation and resource dynamics ABM: Scenario 3 - results_s3 ----

#Here is the code to get the summary statistics and plots from the simulation of Scenario 3, which aims to understand the influence of resource production on the variability of life cycles.

#Production: parameter sweep between 0.1 and 0.9 (17 values)
#Transfers: null
#Habitat quality: baseline

#R settings ----

#set work directory
getwd()
#setwd("./LCV_RD_ABM")

#install packages
#install.packages("scales")
library(scales)

#Scenario 3 ----

#Data import ----

#import raw results_s3 from the simulation
results_s3 <- readRDS("./Scenario_3/lht_list_s3.RData")

#get the parameter values for labeling

# Step 1: Create the blockmatrix with original probabilities
blockmatrix <- matrix(c(
  0.25,0.75,0.75,0.75, #juvenile column
  0.25,0.25,0.25,0.25, #adult column
  0.25,0.4,0.4,0.5, #reproductive career column
  0.25,0.5,0.5,0.4 #post-reproductive career column
),
nrow=4,ncol=4)

# Step 2: Initialize a list to store 19 matrices
matrices_list <- vector("list", 19)

# Step 3: Iterate over the elements of the blockmatrix
for (i in 1:nrow(blockmatrix)) { # rows
  for (j in 1:ncol(blockmatrix)) { # columns
    # Generate a sequence for each element
    sequence <- round(seq((blockmatrix[i, j] - 0.2), (blockmatrix[i, j] + 0.2), length = 19), 2)
    
    # Step 4: Assign the sequence values to the appropriate position in each of the 19 matrices
    for (k in 1:length(matrices_list)) {
      if (is.null(matrices_list[[k]])) {
        matrices_list[[k]] <- matrix(0, nrow = 4, ncol = 4) # Initialize the matrix
      }
      matrices_list[[k]][i, j] <- sequence[k]
    }
  }
}

# check the matrices
head(matrices_list)

#Summary statistics ----

summary_stats_s3 <- lapply(1:length(results_s3),function(x)list())
#match the original names with the sample
names(summary_stats_s3) <- names(results_s3)
names(summary_stats_s3)

#gather summary statistics of each result
for (i in 1:length(results_s3)){
  if(nrow(results_s3[[i]]) == 0){
    summary_stats_s3[[i]] <- summary_stats_s3[[i]]
  } else{
    summary_stats_s3[[i]]$summary <- summary(results_s3[[i]][,c("lng","lro","asm","afr","alr","meno")])
    summary_stats_s3[[i]]$sm <- as.data.frame(lapply(results_s3[[i]][,c("lng","lro","asm","afr","alr","meno")],sd,na.rm=T))
    summary_stats_s3[[i]]$mean <- as.data.frame(lapply(results_s3[[i]][,c("lng","lro","asm","afr","alr","meno")],mean,na.rm=T))
    summary_stats_s3[[i]]$cv <- summary_stats_s3[[i]]$sd / summary_stats_s3[[i]]$mean
    summary_stats_s3[[i]]$min <- as.data.frame(lapply(results_s3[[i]][,c("lng","lro","asm","afr","alr","meno")],min,na.rm=T))
    summary_stats_s3[[i]]$max <- as.data.frame(lapply(results_s3[[i]][,c("lng","lro","asm","afr","alr","meno")],max,na.rm=T))
    if(nrow(results_s3[[i]]) == 1 | summary_stats_s3[[i]]$sd$lng == 0 & summary_stats_s3[[i]]$sd$lro == 0){
      summary_stats_s3[[i]]$summary <- summary(results_s3[[i]][,c("lng","lro","asm","afr","alr","meno")])
      summary_stats_s3[[i]]$sm <- as.data.frame(lapply(results_s3[[i]][,c("lng","lro","asm","afr","alr","meno")],sd,na.rm=T))
      summary_stats_s3[[i]]$sd[,c("lng","lro")][which(is.na(summary_stats_s3[[i]]$sd[,c("lng","lro")]))] <- 0
      summary_stats_s3[[i]]$mean <- as.data.frame(lapply(results_s3[[i]][,c("lng","lro","asm","afr","alr","meno")],mean,na.rm=T))
      summary_stats_s3[[i]]$cv[,c("lng","lro")] <- 0  
      summary_stats_s3[[i]]$min <- as.data.frame(lapply(results_s3[[i]][,c("lng","lro","asm","afr","alr","meno")],min,na.rm=T))
      summary_stats_s3[[i]]$max <- as.data.frame(lapply(results_s3[[i]][,c("lng","lro","asm","afr","alr","meno")],max,na.rm=T))
    } 
  }
}

## Mean sorted by life history trait ----

#lng
#create data frame to gather means of lng
lng_mean_s3 <- as.data.frame(matrix(NA,19,11))
colnames(lng_mean_s3) <- c("m",1:10)
lng_mean_s3$m <- round(seq((blockmatrix[2, 4] - 0.2), (blockmatrix[2, 4] + 0.2), length = 19), 2)
#check it
lng_mean_s3
#extract the mean of lng across the parameter space
for(m in 1:nrow(lng_mean_s3)){
  for(r in 2:ncol(lng_mean_s3)){
    if(is.null(summary_stats_s3[grep(paste("m",m,"",sep="_"),names(summary_stats_s3))][[r-1]]$mean$lng) == T){
      lng_mean_s3[m,r] <- NA
    }else{
      lng_mean_s3[m,r] <- summary_stats_s3[grep(paste("m",m,"",sep="_"),names(summary_stats_s3))][[r-1]]$mean$lng
    }
  }
}
#check it
lng_mean_s3

#lro
#create data frame to gather means of lro
lro_mean_s3 <- as.data.frame(matrix(NA,19,11))
colnames(lro_mean_s3) <- c("m",1:10)
lro_mean_s3$m <- round(seq((blockmatrix[2, 4] - 0.2), (blockmatrix[2, 4] + 0.2), length = 19), 2)
#check it
lro_mean_s3
#extract the mean of lro across the parameter space
for(m in 1:nrow(lro_mean_s3)){
  for(r in 2:ncol(lro_mean_s3)){
    if(is.null(summary_stats_s3[grep(paste("m",m,"",sep="_"),names(summary_stats_s3))][[r-1]]$mean$lro) == T){
      lro_mean_s3[m,r] <- NA
    }else{
      lro_mean_s3[m,r] <- summary_stats_s3[grep(paste("m",m,"",sep="_"),names(summary_stats_s3))][[r-1]]$mean$lro
    }
  }
}
#check it
lro_mean_s3

#asm
#create data frame to gather means of asm
asm_mean_s3 <- as.data.frame(matrix(NA,19,11))
colnames(asm_mean_s3) <- c("m",1:10)
asm_mean_s3$m <- round(seq((blockmatrix[2, 4] - 0.2), (blockmatrix[2, 4] + 0.2), length = 19), 2)
#check it
asm_mean_s3
#extract the mean of asm across the parameter space
for(m in 1:nrow(asm_mean_s3)){
  for(r in 2:ncol(asm_mean_s3)){
    if(is.null(summary_stats_s3[grep(paste("m",m,"",sep="_"),names(summary_stats_s3))][[r-1]]$mean$asm) == T){
      asm_mean_s3[m,r] <- NA
    }else{
      asm_mean_s3[m,r] <- summary_stats_s3[grep(paste("m",m,"",sep="_"),names(summary_stats_s3))][[r-1]]$mean$asm
    }
  }
}
#check it
asm_mean_s3

#afr
#create data frame to gather means of afr
afr_mean_s3 <- as.data.frame(matrix(NA,19,11))
colnames(afr_mean_s3) <- c("m",1:10)
afr_mean_s3$m <- round(seq((blockmatrix[2, 4] - 0.2), (blockmatrix[2, 4] + 0.2), length = 19), 2)
#check it
afr_mean_s3
#extract the mean of afr across the parameter space
for(m in 1:nrow(afr_mean_s3)){
  for(r in 2:ncol(afr_mean_s3)){
    if(is.null(summary_stats_s3[grep(paste("m",m,"",sep="_"),names(summary_stats_s3))][[r-1]]$mean$afr) == T){
      afr_mean_s3[m,r] <- NA
    }else{
      afr_mean_s3[m,r] <- summary_stats_s3[grep(paste("m",m,"",sep="_"),names(summary_stats_s3))][[r-1]]$mean$afr
    }
  }
}
#check it
afr_mean_s3

#alr
#create data frame to gather means of alr
alr_mean_s3 <- as.data.frame(matrix(NA,19,11))
colnames(alr_mean_s3) <- c("m",1:10)
alr_mean_s3$m <- round(seq((blockmatrix[2, 4] - 0.2), (blockmatrix[2, 4] + 0.2), length = 19), 2)
#check it
alr_mean_s3
#extract the mean of alr across the parameter space
for(m in 1:nrow(alr_mean_s3)){
  for(r in 2:ncol(alr_mean_s3)){
    if(is.null(summary_stats_s3[grep(paste("m",m,"",sep="_"),names(summary_stats_s3))][[r-1]]$mean$alr) == T){
      alr_mean_s3[m,r] <- NA
    }else{
      alr_mean_s3[m,r] <- summary_stats_s3[grep(paste("m",m,"",sep="_"),names(summary_stats_s3))][[r-1]]$mean$alr
    }
  }
}
#check it
alr_mean_s3

#meno
#create data frame to gather means of meno
meno_mean_s3 <- as.data.frame(matrix(NA,19,11))
colnames(meno_mean_s3) <- c("m",1:10)
meno_mean_s3$m <- round(seq((blockmatrix[2, 4] - 0.2), (blockmatrix[2, 4] + 0.2), length = 19), 2)
#check it
meno_mean_s3
#extract the mean of meno across the parameter space
for(m in 1:nrow(meno_mean_s3)){
  for(r in 2:ncol(meno_mean_s3)){
    if(is.null(summary_stats_s3[grep(paste("m",m,"",sep="_"),names(summary_stats_s3))][[r-1]]$mean$meno) == T){
      meno_mean_s3[m,r] <- NA
    }else{
      meno_mean_s3[m,r] <- summary_stats_s3[grep(paste("m",m,"",sep="_"),names(summary_stats_s3))][[r-1]]$mean$meno
    }
  }
}
#check it
meno_mean_s3

## SD sorted by life history trait ----

#lng
#create data frame to gather sds of lng
lng_sd_s3 <- as.data.frame(matrix(NA,19,11))
colnames(lng_sd_s3) <- c("m",1:10)
lng_sd_s3$m <- round(seq((blockmatrix[2, 4] - 0.2), (blockmatrix[2, 4] + 0.2), length = 19), 2)
#check it
lng_sd_s3
#extract the sd of lng across the parameter space
for(m in 1:nrow(lng_sd_s3)){
  for(r in 2:ncol(lng_sd_s3)){
    if(is.null(summary_stats_s3[grep(paste("m",m,"",sep="_"),names(summary_stats_s3))][[r-1]]$sd$lng) == T){
      lng_sd_s3[m,r] <- NA
    }else{
      lng_sd_s3[m,r] <- summary_stats_s3[grep(paste("m",m,"",sep="_"),names(summary_stats_s3))][[r-1]]$sd$lng
    }
  }
}
#check it
lng_sd_s3

#lro
#create data frame to gather sds of lro
lro_sd_s3 <- as.data.frame(matrix(NA,19,11))
colnames(lro_sd_s3) <- c("m",1:10)
lro_sd_s3$m <- round(seq((blockmatrix[2, 4] - 0.2), (blockmatrix[2, 4] + 0.2), length = 19), 2)
#check it
lro_sd_s3
#extract the sd of lro across the parameter space
for(m in 1:nrow(lro_sd_s3)){
  for(r in 2:ncol(lro_sd_s3)){
    if(is.null(summary_stats_s3[grep(paste("m",m,"",sep="_"),names(summary_stats_s3))][[r-1]]$sd$lro) == T){
      lro_sd_s3[m,r] <- NA
    }else{
      lro_sd_s3[m,r] <- summary_stats_s3[grep(paste("m",m,"",sep="_"),names(summary_stats_s3))][[r-1]]$sd$lro
    }
  }
}
#check it
lro_sd_s3

#asm
#create data frame to gather sds of asm
asm_sd_s3 <- as.data.frame(matrix(NA,19,11))
colnames(asm_sd_s3) <- c("m",1:10)
asm_sd_s3$m <- round(seq((blockmatrix[2, 4] - 0.2), (blockmatrix[2, 4] + 0.2), length = 19), 2)
#check it
asm_sd_s3
#extract the sd of asm across the parameter space
for(m in 1:nrow(asm_sd_s3)){
  for(r in 2:ncol(asm_sd_s3)){
    if(is.null(summary_stats_s3[grep(paste("m",m,"",sep="_"),names(summary_stats_s3))][[r-1]]$sd$asm) == T){
      asm_sd_s3[m,r] <- NA
    }else{
      asm_sd_s3[m,r] <- summary_stats_s3[grep(paste("m",m,"",sep="_"),names(summary_stats_s3))][[r-1]]$sd$asm
    }
  }
}
#check it
asm_sd_s3

#afr
#create data frame to gather sds of afr
afr_sd_s3 <- as.data.frame(matrix(NA,19,11))
colnames(afr_sd_s3) <- c("m",1:10)
afr_sd_s3$m <- round(seq((blockmatrix[2, 4] - 0.2), (blockmatrix[2, 4] + 0.2), length = 19), 2)
#check it
afr_sd_s3
#extract the sd of afr across the parameter space
for(m in 1:nrow(afr_sd_s3)){
  for(r in 2:ncol(afr_sd_s3)){
    if(is.null(summary_stats_s3[grep(paste("m",m,"",sep="_"),names(summary_stats_s3))][[r-1]]$sd$afr) == T){
      afr_sd_s3[m,r] <- NA
    }else{
      afr_sd_s3[m,r] <- summary_stats_s3[grep(paste("m",m,"",sep="_"),names(summary_stats_s3))][[r-1]]$sd$afr
    }
  }
}
#check it
afr_sd_s3

#alr
#create data frame to gather sds of alr
alr_sd_s3 <- as.data.frame(matrix(NA,19,11))
colnames(alr_sd_s3) <- c("m",1:10)
alr_sd_s3$m <- round(seq((blockmatrix[2, 4] - 0.2), (blockmatrix[2, 4] + 0.2), length = 19), 2)
#check it
alr_sd_s3
#extract the sd of alr across the parameter space
for(m in 1:nrow(alr_sd_s3)){
  for(r in 2:ncol(alr_sd_s3)){
    if(is.null(summary_stats_s3[grep(paste("m",m,"",sep="_"),names(summary_stats_s3))][[r-1]]$sd$alr) == T){
      alr_sd_s3[m,r] <- NA
    }else{
      alr_sd_s3[m,r] <- summary_stats_s3[grep(paste("m",m,"",sep="_"),names(summary_stats_s3))][[r-1]]$sd$alr
    }
  }
}
#check it
alr_sd_s3

#meno
#create data frame to gather sds of meno
meno_sd_s3 <- as.data.frame(matrix(NA,19,11))
colnames(meno_sd_s3) <- c("m",1:10)
meno_sd_s3$m <- round(seq((blockmatrix[2, 4] - 0.2), (blockmatrix[2, 4] + 0.2), length = 19), 2)
#check it
meno_sd_s3
#extract the sd of meno across the parameter space
for(m in 1:nrow(meno_sd_s3)){
  for(r in 2:ncol(meno_sd_s3)){
    if(is.null(summary_stats_s3[grep(paste("m",m,"",sep="_"),names(summary_stats_s3))][[r-1]]$sd$meno) == T){
      meno_sd_s3[m,r] <- NA
    }else{
      meno_sd_s3[m,r] <- summary_stats_s3[grep(paste("m",m,"",sep="_"),names(summary_stats_s3))][[r-1]]$sd$meno
    }
  }
}
#check it
meno_sd_s3

## Min sorted by life history trait ----

#lng
#create data frame to gather mins of lng
lng_min_s3 <- as.data.frame(matrix(NA,19,11))
colnames(lng_min_s3) <- c("m",1:10)
lng_min_s3$m <- round(seq((blockmatrix[2, 4] - 0.2), (blockmatrix[2, 4] + 0.2), length = 19), 2)
#check it
lng_min_s3
#extract the min of lng across the parameter space
for(m in 1:nrow(lng_min_s3)){
  for(r in 2:ncol(lng_min_s3)){
    if(is.null(summary_stats_s3[grep(paste("m",m,"",sep="_"),names(summary_stats_s3))][[r-1]]$min$lng) == T ||
       is.infinite(summary_stats_s3[grep(paste("m",m,"",sep="_"),names(summary_stats_s3))][[r-1]]$min$lng) == T){
      lng_min_s3[m,r] <- NA
    }else{
      lng_min_s3[m,r] <- summary_stats_s3[grep(paste("m",m,"",sep="_"),names(summary_stats_s3))][[r-1]]$min$lng
    }
  }
}
#check it
lng_min_s3

#lro
#create data frame to gather mins of lro
lro_min_s3 <- as.data.frame(matrix(NA,19,11))
colnames(lro_min_s3) <- c("m",1:10)
lro_min_s3$m <- round(seq((blockmatrix[2, 4] - 0.2), (blockmatrix[2, 4] + 0.2), length = 19), 2)
#check it
lro_min_s3
#extract the min of lro across the parameter space
for(m in 1:nrow(lro_min_s3)){
  for(r in 2:ncol(lro_min_s3)){
    if(is.null(summary_stats_s3[grep(paste("m",m,"",sep="_"),names(summary_stats_s3))][[r-1]]$min$lro) == T ||
       is.infinite(summary_stats_s3[grep(paste("m",m,"",sep="_"),names(summary_stats_s3))][[r-1]]$min$lro) == T){
      lro_min_s3[m,r] <- NA
    }else{
      lro_min_s3[m,r] <- summary_stats_s3[grep(paste("m",m,"",sep="_"),names(summary_stats_s3))][[r-1]]$min$lro
    }
  }
}
#check it
lro_min_s3

#asm
#create data frame to gather mins of asm
asm_min_s3 <- as.data.frame(matrix(NA,19,11))
colnames(asm_min_s3) <- c("m",1:10)
asm_min_s3$m <- round(seq((blockmatrix[2, 4] - 0.2), (blockmatrix[2, 4] + 0.2), length = 19), 2)
#check it
asm_min_s3
#extract the min of asm across the parameter space
for(m in 1:nrow(asm_min_s3)){
  for(r in 2:ncol(asm_min_s3)){
    if(is.null(summary_stats_s3[grep(paste("m",m,"",sep="_"),names(summary_stats_s3))][[r-1]]$min$asm) == T ||
       is.infinite(summary_stats_s3[grep(paste("m",m,"",sep="_"),names(summary_stats_s3))][[r-1]]$min$asm) == T){
      asm_min_s3[m,r] <- NA
    }else{
      asm_min_s3[m,r] <- summary_stats_s3[grep(paste("m",m,"",sep="_"),names(summary_stats_s3))][[r-1]]$min$asm
    }
  }
}
#check it
asm_min_s3

#afr
#create data frame to gather mins of afr
afr_min_s3 <- as.data.frame(matrix(NA,19,11))
colnames(afr_min_s3) <- c("m",1:10)
afr_min_s3$m <- round(seq((blockmatrix[2, 4] - 0.2), (blockmatrix[2, 4] + 0.2), length = 19), 2)
#check it
afr_min_s3
#extract the min of afr across the parameter space
for(m in 1:nrow(afr_min_s3)){
  for(r in 2:ncol(afr_min_s3)){
    if(is.null(summary_stats_s3[grep(paste("m",m,"",sep="_"),names(summary_stats_s3))][[r-1]]$min$afr) == T ||
       is.infinite(summary_stats_s3[grep(paste("m",m,"",sep="_"),names(summary_stats_s3))][[r-1]]$min$afr) == T){
      afr_min_s3[m,r] <- NA
    }else{
      afr_min_s3[m,r] <- summary_stats_s3[grep(paste("m",m,"",sep="_"),names(summary_stats_s3))][[r-1]]$min$afr
    }
  }
}
#check it
afr_min_s3

#alr
#create data frame to gather mins of alr
alr_min_s3 <- as.data.frame(matrix(NA,19,11))
colnames(alr_min_s3) <- c("m",1:10)
alr_min_s3$m <- round(seq((blockmatrix[2, 4] - 0.2), (blockmatrix[2, 4] + 0.2), length = 19), 2)
#check it
alr_min_s3
#extract the min of alr across the parameter space
for(m in 1:nrow(alr_min_s3)){
  for(r in 2:ncol(alr_min_s3)){
    if(is.null(summary_stats_s3[grep(paste("m",m,"",sep="_"),names(summary_stats_s3))][[r-1]]$min$alr) == T ||
       is.infinite(summary_stats_s3[grep(paste("m",m,"",sep="_"),names(summary_stats_s3))][[r-1]]$min$alr) == T){
      alr_min_s3[m,r] <- NA
    }else{
      alr_min_s3[m,r] <- summary_stats_s3[grep(paste("m",m,"",sep="_"),names(summary_stats_s3))][[r-1]]$min$alr
    }
  }
}
#check it
alr_min_s3

#meno
#create data frame to gather mins of meno
meno_min_s3 <- as.data.frame(matrix(NA,19,11))
colnames(meno_min_s3) <- c("m",1:10)
meno_min_s3$m <- round(seq((blockmatrix[2, 4] - 0.2), (blockmatrix[2, 4] + 0.2), length = 19), 2)
#check it
meno_min_s3
#extract the min of meno across the parameter space
for(m in 1:nrow(meno_min_s3)){
  for(r in 2:ncol(meno_min_s3)){
    if(is.null(summary_stats_s3[grep(paste("m",m,"",sep="_"),names(summary_stats_s3))][[r-1]]$min$meno) == T ||
       is.infinite(summary_stats_s3[grep(paste("m",m,"",sep="_"),names(summary_stats_s3))][[r-1]]$min$meno) == T){
      meno_min_s3[m,r] <- NA
    }else{
      meno_min_s3[m,r] <- summary_stats_s3[grep(paste("m",m,"",sep="_"),names(summary_stats_s3))][[r-1]]$min$meno
    }
  }
}
#check it
meno_min_s3

## Max sorted by life history trait ----

#lng
#create data frame to gather maxs of lng
lng_max_s3 <- as.data.frame(matrix(NA,19,11))
colnames(lng_max_s3) <- c("m",1:10)
lng_max_s3$m <- round(seq((blockmatrix[2, 4] - 0.2), (blockmatrix[2, 4] + 0.2), length = 19), 2)
#check it
lng_max_s3
#extract the max of lng across the parameter space
for(m in 1:nrow(lng_max_s3)){
  for(r in 2:ncol(lng_max_s3)){
    if(is.null(summary_stats_s3[grep(paste("m",m,"",sep="_"),names(summary_stats_s3))][[r-1]]$max$lng) == T ||
       is.infinite(summary_stats_s3[grep(paste("m",m,"",sep="_"),names(summary_stats_s3))][[r-1]]$max$lng) == T){
      lng_max_s3[m,r] <- NA
    }else{
      lng_max_s3[m,r] <- summary_stats_s3[grep(paste("m",m,"",sep="_"),names(summary_stats_s3))][[r-1]]$max$lng
    }
  }
}
#check it
lng_max_s3

#lro
#create data frame to gather maxs of lro
lro_max_s3 <- as.data.frame(matrix(NA,19,11))
colnames(lro_max_s3) <- c("m",1:10)
lro_max_s3$m <- round(seq((blockmatrix[2, 4] - 0.2), (blockmatrix[2, 4] + 0.2), length = 19), 2)
#check it
lro_max_s3
#extract the max of lro across the parameter space
for(m in 1:nrow(lro_max_s3)){
  for(r in 2:ncol(lro_max_s3)){
    if(is.null(summary_stats_s3[grep(paste("m",m,"",sep="_"),names(summary_stats_s3))][[r-1]]$max$lro) == T ||
       is.infinite(summary_stats_s3[grep(paste("m",m,"",sep="_"),names(summary_stats_s3))][[r-1]]$max$lro) == T){
      lro_max_s3[m,r] <- NA
    }else{
      lro_max_s3[m,r] <- summary_stats_s3[grep(paste("m",m,"",sep="_"),names(summary_stats_s3))][[r-1]]$max$lro
    }
  }
}
#check it
lro_max_s3

#asm
#create data frame to gather maxs of asm
asm_max_s3 <- as.data.frame(matrix(NA,19,11))
colnames(asm_max_s3) <- c("m",1:10)
asm_max_s3$m <- round(seq((blockmatrix[2, 4] - 0.2), (blockmatrix[2, 4] + 0.2), length = 19), 2)
#check it
asm_max_s3
#extract the max of asm across the parameter space
for(m in 1:nrow(asm_max_s3)){
  for(r in 2:ncol(asm_max_s3)){
    if(is.null(summary_stats_s3[grep(paste("m",m,"",sep="_"),names(summary_stats_s3))][[r-1]]$max$asm) == T ||
       is.infinite(summary_stats_s3[grep(paste("m",m,"",sep="_"),names(summary_stats_s3))][[r-1]]$max$asm) == T){
      asm_max_s3[m,r] <- NA
    }else{
      asm_max_s3[m,r] <- summary_stats_s3[grep(paste("m",m,"",sep="_"),names(summary_stats_s3))][[r-1]]$max$asm
    }
  }
}
#check it
asm_max_s3

#afr
#create data frame to gather maxs of afr
afr_max_s3 <- as.data.frame(matrix(NA,19,11))
colnames(afr_max_s3) <- c("m",1:10)
afr_max_s3$m <- round(seq((blockmatrix[2, 4] - 0.2), (blockmatrix[2, 4] + 0.2), length = 19), 2)
#check it
afr_max_s3
#extract the max of afr across the parameter space
for(m in 1:nrow(afr_max_s3)){
  for(r in 2:ncol(afr_max_s3)){
    if(is.null(summary_stats_s3[grep(paste("m",m,"",sep="_"),names(summary_stats_s3))][[r-1]]$max$afr) == T ||
       is.infinite(summary_stats_s3[grep(paste("m",m,"",sep="_"),names(summary_stats_s3))][[r-1]]$max$afr) == T){
      afr_max_s3[m,r] <- NA
    }else{
      afr_max_s3[m,r] <- summary_stats_s3[grep(paste("m",m,"",sep="_"),names(summary_stats_s3))][[r-1]]$max$afr
    }
  }
}
#check it
afr_max_s3

#alr
#create data frame to gather maxs of alr
alr_max_s3 <- as.data.frame(matrix(NA,19,11))
colnames(alr_max_s3) <- c("m",1:10)
alr_max_s3$m <- round(seq((blockmatrix[2, 4] - 0.2), (blockmatrix[2, 4] + 0.2), length = 19), 2)
#check it
alr_max_s3
#extract the max of alr across the parameter space
for(m in 1:nrow(alr_max_s3)){
  for(r in 2:ncol(alr_max_s3)){
    if(is.null(summary_stats_s3[grep(paste("m",m,"",sep="_"),names(summary_stats_s3))][[r-1]]$max$alr) == T ||
       is.infinite(summary_stats_s3[grep(paste("m",m,"",sep="_"),names(summary_stats_s3))][[r-1]]$max$alr) == T){
      alr_max_s3[m,r] <- NA
    }else{
      alr_max_s3[m,r] <- summary_stats_s3[grep(paste("m",m,"",sep="_"),names(summary_stats_s3))][[r-1]]$max$alr
    }
  }
}
#check it
alr_max_s3

#meno
#create data frame to gather maxs of meno
meno_max_s3 <- as.data.frame(matrix(NA,19,11))
colnames(meno_max_s3) <- c("m",1:10)
meno_max_s3$m <- round(seq((blockmatrix[2, 4] - 0.2), (blockmatrix[2, 4] + 0.2), length = 19), 2)
#check it
meno_max_s3
#extract the max of meno across the parameter space
for(m in 1:nrow(meno_max_s3)){
  for(r in 2:ncol(meno_max_s3)){
    if(is.null(summary_stats_s3[grep(paste("m",m,"",sep="_"),names(summary_stats_s3))][[r-1]]$max$meno) == T ||
       is.infinite(summary_stats_s3[grep(paste("m",m,"",sep="_"),names(summary_stats_s3))][[r-1]]$max$meno) == T){
      meno_max_s3[m,r] <- NA
    }else{
      meno_max_s3[m,r] <- summary_stats_s3[grep(paste("m",m,"",sep="_"),names(summary_stats_s3))][[r-1]]$max$meno
    }
  }
}
#check it
meno_max_s3

## CV sorted by life history trait ----

#lng
#create data frame to gather cvs of lng
lng_cv_s3 <- as.data.frame(matrix(NA,19,11))
colnames(lng_cv_s3) <- c("m",1:10)
lng_cv_s3$m <- round(seq((blockmatrix[2, 4] - 0.2), (blockmatrix[2, 4] + 0.2), length = 19), 2)
#check it
lng_cv_s3
#extract the cv of lng across the parameter space
for(m in 1:nrow(lng_cv_s3)){
  for(r in 2:ncol(lng_cv_s3)){
    if(is.null(summary_stats_s3[grep(paste("m",m,"",sep="_"),names(summary_stats_s3))][[r-1]]$cv$lng) == T ||
       is.infinite(summary_stats_s3[grep(paste("m",m,"",sep="_"),names(summary_stats_s3))][[r-1]]$cv$lng) == T){
      lng_cv_s3[m,r] <- NA
    }else{
      lng_cv_s3[m,r] <- summary_stats_s3[grep(paste("m",m,"",sep="_"),names(summary_stats_s3))][[r-1]]$cv$lng
    }
  }
}
#check it
lng_cv_s3

#lro
#create data frame to gather cvs of lro
lro_cv_s3 <- as.data.frame(matrix(NA,19,11))
colnames(lro_cv_s3) <- c("m",1:10)
lro_cv_s3$m <- round(seq((blockmatrix[2, 4] - 0.2), (blockmatrix[2, 4] + 0.2), length = 19), 2)
#check it
lro_cv_s3
#extract the cv of lro across the parameter space
for(m in 1:nrow(lro_cv_s3)){
  for(r in 2:ncol(lro_cv_s3)){
    if(is.null(summary_stats_s3[grep(paste("m",m,"",sep="_"),names(summary_stats_s3))][[r-1]]$cv$lro) == T ||
       is.infinite(summary_stats_s3[grep(paste("m",m,"",sep="_"),names(summary_stats_s3))][[r-1]]$cv$lro) == T){
      lro_cv_s3[m,r] <- NA
    }else{
      lro_cv_s3[m,r] <- summary_stats_s3[grep(paste("m",m,"",sep="_"),names(summary_stats_s3))][[r-1]]$cv$lro
    }
  }
}
#check it
lro_cv_s3

#asm
#create data frame to gather cvs of asm
asm_cv_s3 <- as.data.frame(matrix(NA,19,11))
colnames(asm_cv_s3) <- c("m",1:10)
asm_cv_s3$m <- round(seq((blockmatrix[2, 4] - 0.2), (blockmatrix[2, 4] + 0.2), length = 19), 2)
#check it
asm_cv_s3
#extract the cv of asm across the parameter space
for(m in 1:nrow(asm_cv_s3)){
  for(r in 2:ncol(asm_cv_s3)){
    if(is.null(summary_stats_s3[grep(paste("m",m,"",sep="_"),names(summary_stats_s3))][[r-1]]$cv$asm) == T ||
       is.infinite(summary_stats_s3[grep(paste("m",m,"",sep="_"),names(summary_stats_s3))][[r-1]]$cv$asm) == T){
      asm_cv_s3[m,r] <- NA
    }else{
      asm_cv_s3[m,r] <- summary_stats_s3[grep(paste("m",m,"",sep="_"),names(summary_stats_s3))][[r-1]]$cv$asm
    }
  }
}
#check it
asm_cv_s3

#afr
#create data frame to gather cvs of afr
afr_cv_s3 <- as.data.frame(matrix(NA,19,11))
colnames(afr_cv_s3) <- c("m",1:10)
afr_cv_s3$m <- round(seq((blockmatrix[2, 4] - 0.2), (blockmatrix[2, 4] + 0.2), length = 19), 2)
#check it
afr_cv_s3
#extract the cv of afr across the parameter space
for(m in 1:nrow(afr_cv_s3)){
  for(r in 2:ncol(afr_cv_s3)){
    if(is.null(summary_stats_s3[grep(paste("m",m,"",sep="_"),names(summary_stats_s3))][[r-1]]$cv$afr) == T ||
       is.infinite(summary_stats_s3[grep(paste("m",m,"",sep="_"),names(summary_stats_s3))][[r-1]]$cv$afr) == T){
      afr_cv_s3[m,r] <- NA
    }else{
      afr_cv_s3[m,r] <- summary_stats_s3[grep(paste("m",m,"",sep="_"),names(summary_stats_s3))][[r-1]]$cv$afr
    }
  }
}
#check it
afr_cv_s3

#alr
#create data frame to gather cvs of alr
alr_cv_s3 <- as.data.frame(matrix(NA,19,11))
colnames(alr_cv_s3) <- c("m",1:10)
alr_cv_s3$m <- round(seq((blockmatrix[2, 4] - 0.2), (blockmatrix[2, 4] + 0.2), length = 19), 2)
#check it
alr_cv_s3
#extract the cv of alr across the parameter space
for(m in 1:nrow(alr_cv_s3)){
  for(r in 2:ncol(alr_cv_s3)){
    if(is.null(summary_stats_s3[grep(paste("m",m,"",sep="_"),names(summary_stats_s3))][[r-1]]$cv$alr) == T ||
       is.infinite(summary_stats_s3[grep(paste("m",m,"",sep="_"),names(summary_stats_s3))][[r-1]]$cv$alr) == T){
      alr_cv_s3[m,r] <- NA
    }else{
      alr_cv_s3[m,r] <- summary_stats_s3[grep(paste("m",m,"",sep="_"),names(summary_stats_s3))][[r-1]]$cv$alr
    }
  }
}
#check it
alr_cv_s3

#meno
#create data frame to gather cvs of meno
meno_cv_s3 <- as.data.frame(matrix(NA,19,11))
colnames(meno_cv_s3) <- c("m",1:10)
meno_cv_s3$m <- round(seq((blockmatrix[2, 4] - 0.2), (blockmatrix[2, 4] + 0.2), length = 19), 2)
#check it
meno_cv_s3
#extract the cv of meno across the parameter space
for(m in 1:nrow(meno_cv_s3)){
  for(r in 2:ncol(meno_cv_s3)){
    if(is.null(summary_stats_s3[grep(paste("m",m,"",sep="_"),names(summary_stats_s3))][[r-1]]$cv$meno) == T ||
       is.infinite(summary_stats_s3[grep(paste("m",m,"",sep="_"),names(summary_stats_s3))][[r-1]]$cv$meno) == T){
      meno_cv_s3[m,r] <- NA
    }else{
      meno_cv_s3[m,r] <- summary_stats_s3[grep(paste("m",m,"",sep="_"),names(summary_stats_s3))][[r-1]]$cv$meno
    }
  }
}
#check it
meno_cv_s3

##Average life cycle ----

###Longevity ----

#m = 0.5
apply(lng_mean_s3[2:11],1,mean,na.rm=T)[which(lng_mean_s3$m == 0.5)]
apply(lng_sd_s3[2:11],1,mean,na.rm=T)[which(lng_mean_s3$m == 0.5)]
apply(lng_min_s3[2:11],1,mean,na.rm=T)[which(lng_mean_s3$m == 0.5)]
apply(lng_max_s3[2:11],1,mean,na.rm=T)[which(lng_mean_s3$m == 0.5)]
#m = 0.7
apply(lng_mean_s3[2:11],1,mean,na.rm=T)[which(lng_mean_s3$m == 0.7)]
apply(lng_sd_s3[2:11],1,mean,na.rm=T)[which(lng_mean_s3$m == 0.7)]
apply(lng_min_s3[2:11],1,mean,na.rm=T)[which(lng_mean_s3$m == 0.7)]
apply(lng_max_s3[2:11],1,mean,na.rm=T)[which(lng_mean_s3$m == 0.7)]
#m = 0.3
apply(lng_mean_s3[2:11],1,mean,na.rm=T)[which(lng_mean_s3$m == 0.3)]
apply(lng_sd_s3[2:11],1,mean,na.rm=T)[which(lng_mean_s3$m == 0.3)]
apply(lng_min_s3[2:11],1,mean,na.rm=T)[which(lng_mean_s3$m == 0.3)]
apply(lng_max_s3[2:11],1,mean,na.rm=T)[which(lng_mean_s3$m == 0.3)]

###Lifetime reproductive output ----

#m = 0.5
apply(lro_mean_s3[2:11],1,mean,na.rm=T)[which(lro_mean_s3$m == 0.5)]
apply(lro_sd_s3[2:11],1,mean,na.rm=T)[which(lro_mean_s3$m == 0.5)]
apply(lro_min_s3[2:11],1,mean,na.rm=T)[which(lro_mean_s3$m == 0.5)]
apply(lro_max_s3[2:11],1,mean,na.rm=T)[which(lro_mean_s3$m == 0.5)]
#m = 0.7
apply(lro_mean_s3[2:11],1,mean,na.rm=T)[which(lro_mean_s3$m == 0.7)]
apply(lro_sd_s3[2:11],1,mean,na.rm=T)[which(lro_mean_s3$m == 0.7)]
apply(lro_min_s3[2:11],1,mean,na.rm=T)[which(lro_mean_s3$m == 0.7)]
apply(lro_max_s3[2:11],1,mean,na.rm=T)[which(lro_mean_s3$m == 0.7)]
#m = 0.3
apply(lro_mean_s3[2:11],1,mean,na.rm=T)[which(lro_mean_s3$m == 0.3)]
apply(lro_sd_s3[2:11],1,mean,na.rm=T)[which(lro_mean_s3$m == 0.3)]
apply(lro_min_s3[2:11],1,mean,na.rm=T)[which(lro_mean_s3$m == 0.3)]
apply(lro_max_s3[2:11],1,mean,na.rm=T)[which(lro_mean_s3$m == 0.3)]

###Age at sexual maturity ----

#m = 0.5
apply(asm_mean_s3[2:11],1,mean,na.rm=T)[which(asm_mean_s3$m == 0.5)]
apply(asm_sd_s3[2:11],1,mean,na.rm=T)[which(asm_mean_s3$m == 0.5)]
apply(asm_min_s3[2:11],1,mean,na.rm=T)[which(asm_mean_s3$m == 0.5)]
apply(asm_max_s3[2:11],1,mean,na.rm=T)[which(asm_mean_s3$m == 0.5)]
#m = 0.7
apply(asm_mean_s3[2:11],1,mean,na.rm=T)[which(asm_mean_s3$m == 0.7)]
apply(asm_sd_s3[2:11],1,mean,na.rm=T)[which(asm_mean_s3$m == 0.7)]
apply(asm_min_s3[2:11],1,mean,na.rm=T)[which(asm_mean_s3$m == 0.7)]
apply(asm_max_s3[2:11],1,mean,na.rm=T)[which(asm_mean_s3$m == 0.7)]
#m = 0.3
apply(asm_mean_s3[2:11],1,mean,na.rm=T)[which(asm_mean_s3$m == 0.3)]
apply(asm_sd_s3[2:11],1,mean,na.rm=T)[which(asm_mean_s3$m == 0.3)]
apply(asm_min_s3[2:11],1,mean,na.rm=T)[which(asm_mean_s3$m == 0.3)]
apply(asm_max_s3[2:11],1,mean,na.rm=T)[which(asm_mean_s3$m == 0.3)]

###Age at first reproduction ----

#m = 0.5
apply(afr_mean_s3[2:11],1,mean,na.rm=T)[which(afr_mean_s3$m == 0.5)]
apply(afr_sd_s3[2:11],1,mean,na.rm=T)[which(afr_mean_s3$m == 0.5)]
apply(afr_min_s3[2:11],1,mean,na.rm=T)[which(afr_mean_s3$m == 0.5)]
apply(afr_max_s3[2:11],1,mean,na.rm=T)[which(afr_mean_s3$m == 0.5)]
#m = 0.7
apply(afr_mean_s3[2:11],1,mean,na.rm=T)[which(afr_mean_s3$m == 0.7)]
apply(afr_sd_s3[2:11],1,mean,na.rm=T)[which(afr_mean_s3$m == 0.7)]
apply(afr_min_s3[2:11],1,mean,na.rm=T)[which(afr_mean_s3$m == 0.7)]
apply(afr_max_s3[2:11],1,mean,na.rm=T)[which(afr_mean_s3$m == 0.7)]
#m = 0.3
apply(afr_mean_s3[2:11],1,mean,na.rm=T)[which(afr_mean_s3$m == 0.3)]
apply(afr_sd_s3[2:11],1,mean,na.rm=T)[which(afr_mean_s3$m == 0.3)]
apply(afr_min_s3[2:11],1,mean,na.rm=T)[which(afr_mean_s3$m == 0.3)]
apply(afr_max_s3[2:11],1,mean,na.rm=T)[which(afr_mean_s3$m == 0.3)]

###Age at last reproduction ----

#m = 0.5
apply(alr_mean_s3[2:11],1,mean,na.rm=T)[which(alr_mean_s3$m == 0.5)]
apply(alr_sd_s3[2:11],1,mean,na.rm=T)[which(alr_mean_s3$m == 0.5)]
apply(alr_min_s3[2:11],1,mean,na.rm=T)[which(alr_mean_s3$m == 0.5)]
apply(alr_max_s3[2:11],1,mean,na.rm=T)[which(alr_mean_s3$m == 0.5)]
#m = 0.7
apply(alr_mean_s3[2:11],1,mean,na.rm=T)[which(alr_mean_s3$m == 0.7)]
apply(alr_sd_s3[2:11],1,mean,na.rm=T)[which(alr_mean_s3$m == 0.7)]
apply(alr_min_s3[2:11],1,mean,na.rm=T)[which(alr_mean_s3$m == 0.7)]
apply(alr_max_s3[2:11],1,mean,na.rm=T)[which(alr_mean_s3$m == 0.7)]
#m = 0.3
apply(alr_mean_s3[2:11],1,mean,na.rm=T)[which(alr_mean_s3$m == 0.3)]
apply(alr_sd_s3[2:11],1,mean,na.rm=T)[which(alr_mean_s3$m == 0.3)]
apply(alr_min_s3[2:11],1,mean,na.rm=T)[which(alr_mean_s3$m == 0.3)]
apply(alr_max_s3[2:11],1,mean,na.rm=T)[which(alr_mean_s3$m == 0.3)]

###Age at menopause ----

#m = 0.5
apply(meno_mean_s3[2:11],1,mean,na.rm=T)[which(meno_mean_s3$m == 0.5)]
apply(meno_sd_s3[2:11],1,mean,na.rm=T)[which(meno_mean_s3$m == 0.5)]
apply(meno_min_s3[2:11],1,mean,na.rm=T)[which(meno_mean_s3$m == 0.5)]
apply(meno_max_s3[2:11],1,mean,na.rm=T)[which(meno_mean_s3$m == 0.5)]
#m = 0.7
apply(meno_mean_s3[2:11],1,mean,na.rm=T)[which(meno_mean_s3$m == 0.7)]
apply(meno_sd_s3[2:11],1,mean,na.rm=T)[which(meno_mean_s3$m == 0.7)]
apply(meno_min_s3[2:11],1,mean,na.rm=T)[which(meno_mean_s3$m == 0.7)]
apply(meno_max_s3[2:11],1,mean,na.rm=T)[which(meno_mean_s3$m == 0.7)]
#m = 0.3
apply(meno_mean_s3[2:11],1,mean,na.rm=T)[which(meno_mean_s3$m == 0.3)]
apply(meno_sd_s3[2:11],1,mean,na.rm=T)[which(meno_mean_s3$m == 0.3)]
apply(meno_min_s3[2:11],1,mean,na.rm=T)[which(meno_mean_s3$m == 0.3)]
apply(meno_max_s3[2:11],1,mean,na.rm=T)[which(meno_mean_s3$m == 0.3)]

## Mean CV per life history trait ----

#create data frame
trans_prob <- round(seq((blockmatrix[2, 4] - 0.2), (blockmatrix[2, 4] + 0.2), length = 19), 2)
lht_mean_s3 <- as.data.frame(matrix(NA,length(trans_prob),7))
colnames(lht_mean_s3) <- c("m","lng","lro","asm","afr","alr","meno")
lht_mean_s3$m <- trans_prob
lht_mean_s3
#calculate the mean CV per life history trait
for(i in 1:nrow(lht_mean_s3)){
  lht_mean_s3[i,"lng"] <- ifelse(sum(is.na(lng_cv_s3[i,2:ncol(lng_cv_s3)]))==10,NA,mean(as.numeric(lng_cv_s3[i,2:ncol(lng_cv_s3)]),na.rm=T))
  lht_mean_s3[i,"lro"] <- ifelse(sum(is.na(lro_cv_s3[i,2:ncol(lro_cv_s3)]))==10,NA,mean(as.numeric(lro_cv_s3[i,2:ncol(lro_cv_s3)]),na.rm=T))
  lht_mean_s3[i,"asm"] <- ifelse(sum(is.na(asm_cv_s3[i,2:ncol(asm_cv_s3)]))==10,NA,mean(as.numeric(asm_cv_s3[i,2:ncol(asm_cv_s3)]),na.rm=T))
  lht_mean_s3[i,"afr"] <- ifelse(sum(is.na(afr_cv_s3[i,2:ncol(afr_cv_s3)]))==10,NA,mean(as.numeric(afr_cv_s3[i,2:ncol(afr_cv_s3)]),na.rm=T))
  lht_mean_s3[i,"alr"] <- ifelse(sum(is.na(alr_cv_s3[i,2:ncol(alr_cv_s3)]))==10,NA,mean(as.numeric(alr_cv_s3[i,2:ncol(alr_cv_s3)]),na.rm=T))
  lht_mean_s3[i,"meno"] <- ifelse(sum(is.na(meno_cv_s3[i,2:ncol(meno_cv_s3)]))==10,NA,mean(as.numeric(meno_cv_s3[i,2:ncol(meno_cv_s3)]),na.rm=T))
}
lht_mean_s3


#Plot it! ----

##Median all together ----

#get the maximum CV of all the life history traits for the scale in the plots
#create empty vector
cv_all <- NA
#select the CV of all life history traits
for(i in 1:length(summary_stats_s3)){
  if(is.null(summary_stats_s3[[i]])==T){
    cv_all <- c(cv_all,NA)
  }else{
    cv_all <- c(cv_all,summary_stats_s3[[i]]$cv)
  }
  cv_all <- as.numeric(cv_all)
  cv_all <- cv_all[!is.na(as.numeric(cv_all))]
}
#get the maximum
max(cv_all)

#plot the CV together
par(mar=c(5, 4, 4, 2))
layout(matrix(c(1,1,2,2,3,4,5,6),ncol=4,byrow=T))

### Longevity ----

plot(c(0,max(cv_all)+0.5)~c(0,1),
     main="Longevity",
     xlab="Transfer probability",
     ylab="CV longevity",
     ylim=c(0,max(cv_all)),
     type="n")
#Scenario 3
for(m in 1:nrow(lng_cv_s3)){
  points(rep(lng_cv_s3[m,1],10),
         as.numeric(lng_cv_s3[m,2:11]),
         pch=16,
         col=alpha("black",0.1)
  )    
}
points(lht_mean_s3$m,
       lht_mean_s3$lng,
       pch=16,cex=1,col="black")

### Lifetime reproductive output ----

plot(c(0,max(cv_all)+0.5)~c(0,1),
     main="Lifetime\nreproductive output",
     xlab="Transfer probability",
     ylab="CV lifetime reproductive output",
     ylim=c(0,max(cv_all)),
     type="n")
#Scenario 3
for(m in 1:nrow(lro_cv_s3)){
  points(rep(lro_cv_s3[m,1],10),
         as.numeric(lro_cv_s3[m,2:11]),
         pch=16,
         col=alpha("black",0.1)
  )    
}
points(lht_mean_s3$m,
       lht_mean_s3$lro,
       pch=16,cex=1,col="black")

### Age at sexual maturity ----

plot(c(0,max(cv_all)+0.5)~c(0,1),
     main="Age at\nsexual maturity",
     xlab="Transfer probability",
     ylab="CV age at sexual maturity",
     ylim=c(0,max(cv_all)),
     type="n")
#Scenario 3
for(m in 1:nrow(asm_cv_s3)){
  points(rep(asm_cv_s3[m,1],10),
         as.numeric(asm_cv_s3[m,2:11]),
         pch=16,
         col=alpha("black",0.1)
  )    
}
points(lht_mean_s3$m,
       lht_mean_s3$asm,
       pch=16,cex=1,col="black")

### Age at first reproduction ----

plot(c(0,max(cv_all)+0.5)~c(0,1),
     main="Age at\nfirst reproduction",
     xlab="Transfer probability",
     ylab="CV age at first reproduction",
     ylim=c(0,max(cv_all)),
     type="n")
#Scenario 3
for(m in 1:nrow(afr_cv_s3)){
  points(rep(afr_cv_s3[m,1],10),
         as.numeric(afr_cv_s3[m,2:11]),
         pch=16,
         col=alpha("black",0.1)
  )    
}
points(lht_mean_s3$m,
       lht_mean_s3$afr,
       pch=16,cex=1,col="black")

### Age at last reproduction ----

plot(c(0,max(cv_all)+0.5)~c(0,1),
     main="Age at\nlast reproduction",
     xlab="Transfer probability",
     ylab="CV age at last reproduction",
     ylim=c(0,max(cv_all)),
     type="n")
#Scenario 3
for(m in 1:nrow(alr_cv_s3)){
  points(rep(alr_cv_s3[m,1],10),
         as.numeric(alr_cv_s3[m,2:11]),
         pch=16,
         col=alpha("black",0.1)
  )    
}
points(lht_mean_s3$m,
       lht_mean_s3$alr,
       pch=16,cex=1,col="black")

### Age at menopause ----

plot(c(0,max(cv_all)+0.5)~c(0,1),
     main="Age at\nmenopause",
     xlab="Transfer probability",
     ylab="CV age at menopause",
     ylim=c(0,max(cv_all)),
     type="n")
#Scenario 3
for(m in 1:nrow(meno_cv_s3)){
  points(rep(meno_cv_s3[m,1],10),
         as.numeric(meno_cv_s3[m,2:11]),
         pch=16,
         col=alpha("black",0.1)
  )    
}
points(lht_mean_s3$m,
       lht_mean_s3$meno,
       pch=16,cex=1,col="black")

##Distribution of life history traits ----

###Longevity ----

#get the frequency of each age across parameter values and repetitions
for (m in 1:19) {
  # create a data frame for each parameter value
  assign(paste("m", m, sep = "_"), data.frame(matrix(0, nrow = 0, ncol = 101)))
  # retrieve the data frame
  temp_df <- get(paste("m", m, sep = "_"))
  # modify the column names
  colnames(temp_df) <- as.character(0:100)
  # reassign the modified data frame in the original data frame
  assign(paste("m", m, sep = "_"), temp_df)
  #loop through repetitions
  for (r in 1:10) {
    #extract the longevity
    lng_vector <- results_s3[grep(paste("m", m, "", sep = "_"), names(results_s3))][[r]]$lng
    #create a new row with zeros
    new_row <- rep(0, 101)
    #update the new row based on longevity values
    for (val in lng_vector) {
      if (val >= 0 & val <= 100) {
        new_row[val + 1] <- new_row[val + 1] + 1  # +1 to account for index starting at 1 in R
      }
    }
    #change column names to match
    new_row_df <- as.data.frame(t(new_row))
    colnames(new_row_df) <- as.character(0:100)
    #add new row into the data frame
    temp_df <- get(paste("m", m, sep = "_"))  #current data frame
    temp_df <- rbind(temp_df, new_row_df)  #new row
    assign(paste("m", m, sep = "_"), temp_df)  #updated data frame
  }
}

#### Plot the densities ----

# Step 1: Create a vector to store indices of data frames with positive longevity
valid_indices <- c()

# Step 2: First pass through to identify valid data frames
for(m in 1:19) {
  # Check if the sum of means is greater than zero
  if (sum(apply(get(paste("m", m, sep = "_")), 2, mean)) > 0) {
    valid_indices <- c(valid_indices, m)  # Store the valid index
  }
}

# Step 3: Create a color palette for only the valid data frames
color_palette <- hcl.colors(length(valid_indices), palette = "zissou 1", alpha = 0.5)

# Step 4: Prepare an empty plot

par(mfrow=c(1,1), mar=c(5, 4, 4, 8))

plot(c(0,100), c(0,15),
     xlab = "Age",
     ylab = "Frequency",
     main = "Longevity",
     type = "n")

# Step 5: Loop through valid indices to calculate and plot the KDEs
for(i in 1:length(valid_indices)) {
  m <- valid_indices[i]
  data <- get(paste("m", m, sep = "_"))
  
  # Plot the line for this valid data frame
  lines(0:(ncol(get(paste("m", m, sep = "_")))-1),
        apply(data, 2, mean),
        type="o",
        col=color_palette[i],
        lty=i,
        pch=16
  )
}

# Step 6: Add a legend
legend(x=105,y=15,
       title = "Transfer\nprobabilities",
       legend = trans_prob[valid_indices], # Use valid indices for the legend
       col = color_palette, # Use the corresponding colors from the palette
       lty = c(1:length(valid_indices)),
       pch=16,
       lwd = 2,
       bty = "n",
       xpd=T)

###Lifetime reproductive output ----

#get the frequency of each age across parameter values and repetitions
for (m in 1:19) {
  # create a data frame for each parameter value
  assign(paste("m", m, sep = "_"), data.frame(matrix(0, nrow = 0, ncol = 21)))
  # retrieve the data frame
  temp_df <- get(paste("m", m, sep = "_"))
  # modify the column names
  colnames(temp_df) <- as.character(0:20)
  # reassign the modified data frame in the original data frame
  assign(paste("m", m, sep = "_"), temp_df)
  #loop through repetitions
  for (r in 1:10) {
    #extract the Lifetime reproductive output
    lro_vector <- results_s3[grep(paste("m", m, "", sep = "_"), names(results_s3))][[r]]$lro
    #create a new row with zeros
    new_row <- rep(0, 21)
    #update the new row based on Lifetime reproductive output values
    for (val in lro_vector) {
      if (val >= 0 & val <= 20) {
        new_row[val + 1] <- new_row[val + 1] + 1  # +1 to account for index starting at 1 in R
      }
    }
    #change column names to match
    new_row_df <- as.data.frame(t(new_row))
    colnames(new_row_df) <- as.character(0:20)
    #add new row into the data frame
    temp_df <- get(paste("m", m, sep = "_"))  #current data frame
    temp_df <- rbind(temp_df, new_row_df)  #new row
    assign(paste("m", m, sep = "_"), temp_df)  #updated data frame
  }
}

#### Plot the densities ----

# Step 1: Create a vector to store indices of data frames with positive Lifetime reproductive output
valid_indices <- c()

# Step 2: First pass through to identify valid data frames
for(m in 1:19) {
  # Check if the sum of means is greater than zero
  if (sum(apply(get(paste("m", m, sep = "_")), 2, mean)) > 0) {
    valid_indices <- c(valid_indices, m)  # Store the valid index
  }
}

# Step 3: Create a color palette for only the valid data frames
color_palette <- hcl.colors(length(valid_indices), palette = "zissou 1", alpha = 0.5)

# Step 4: Prepare an empty plot

par(mfrow=c(1,1), mar=c(5, 4, 4, 8))

plot(c(0,20), c(0,75),
     xlab = "Age",
     ylab = "Frequency",
     main = "Lifetime reproductive output",
     type = "n")

# Step 5: Loop through valid indices to calculate and plot the KDEs
for(i in 1:length(valid_indices)) {
  m <- valid_indices[i]
  data <- get(paste("m", m, sep = "_"))
  
  # Plot the line for this valid data frame
  lines(0:(ncol(get(paste("m", m, sep = "_")))-1),
        apply(data, 2, mean),
        type="o",
        col=color_palette[i],
        lty=i,
        pch=16
  )
}

# Step 6: Add a legend
legend(x=21,y=75,
       title = "Transfer\nprobabilities",
       legend = trans_prob[valid_indices], # Use valid indices for the legend
       col = color_palette, # Use the corresponding colors from the palette
       lty = c(1:length(valid_indices)),
       pch=16,
       lwd = 2,
       bty = "n",
       xpd=T)

###Age at sexual maturity ----

#get the frequency of each age across parameter values and repetitions
for (m in 1:19) {
  # create a data frame for each parameter value
  assign(paste("m", m, sep = "_"), data.frame(matrix(0, nrow = 0, ncol = 101)))
  # retrieve the data frame
  temp_df <- get(paste("m", m, sep = "_"))
  # modify the column names
  colnames(temp_df) <- as.character(0:100)
  # reassign the modified data frame in the original data frame
  assign(paste("m", m, sep = "_"), temp_df)
  #loop through repetitions
  for (r in 1:10) {
    #extract the Age at sexual maturity
    asm_vector <- results_s3[grep(paste("m", m, "", sep = "_"), names(results_s3))][[r]]$asm
    #change NAs into zeros
    # asm_vector[is.na(asm_vector)] <- 0
    #create a new row with zeros
    new_row <- rep(0, 101)
    #update the new row based on Age at sexual maturity values
    for (val in asm_vector) {
      if (val >= 0 & val <= 100 & !is.na(val)) {
        new_row[val + 1] <- new_row[val + 1] + 1  # +1 to account for index starting at 1 in R
      }
    }
    #change column names to match
    new_row_df <- as.data.frame(t(new_row))
    colnames(new_row_df) <- as.character(0:100)
    #add new row into the data frame
    temp_df <- get(paste("m", m, sep = "_"))  #current data frame
    temp_df <- rbind(temp_df, new_row_df)  #new row
    assign(paste("m", m, sep = "_"), temp_df)  #updated data frame
  }
}

#### Plot the densities ----

# Step 1: Create a vector to store indices of data frames with positive Age at sexual maturity
valid_indices <- c()

# Step 2: First pass through to identify valid data frames
for(m in 1:19) {
  # Check if the sum of means is greater than zero
  if (sum(apply(get(paste("m", m, sep = "_")), 2, mean)) > 0) {
    valid_indices <- c(valid_indices, m)  # Store the valid index
  }
}

# Step 3: Create a color palette for only the valid data frames
color_palette <- hcl.colors(length(valid_indices), palette = "zissou 1", alpha = 0.5)

# Step 4: Prepare an empty plot

par(mfrow=c(1,1), mar=c(5, 4, 4, 8))

plot(c(10,25), c(0,40),
     xlab = "Age",
     ylab = "Frequency",
     main = "Age at sexual maturity",
     type = "n")

# Step 5: Loop through valid indices to calculate and plot the KDEs
for(i in 1:length(valid_indices)) {
  m <- valid_indices[i]
  data <- get(paste("m", m, sep = "_"))
  
  # Plot the line for this valid data frame
  lines(0:(ncol(get(paste("m", m, sep = "_")))-1),
        apply(data, 2, mean),
        type="o",
        col=color_palette[i],
        lty=i,
        pch=16
  )
}

# Step 6: Add a legend
legend(x=25.7,y=40,
       title = "Transfer\nprobabilities",
       legend = trans_prob[valid_indices], # Use valid indices for the legend
       col = color_palette, # Use the corresponding colors from the palette
       lty = c(1:length(valid_indices)),
       pch=16,
       lwd = 2,
       bty = "n",
       xpd=T)

###Age at first reproduction ----

#get the frequency of each age across parameter values and repetitions
for (m in 1:19) {
  # create a data frame for each parameter value
  assign(paste("m", m, sep = "_"), data.frame(matrix(0, nrow = 0, ncol = 101)))
  # retrieve the data frame
  temp_df <- get(paste("m", m, sep = "_"))
  # modify the column names
  colnames(temp_df) <- as.character(0:100)
  # reassign the modified data frame in the original data frame
  assign(paste("m", m, sep = "_"), temp_df)
  #loop through repetitions
  for (r in 1:10) {
    #extract the Age at first reproduction
    afr_vector <- results_s3[grep(paste("m", m, "", sep = "_"), names(results_s3))][[r]]$afr
    #change NAs into zeros
    # afr_vector[is.na(afr_vector)] <- 0
    #create a new row with zeros
    new_row <- rep(0, 101)
    #update the new row based on Age at first reproduction values
    for (val in afr_vector) {
      if (val >= 0 & val <= 100 & !is.na(val)) {
        new_row[val + 1] <- new_row[val + 1] + 1  # +1 to account for index starting at 1 in R
      }
    }
    #change column names to match
    new_row_df <- as.data.frame(t(new_row))
    colnames(new_row_df) <- as.character(0:100)
    #add new row into the data frame
    temp_df <- get(paste("m", m, sep = "_"))  #current data frame
    temp_df <- rbind(temp_df, new_row_df)  #new row
    assign(paste("m", m, sep = "_"), temp_df)  #updated data frame
  }
}

#### Plot the densities ----

# Step 1: Create a vector to store indices of data frames with positive Age at first reproduction
valid_indices <- c()

# Step 2: First pass through to identify valid data frames
for(m in 1:19) {
  # Check if the sum of means is greater than zero
  if (sum(apply(get(paste("m", m, sep = "_")), 2, mean)) > 0) {
    valid_indices <- c(valid_indices, m)  # Store the valid index
  }
}

# Step 3: Create a color palette for only the valid data frames
color_palette <- hcl.colors(length(valid_indices), palette = "zissou 1", alpha = 0.5)

# Step 4: Prepare an empty plot

par(mfrow=c(1,1), mar=c(5, 4, 4, 8))

plot(c(10,70), c(0,5),
     xlab = "Age",
     ylab = "Frequency",
     main = "Age at first reproduction",
     type = "n")

# Step 5: Loop through valid indices to calculate and plot the KDEs
for(i in 1:length(valid_indices)) {
  m <- valid_indices[i]
  data <- get(paste("m", m, sep = "_"))
  
  # Plot the line for this valid data frame
  lines(0:(ncol(get(paste("m", m, sep = "_")))-1),
        apply(data, 2, mean),
        type="o",
        col=color_palette[i],
        lty=i,
        pch=16
  )
}

# Step 6: Add a legend
legend(x=73,y=5,
       title = "Transfer\nprobabilities",
       legend = trans_prob[valid_indices], # Use valid indices for the legend
       col = color_palette, # Use the corresponding colors from the palette
       lty = c(1:length(valid_indices)),
       lwd = 2,
       pch=16,
       bty = "n",
       xpd=T)

###Age at last reproduction ----

#get the frequency of each age across parameter values and repetitions
for (m in 1:19) {
  # create a data frame for each parameter value
  assign(paste("m", m, sep = "_"), data.frame(matrix(0, nrow = 0, ncol = 101)))
  # retrieve the data frame
  temp_df <- get(paste("m", m, sep = "_"))
  # modify the column names
  colnames(temp_df) <- as.character(0:100)
  # reassign the modified data frame in the original data frame
  assign(paste("m", m, sep = "_"), temp_df)
  #loop through repetitions
  for (r in 1:10) {
    #extract the Age at last reproduction
    alr_vector <- results_s3[grep(paste("m", m, "", sep = "_"), names(results_s3))][[r]]$alr
    #change NAs into zeros
    # alr_vector[is.na(alr_vector)] <- 0
    #create a new row with zeros
    new_row <- rep(0, 101)
    #update the new row based on Age at last reproduction values
    for (val in alr_vector) {
      if (val >= 0 & val <= 100 & !is.na(val)) {
        new_row[val + 1] <- new_row[val + 1] + 1  # +1 to account for index starting at 1 in R
      }
    }
    #change column names to match
    new_row_df <- as.data.frame(t(new_row))
    colnames(new_row_df) <- as.character(0:100)
    #add new row into the data frame
    temp_df <- get(paste("m", m, sep = "_"))  #current data frame
    temp_df <- rbind(temp_df, new_row_df)  #new row
    assign(paste("m", m, sep = "_"), temp_df)  #updated data frame
  }
}

#### Plot the densities ----

# Step 1: Create a vector to store indices of data frames with positive Age at last reproduction
valid_indices <- c()

# Step 2: First pass through to identify valid data frames
for(m in 1:19) {
  # Check if the sum of means is greater than zero
  if (sum(apply(get(paste("m", m, sep = "_")), 2, mean)) > 0) {
    valid_indices <- c(valid_indices, m)  # Store the valid index
  }
}

# Step 3: Create a color palette for only the valid data frames
color_palette <- hcl.colors(length(valid_indices), palette = "zissou 1", alpha = 0.5)

# Step 4: Prepare an empty plot

par(mfrow=c(1,1), mar=c(5, 4, 4, 8))

plot(c(10,65), c(0,4),
     xlab = "Age",
     ylab = "Frequency",
     main = "Age at last reproduction",
     type = "n")

# Step 5: Loop through valid indices to calculate and plot the KDEs
for(i in 1:length(valid_indices)) {
  m <- valid_indices[i]
  data <- get(paste("m", m, sep = "_"))
  
  # Plot the line for this valid data frame
  lines(0:(ncol(get(paste("m", m, sep = "_")))-1),
        apply(data, 2, mean),
        type="o",
        col=color_palette[i],
        lty=i,
        pch=16
  )
}

# Step 6: Add a legend
legend(x=67.5,y=4,
       title = "Transfer\nprobabilities",
       legend = trans_prob[valid_indices], # Use valid indices for the legend
       col = color_palette, # Use the corresponding colors from the palette
       lty = c(1:length(valid_indices)),
       pch=16,
       lwd = 2,
       bty = "n",
       xpd=T)

###Age at menopause ----

#get the frequency of each age across parameter values and repetitions
for (m in 1:19) {
  # create a data frame for each parameter value
  assign(paste("m", m, sep = "_"), data.frame(matrix(0, nrow = 0, ncol = 101)))
  # retrieve the data frame
  temp_df <- get(paste("m", m, sep = "_"))
  # modify the column names
  colnames(temp_df) <- as.character(0:100)
  # reassign the modified data frame in the original data frame
  assign(paste("m", m, sep = "_"), temp_df)
  #loop through repetitions
  for (r in 1:10) {
    #extract the Age at menopause
    meno_vector <- results_s3[grep(paste("m", m, "", sep = "_"), names(results_s3))][[r]]$meno
    #change NAs into zeros
    # meno_vector[is.na(meno_vector)] <- 0
    #create a new row with zeros
    new_row <- rep(0, 101)
    #update the new row based on Age at menopause values
    for (val in meno_vector) {
      if (val >= 0 & val <= 100 & !is.na(val)) {
        new_row[val + 1] <- new_row[val + 1] + 1  # +1 to account for index starting at 1 in R
      }
    }
    #change column names to match
    new_row_df <- as.data.frame(t(new_row))
    colnames(new_row_df) <- as.character(0:100)
    #add new row into the data frame
    temp_df <- get(paste("m", m, sep = "_"))  #current data frame
    temp_df <- rbind(temp_df, new_row_df)  #new row
    assign(paste("m", m, sep = "_"), temp_df)  #updated data frame
  }
}

#### Plot the densities ----

# Step 1: Create a vector to store indices of data frames with positive Age at menopause
valid_indices <- c()

# Step 2: First pass through to identify valid data frames
for(m in 1:19) {
  # Check if the sum of means is greater than zero
  if (sum(apply(get(paste("m", m, sep = "_")), 2, mean)) > 0) {
    valid_indices <- c(valid_indices, m)  # Store the valid index
  }
}

# Step 3: Create a color palette for only the valid data frames
color_palette <- hcl.colors(length(valid_indices), palette = "zissou 1", alpha = 0.5)

# Step 4: Prepare an empty plot

par(mfrow=c(1,1), mar=c(5, 4, 4, 8))

plot(c(30,70), c(0,15),
     xlab = "Age",
     ylab = "Frequency",
     main = "Age at menopause",
     type = "n")

# Step 5: Loop through valid indices to calculate and plot the KDEs
for(i in 1:length(valid_indices)) {
  m <- valid_indices[i]
  data <- get(paste("m", m, sep = "_"))
  
  # Plot the line for this valid data frame
  lines(0:(ncol(get(paste("m", m, sep = "_")))-1),
        apply(data, 2, mean),
        type="o",
        col=color_palette[i],
        lty=i,
        pch=16
  )
}

# Step 6: Add a legend
legend(x=72.5,y=15,
       title = "Transfer\nprobabilities",
       legend = trans_prob[valid_indices], # Use valid indices for the legend
       col = color_palette, # Use the corresponding colors from the palette
       lty = c(1:length(valid_indices)),
       pch=16,
       lwd = 2,
       bty = "n",
       xpd=T)
