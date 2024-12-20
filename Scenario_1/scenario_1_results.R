# Life cycle variation and resource dynamics ABM: Scenario 1 - results ----

#Here is the code to get the summary statistics and plots from the simulation of Scenario 1, which aims to understand the influence of resource production on the variability of life cycles.

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

#Data import ----

#import raw results from the simulation
results <- readRDS("./Scenario_1/lht_list_s1.RData")

#get the parameter values for labeling

#source the functions
#Stage-specific maximum amount of resource production 
source("./Model_code/production_maxprod_fx.R")
#Stage-specific production probabilities
source("./Model_code/production_prodprob_fx.R")
#get the values
#habitat quality
habitat_quality <- 4
#stage-specific maximum amount of resource production.
maxprod <- max_production(habitat_quality)
#maximum production probability
max_prod_prob <- seq(0.1,0.9,length=17)
#stage-specific production probabilities.
prod_prob <- production_prob(max_prod_prob)

#Summary statistics ----

summary_stats <- lapply(1:length(results),function(x)list())
#match the original names with the sample
names(summary_stats) <- names(results)
names(summary_stats)

#gather summary statistics of each result
for (i in 1:length(results)){
  if(nrow(results[[i]]) == 0){
    summary_stats[[i]] <- summary_stats[[i]]
  } else{
    summary_stats[[i]]$summary <- summary(results[[i]][,c("lng","lro","asm","afr","alr","meno")])
    summary_stats[[i]]$sd <- as.data.frame(lapply(results[[i]][,c("lng","lro","asm","afr","alr","meno")],sd,na.rm=T))
    summary_stats[[i]]$mean <- as.data.frame(lapply(results[[i]][,c("lng","lro","asm","afr","alr","meno")],mean,na.rm=T))
    summary_stats[[i]]$cv <- summary_stats[[i]]$sd / summary_stats[[i]]$mean  
    if(nrow(results[[i]]) == 1 | summary_stats[[i]]$sd$lng == 0 & summary_stats[[i]]$sd$lro == 0){
      summary_stats[[i]]$summary <- summary(results[[i]][,c("lng","lro","asm","afr","alr","meno")])
      summary_stats[[i]]$sd <- as.data.frame(lapply(results[[i]][,c("lng","lro","asm","afr","alr","meno")],sd,na.rm=T))
      summary_stats[[i]]$sd[,c("lng","lro")][which(is.na(summary_stats[[i]]$sd[,c("lng","lro")]))] <- 0
      summary_stats[[i]]$mean <- as.data.frame(lapply(results[[i]][,c("lng","lro","asm","afr","alr","meno")],mean,na.rm=T))
      summary_stats[[i]]$cv[,c("lng","lro")] <- 0  
    } 
  }
}

## CV sorted by life history trait ----

#lng
#create data frame to gather CVs of lng
lng_cv <- as.data.frame(matrix(NA,17,11))
colnames(lng_cv) <- c("p",1:10)
lng_cv$p <- prod_prob[2,]
#check it
lng_cv
#extract the CV of lng across the parameter space
for(d in 1:nrow(lng_cv)){
  for(r in 2:ncol(lng_cv)){
    if(is.null(summary_stats[grep(paste("d",d,"",sep="_"),names(summary_stats))][[r-1]]$cv$lng) == T){
      lng_cv[d,r] <- NA
    }else{
      lng_cv[d,r] <- summary_stats[grep(paste("d",d,"",sep="_"),names(summary_stats))][[r-1]]$cv$lng
    }
  }
}
#check it
lng_cv

#lro
#create data frame to gather CVs of lro
lro_cv <- as.data.frame(matrix(NA,17,11))
colnames(lro_cv) <- c("p",1:10)
lro_cv$p <- prod_prob[2,]
#check it
lro_cv
#extract the CV of lro across the parameter space
for(d in 1:nrow(lro_cv)){
  for(r in 2:ncol(lro_cv)){
    if(is.null(summary_stats[grep(paste("d",d,"",sep="_"),names(summary_stats))][[r-1]]$cv$lro) == T){
      lro_cv[d,r] <- NA
    }else{
      lro_cv[d,r] <- summary_stats[grep(paste("d",d,"",sep="_"),names(summary_stats))][[r-1]]$cv$lro
    }
  }
}
#check it
lro_cv

#asm
#create data frame to gather CVs of asm
asm_cv <- as.data.frame(matrix(NA,17,11))
colnames(asm_cv) <- c("p",1:10)
asm_cv$p <- prod_prob[2,]
#check it
asm_cv
#extract the CV of asm across the parameter space
for(d in 1:nrow(asm_cv)){
  for(r in 2:ncol(asm_cv)){
    if(is.null(summary_stats[grep(paste("d",d,"",sep="_"),names(summary_stats))][[r-1]]$cv$asm) == T){
      asm_cv[d,r] <- NA
    }else{
      asm_cv[d,r] <- summary_stats[grep(paste("d",d,"",sep="_"),names(summary_stats))][[r-1]]$cv$asm
    }
  }
}
#check it
asm_cv

#afr
#create data frame to gather CVs of afr
afr_cv <- as.data.frame(matrix(NA,17,11))
colnames(afr_cv) <- c("p",1:10)
afr_cv$p <- prod_prob[2,]
#check it
afr_cv
#extract the CV of afr across the parameter space
for(d in 1:nrow(afr_cv)){
  for(r in 2:ncol(afr_cv)){
    if(is.null(summary_stats[grep(paste("d",d,"",sep="_"),names(summary_stats))][[r-1]]$cv$afr) == T){
      afr_cv[d,r] <- NA
    }else{
      afr_cv[d,r] <- summary_stats[grep(paste("d",d,"",sep="_"),names(summary_stats))][[r-1]]$cv$afr
    }
  }
}
#check it
afr_cv

#alr
#create data frame to gather CVs of alr
alr_cv <- as.data.frame(matrix(NA,17,11))
colnames(alr_cv) <- c("p",1:10)
alr_cv$p <- prod_prob[2,]
#check it
alr_cv
#extract the CV of alr across the parameter space
for(d in 1:nrow(alr_cv)){
  for(r in 2:ncol(alr_cv)){
    if(is.null(summary_stats[grep(paste("d",d,"",sep="_"),names(summary_stats))][[r-1]]$cv$alr) == T){
      alr_cv[d,r] <- NA
    }else{
      alr_cv[d,r] <- summary_stats[grep(paste("d",d,"",sep="_"),names(summary_stats))][[r-1]]$cv$alr
    }
  }
}
#check it
alr_cv

#meno
#create data frame to gather CVs of meno
meno_cv <- as.data.frame(matrix(NA,17,11))
colnames(meno_cv) <- c("p",1:10)
meno_cv$p <- prod_prob[2,]
#check it
meno_cv
#extract the CV of meno across the parameter space
for(d in 1:nrow(meno_cv)){
  for(r in 2:ncol(meno_cv)){
    if(is.null(summary_stats[grep(paste("d",d,"",sep="_"),names(summary_stats))][[r-1]]$cv$meno) == T){
      meno_cv[d,r] <- NA
    }else{
      meno_cv[d,r] <- summary_stats[grep(paste("d",d,"",sep="_"),names(summary_stats))][[r-1]]$cv$meno
    }
  }
}
#check it
meno_cv

## Median CV per life history trait ----

#Longevity
#create data frame
lht_median <- as.data.frame(matrix(NA,ncol(prod_prob),7))
colnames(lht_median) <- c("p","lng","lro","asm","afr","alr","meno")
lht_median$p <- prod_prob[2,]
lht_median
#calculate the median CV per life history trait
for(i in 1:nrow(lht_median)){
  lht_median[i,"lng"] <- ifelse(sum(is.na(lng_cv[i,2:ncol(lng_cv)]))==10,NA,median(as.numeric(lng_cv[i,2:ncol(lng_cv)]),na.rm=T))
  lht_median[i,"lro"] <- ifelse(sum(is.na(lro_cv[i,2:ncol(lro_cv)]))==10,NA,median(as.numeric(lro_cv[i,2:ncol(lro_cv)]),na.rm=T))
  lht_median[i,"asm"] <- ifelse(sum(is.na(asm_cv[i,2:ncol(asm_cv)]))==10,NA,median(as.numeric(asm_cv[i,2:ncol(asm_cv)]),na.rm=T))
  lht_median[i,"afr"] <- ifelse(sum(is.na(afr_cv[i,2:ncol(afr_cv)]))==10,NA,median(as.numeric(afr_cv[i,2:ncol(afr_cv)]),na.rm=T))
  lht_median[i,"alr"] <- ifelse(sum(is.na(alr_cv[i,2:ncol(alr_cv)]))==10,NA,median(as.numeric(alr_cv[i,2:ncol(alr_cv)]),na.rm=T))
  lht_median[i,"meno"] <- ifelse(sum(is.na(meno_cv[i,2:ncol(meno_cv)]))==10,NA,median(as.numeric(meno_cv[i,2:ncol(meno_cv)]),na.rm=T))
}
lht_median

#Plot it! ----

##Median all together ----

#get the maximum CV of all the life history traits for the scale in the plots
#create empty vector
cv_all <- NA
#select the CV of all life history traits
for(i in 1:length(summary_stats)){
  if(is.null(summary_stats[[i]])==T){
    cv_all <- c(cv_all,NA)
  }else{
    cv_all <- c(cv_all,summary_stats[[i]]$cv)
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
     xlab="Parameter value",
     ylab="CV longevity",
     ylim=c(0,max(cv_all)),
     type="n")
for(d in 1:nrow(lng_cv)){
    points(as.numeric(lng_cv[d,2:11])~rep(lng_cv[d,1],10),
           pch=16,
           col=alpha("grey",0.5)
           )    
}
points(lht_median$lng~lht_median$p,pch=16,cex=1)

### Lifetime reproductive output ----

plot(c(0,max(cv_all)+0.5)~c(0,1),
     main="Lifetime\nreproductive output",
     xlab="Parameter value",
     ylab="CV lifetime reproductive output",
     ylim=c(0,max(cv_all)),
     type="n")
for(d in 1:nrow(lro_cv)){
  points(as.numeric(lro_cv[d,2:11])~rep(lro_cv[d,1],10),
         pch=16,
         col=alpha("grey",0.5)
  )    
}
points(lht_median$lro~lht_median$p,pch=16,cex=1)

### Age at sexual maturity ----

plot(c(0,max(cv_all)+0.5)~c(0,1),
     main="Age at\nsexual maturity",
     xlab="Parameter value",
     ylab="CV age at sexual maturity",
     ylim=c(0,max(cv_all)),
     type="n")
for(d in 1:nrow(asm_cv)){
  points(as.numeric(asm_cv[d,2:11])~rep(asm_cv[d,1],10),
         pch=16,
         col=alpha("grey",0.5)
  )    
}
points(lht_median$asm~lht_median$p,pch=16,cex=1)

### Age at first reproduction ----

plot(c(0,max(cv_all)+0.5)~c(0,1),
     main="Age at\nfirst reproduction",
     xlab="Parameter value",
     ylab="CV age at first reproduction",
     ylim=c(0,max(cv_all)),
     type="n")
for(d in 1:nrow(afr_cv)){
  points(as.numeric(afr_cv[d,2:11])~rep(afr_cv[d,1],10),
         pch=16,
         col=alpha("grey",0.5)
  )    
}
points(lht_median$afr~lht_median$p,pch=16,cex=1)

### Age at last reproduction ----

plot(c(0,max(cv_all)+0.5)~c(0,1),
     main="Age at\nlast reproduction",
     xlab="Parameter value",
     ylab="CV age at last reproduction",
     ylim=c(0,max(cv_all)),
     type="n")
for(d in 1:nrow(alr_cv)){
  points(as.numeric(alr_cv[d,2:11])~rep(alr_cv[d,1],10),
         pch=16,
         col=alpha("grey",0.5)
  )    
}
points(lht_median$alr~lht_median$p,pch=16,cex=1)

### Age at menopause ----

plot(c(0,max(cv_all)+0.5)~c(0,1),
     main="Age at\nmenopause",
     xlab="Parameter value",
     ylab="CV age at menopause",
     ylim=c(0,max(cv_all)),
     type="n")
for(d in 1:nrow(meno_cv)){
  points(as.numeric(meno_cv[d,2:11])~rep(meno_cv[d,1],10),
         pch=16,
         col=alpha("grey",0.5)
  )    
}
points(lht_median$meno~lht_median$p,pch=16,cex=1)

##Distribution of life history traits ----

###Longevity ----

#get the frequency of each age across parameter values and repetitions
for (d in 1:17) {
  # create a data frame for each parameter value
  assign(paste("d", d, sep = "_"), data.frame(matrix(0, nrow = 0, ncol = 101)))
  # retrieve the data frame
  temp_df <- get(paste("d", d, sep = "_"))
  # modify the column names
  colnames(temp_df) <- as.character(0:100)
  # reassign the modified data frame in the original data frame
  assign(paste("d", d, sep = "_"), temp_df)
  #loop through repetitions
  for (r in 1:10) {
    #extract the longevity
    lng_vector <- results[grep(paste("d", d, "", sep = "_"), names(results))][[r]]$lng
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
    temp_df <- get(paste("d", d, sep = "_"))  #current data frame
    temp_df <- rbind(temp_df, new_row_df)  #new row
    assign(paste("d", d, sep = "_"), temp_df)  #updated data frame
  }
}

#### Plot the densities ----

# Step 1: Create a vector to store indices of data frames with positive longevity
valid_indices <- c()

# Step 2: First pass through to identify valid data frames
for(d in 1:17) {
  # Check if the sum of means is greater than zero
  if (sum(apply(get(paste("d", d, sep = "_")), 2, mean)) > 0) {
    valid_indices <- c(valid_indices, d)  # Store the valid index
  }
}

# Step 3: Create a color palette for only the valid data frames
color_palette <- hcl.colors(length(valid_indices), palette = "zissou 1", alpha = 0.5)

# Step 4: Prepare an empty plot

par(mfrow=c(1,1), mar=c(5, 4, 4, 8))

plot(c(0,100), c(0,150),
     xlab = "Age",
     ylab = "Frequency",
     main = "Longevity",
     type = "n")

# Step 5: Loop through valid indices to calculate and plot the KDEs
for(i in 1:length(valid_indices)) {
  d <- valid_indices[i]
  data <- get(paste("d", d, sep = "_"))
  
  # Plot the line for this valid data frame
  lines(0:(ncol(get(paste("d", d, sep = "_")))-1),
        apply(data, 2, mean),
        type="o",
        col=color_palette[i],
        lty=i,
        pch=16
  )
}

# Step 6: Add a legend
legend(x=105,y=150,
       title = "Production\nprobabilities",
       legend = prod_prob[2,valid_indices], # Use valid indices for the legend
       col = color_palette, # Use the corresponding colors from the palette
       lty = c(1:length(valid_indices)),
       pch=16,
       lwd = 2,
       bty = "n",
       xpd=T)

###Lifetime reproductive output ----

#get the frequency of each age across parameter values and repetitions
for (d in 1:17) {
  # create a data frame for each parameter value
  assign(paste("d", d, sep = "_"), data.frame(matrix(0, nrow = 0, ncol = 21)))
  # retrieve the data frame
  temp_df <- get(paste("d", d, sep = "_"))
  # modify the column names
  colnames(temp_df) <- as.character(0:20)
  # reassign the modified data frame in the original data frame
  assign(paste("d", d, sep = "_"), temp_df)
  #loop through repetitions
  for (r in 1:10) {
    #extract the Lifetime reproductive output
    lro_vector <- results[grep(paste("d", d, "", sep = "_"), names(results))][[r]]$lro
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
    temp_df <- get(paste("d", d, sep = "_"))  #current data frame
    temp_df <- rbind(temp_df, new_row_df)  #new row
    assign(paste("d", d, sep = "_"), temp_df)  #updated data frame
  }
}

#### Plot the densities ----

# Step 1: Create a vector to store indices of data frames with positive Lifetime reproductive output
valid_indices <- c()

# Step 2: First pass through to identify valid data frames
for(d in 1:17) {
  # Check if the sum of means is greater than zero
  if (sum(apply(get(paste("d", d, sep = "_")), 2, mean)) > 0) {
    valid_indices <- c(valid_indices, d)  # Store the valid index
  }
}

# Step 3: Create a color palette for only the valid data frames
color_palette <- hcl.colors(length(valid_indices), palette = "zissou 1", alpha = 0.5)

# Step 4: Prepare an empty plot

par(mfrow=c(1,1), mar=c(5, 4, 4, 8))

plot(c(0,20), c(0,1200),
     xlab = "Age",
     ylab = "Frequency",
     main = "Lifetime reproductive output",
     type = "n")

# Step 5: Loop through valid indices to calculate and plot the KDEs
for(i in 1:length(valid_indices)) {
  d <- valid_indices[i]
  data <- get(paste("d", d, sep = "_"))
  
  # Plot the line for this valid data frame
  lines(0:(ncol(get(paste("d", d, sep = "_")))-1),
        apply(data, 2, mean),
        type="o",
        col=color_palette[i],
        lty=i,
        pch=16
  )
}

# Step 6: Add a legend
legend(x=21,y=1200,
       title = "Production\nprobabilities",
       legend = prod_prob[2,valid_indices], # Use valid indices for the legend
       col = color_palette, # Use the corresponding colors from the palette
       lty = c(1:length(valid_indices)),
       pch=16,
       lwd = 2,
       bty = "n",
       xpd=T)

###Age at sexual maturity ----

#get the frequency of each age across parameter values and repetitions
for (d in 1:17) {
  # create a data frame for each parameter value
  assign(paste("d", d, sep = "_"), data.frame(matrix(0, nrow = 0, ncol = 101)))
  # retrieve the data frame
  temp_df <- get(paste("d", d, sep = "_"))
  # modify the column names
  colnames(temp_df) <- as.character(0:100)
  # reassign the modified data frame in the original data frame
  assign(paste("d", d, sep = "_"), temp_df)
  #loop through repetitions
  for (r in 1:10) {
    #extract the Age at sexual maturity
    asm_vector <- results[grep(paste("d", d, "", sep = "_"), names(results))][[r]]$asm
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
    temp_df <- get(paste("d", d, sep = "_"))  #current data frame
    temp_df <- rbind(temp_df, new_row_df)  #new row
    assign(paste("d", d, sep = "_"), temp_df)  #updated data frame
  }
}

#### Plot the densities ----

# Step 1: Create a vector to store indices of data frames with positive Age at sexual maturity
valid_indices <- c()

# Step 2: First pass through to identify valid data frames
for(d in 1:17) {
  # Check if the sum of means is greater than zero
  if (sum(apply(get(paste("d", d, sep = "_")), 2, mean)) > 0) {
    valid_indices <- c(valid_indices, d)  # Store the valid index
  }
}

# Step 3: Create a color palette for only the valid data frames
color_palette <- hcl.colors(length(valid_indices), palette = "zissou 1", alpha = 0.5)

# Step 4: Prepare an empty plot

par(mfrow=c(1,1), mar=c(5, 4, 4, 8))

plot(c(10,25), c(0,250),
     xlab = "Age",
     ylab = "Frequency",
     main = "Age at sexual maturity",
     type = "n")

# Step 5: Loop through valid indices to calculate and plot the KDEs
for(i in 1:length(valid_indices)) {
  d <- valid_indices[i]
  data <- get(paste("d", d, sep = "_"))
  
  # Plot the line for this valid data frame
  lines(0:(ncol(get(paste("d", d, sep = "_")))-1),
        apply(data, 2, mean),
        type="o",
        col=color_palette[i],
        lty=i,
        pch=16
  )
}

# Step 6: Add a legend
legend(x=25.7,y=250,
       title = "Production\nprobabilities",
       legend = prod_prob[2,valid_indices], # Use valid indices for the legend
       col = color_palette, # Use the corresponding colors from the palette
       lty = c(1:length(valid_indices)),
       pch=16,
       lwd = 2,
       bty = "n",
       xpd=T)

###Age at first reproduction ----

#get the frequency of each age across parameter values and repetitions
for (d in 1:17) {
  # create a data frame for each parameter value
  assign(paste("d", d, sep = "_"), data.frame(matrix(0, nrow = 0, ncol = 101)))
  # retrieve the data frame
  temp_df <- get(paste("d", d, sep = "_"))
  # modify the column names
  colnames(temp_df) <- as.character(0:100)
  # reassign the modified data frame in the original data frame
  assign(paste("d", d, sep = "_"), temp_df)
  #loop through repetitions
  for (r in 1:10) {
    #extract the Age at first reproduction
    afr_vector <- results[grep(paste("d", d, "", sep = "_"), names(results))][[r]]$afr
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
    temp_df <- get(paste("d", d, sep = "_"))  #current data frame
    temp_df <- rbind(temp_df, new_row_df)  #new row
    assign(paste("d", d, sep = "_"), temp_df)  #updated data frame
  }
}

#### Plot the densities ----

# Step 1: Create a vector to store indices of data frames with positive Age at first reproduction
valid_indices <- c()

# Step 2: First pass through to identify valid data frames
for(d in 1:17) {
  # Check if the sum of means is greater than zero
  if (sum(apply(get(paste("d", d, sep = "_")), 2, mean)) > 0) {
    valid_indices <- c(valid_indices, d)  # Store the valid index
  }
}

# Step 3: Create a color palette for only the valid data frames
color_palette <- hcl.colors(length(valid_indices), palette = "zissou 1", alpha = 0.5)

# Step 4: Prepare an empty plot

par(mfrow=c(1,1), mar=c(5, 4, 4, 8))

plot(c(10,30), c(0,180),
     xlab = "Age",
     ylab = "Frequency",
     main = "Age at first reproduction",
     type = "n")

# Step 5: Loop through valid indices to calculate and plot the KDEs
for(i in 1:length(valid_indices)) {
  d <- valid_indices[i]
  data <- get(paste("d", d, sep = "_"))
  
  # Plot the line for this valid data frame
  lines(0:(ncol(get(paste("d", d, sep = "_")))-1),
        apply(data, 2, mean),
        type="o",
        col=color_palette[i],
        lty=i,
        pch=16
  )
}

# Step 6: Add a legend
legend(x=31,y=180,
       title = "Production\nprobabilities",
       legend = prod_prob[2,valid_indices], # Use valid indices for the legend
       col = color_palette, # Use the corresponding colors from the palette
       lty = c(1:length(valid_indices)),
       lwd = 2,
       pch=16,
       bty = "n",
       xpd=T)

###Age at last reproduction ----

#get the frequency of each age across parameter values and repetitions
for (d in 1:17) {
  # create a data frame for each parameter value
  assign(paste("d", d, sep = "_"), data.frame(matrix(0, nrow = 0, ncol = 101)))
  # retrieve the data frame
  temp_df <- get(paste("d", d, sep = "_"))
  # modify the column names
  colnames(temp_df) <- as.character(0:100)
  # reassign the modified data frame in the original data frame
  assign(paste("d", d, sep = "_"), temp_df)
  #loop through repetitions
  for (r in 1:10) {
    #extract the Age at last reproduction
    alr_vector <- results[grep(paste("d", d, "", sep = "_"), names(results))][[r]]$alr
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
    temp_df <- get(paste("d", d, sep = "_"))  #current data frame
    temp_df <- rbind(temp_df, new_row_df)  #new row
    assign(paste("d", d, sep = "_"), temp_df)  #updated data frame
  }
}

#### Plot the densities ----

# Step 1: Create a vector to store indices of data frames with positive Age at last reproduction
valid_indices <- c()

# Step 2: First pass through to identify valid data frames
for(d in 1:17) {
  # Check if the sum of means is greater than zero
  if (sum(apply(get(paste("d", d, sep = "_")), 2, mean)) > 0) {
    valid_indices <- c(valid_indices, d)  # Store the valid index
  }
}

# Step 3: Create a color palette for only the valid data frames
color_palette <- hcl.colors(length(valid_indices), palette = "zissou 1", alpha = 0.5)

# Step 4: Prepare an empty plot

par(mfrow=c(1,1), mar=c(5, 4, 4, 8))

plot(c(10,65), c(0,80),
     xlab = "Age",
     ylab = "Frequency",
     main = "Age at last reproduction",
     type = "n")

# Step 5: Loop through valid indices to calculate and plot the KDEs
for(i in 1:length(valid_indices)) {
  d <- valid_indices[i]
  data <- get(paste("d", d, sep = "_"))
  
  # Plot the line for this valid data frame
  lines(0:(ncol(get(paste("d", d, sep = "_")))-1),
        apply(data, 2, mean),
        type="o",
        col=color_palette[i],
        lty=i,
        pch=16
  )
}

# Step 6: Add a legend
legend(x=67.5,y=80,
       title = "Production\nprobabilities",
       legend = prod_prob[2,valid_indices], # Use valid indices for the legend
       col = color_palette, # Use the corresponding colors from the palette
       lty = c(1:length(valid_indices)),
       pch=16,
       lwd = 2,
       bty = "n",
       xpd=T)

###Age at menopause ----

#get the frequency of each age across parameter values and repetitions
for (d in 1:17) {
  # create a data frame for each parameter value
  assign(paste("d", d, sep = "_"), data.frame(matrix(0, nrow = 0, ncol = 101)))
  # retrieve the data frame
  temp_df <- get(paste("d", d, sep = "_"))
  # modify the column names
  colnames(temp_df) <- as.character(0:100)
  # reassign the modified data frame in the original data frame
  assign(paste("d", d, sep = "_"), temp_df)
  #loop through repetitions
  for (r in 1:10) {
    #extract the Age at menopause
    meno_vector <- results[grep(paste("d", d, "", sep = "_"), names(results))][[r]]$meno
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
    temp_df <- get(paste("d", d, sep = "_"))  #current data frame
    temp_df <- rbind(temp_df, new_row_df)  #new row
    assign(paste("d", d, sep = "_"), temp_df)  #updated data frame
  }
}

#### Plot the densities ----

# Step 1: Create a vector to store indices of data frames with positive Age at menopause
valid_indices <- c()

# Step 2: First pass through to identify valid data frames
for(d in 1:17) {
  # Check if the sum of means is greater than zero
  if (sum(apply(get(paste("d", d, sep = "_")), 2, mean)) > 0) {
    valid_indices <- c(valid_indices, d)  # Store the valid index
  }
}

# Step 3: Create a color palette for only the valid data frames
color_palette <- hcl.colors(length(valid_indices), palette = "zissou 1", alpha = 0.5)

# Step 4: Prepare an empty plot

par(mfrow=c(1,1), mar=c(5, 4, 4, 8))

plot(c(30,70), c(0,220),
     xlab = "Age",
     ylab = "Frequency",
     main = "Age at menopause",
     type = "n")

# Step 5: Loop through valid indices to calculate and plot the KDEs
for(i in 1:length(valid_indices)) {
  d <- valid_indices[i]
  data <- get(paste("d", d, sep = "_"))
  
  # Plot the line for this valid data frame
  lines(0:(ncol(get(paste("d", d, sep = "_")))-1),
        apply(data, 2, mean),
        type="o",
        col=color_palette[i],
        lty=i,
        pch=16
  )
}

# Step 6: Add a legend
legend(x=72.5,y=220,
       title = "Production\nprobabilities",
       legend = prod_prob[2,valid_indices], # Use valid indices for the legend
       col = color_palette, # Use the corresponding colors from the palette
       lty = c(1:length(valid_indices)),
       pch=16,
       lwd = 2,
       bty = "n",
       xpd=T)
