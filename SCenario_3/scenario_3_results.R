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
results <- readRDS("./Scenario_3/lht_list_s3.RData")

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

summary_stats <- lapply(1:length(results),function(x)list())
#match the original names with the sample
names(summary_stats) <- names(results)
names(summary_stats)

#gather summary statistics of each result
for (i in 1:length(results)){
  if(nrow(results[[i]]) == 0){
    summary_stats[[i]] <- summary_stats[[i]]
  } else{
    if(nrow(results[[i]]) > 1){
      summary_stats[[i]]$summary <- summary(results[[i]][,c("lng","lro","asm","afr","alr","meno")])
      summary_stats[[i]]$sm <- as.data.frame(lapply(results[[i]][,c("lng","lro","asm","afr","alr","meno")],sd,na.rm=T))
      summary_stats[[i]]$mean <- as.data.frame(lapply(results[[i]][,c("lng","lro","asm","afr","alr","meno")],mean,na.rm=T))
      summary_stats[[i]]$cv <- summary_stats[[i]]$sd / summary_stats[[i]]$mean  
      if(summary_stats[[i]]$sd$lro == 0 & summary_stats[[i]]$mean$lro == 0){
        summary_stats[[i]]$cv$lro <- 0
      } else if(nrow(results[[i]]) == 1){
        summary_stats[[i]]$summary <- summary(results[[i]][,c("lng","lro","asm","afr","alr","meno")])
        summary_stats[[i]]$sm <- as.data.frame(lapply(results[[i]][,c("lng","lro","asm","afr","alr","meno")],sd,na.rm=T))
        summary_stats[[i]]$sd[,c("lng","lro")][which(is.na(summary_stats[[i]]$sd[,c("lng","lro")]))] <- 0
        summary_stats[[i]]$mean <- as.data.frame(lapply(results[[i]][,c("lng","lro","asm","afr","alr","meno")],mean,na.rm=T))
        summary_stats[[i]]$cv <- summary_stats[[i]]$sd / summary_stats[[i]]$mean  
      }  
    }
    
  }
}

## CV sorted by life history trait ----

#lng
#create data frame to gather CVs of lng
lng_cv <- as.data.frame(matrix(NA,19,11))
colnames(lng_cv) <- c("p",1:10)
lng_cv$p <- round(seq((blockmatrix[2, 1] - 0.2), (blockmatrix[2, 1] + 0.2), length = 19), 2)
#check it
lng_cv
#extract the CV of lng across the parameter space
for(m in 1:nrow(lng_cv)){
  for(r in 2:ncol(lng_cv)){
    if(is.null(summary_stats[grep(paste("m",m,"",sep="_"),names(summary_stats))][[r-1]]$cv$lng) == T){
      lng_cv[m,r] <- NA
    }else{
      lng_cv[m,r] <- summary_stats[grep(paste("m",m,"",sep="_"),names(summary_stats))][[r-1]]$cv$lng
    }
  }
}
#check it
lng_cv

#lro
#create data frame to gather CVs of lro
lro_cv <- as.data.frame(matrix(NA,19,11))
colnames(lro_cv) <- c("p",1:10)
lro_cv$p <- round(seq((blockmatrix[2, 1] - 0.2), (blockmatrix[2, 1] + 0.2), length = 19), 2)
#check it
lro_cv
#extract the CV of lro across the parameter space
for(m in 1:nrow(lro_cv)){
  for(r in 2:ncol(lro_cv)){
    if(is.null(summary_stats[grep(paste("m",m,"",sep="_"),names(summary_stats))][[r-1]]$cv$lro) == T){
      lro_cv[m,r] <- NA
    }else{
      lro_cv[m,r] <- summary_stats[grep(paste("m",m,"",sep="_"),names(summary_stats))][[r-1]]$cv$lro
    }
  }
}
#check it
lro_cv

#asm
#create data frame to gather CVs of asm
asm_cv <- as.data.frame(matrix(NA,19,11))
colnames(asm_cv) <- c("p",1:10)
asm_cv$p <- round(seq((blockmatrix[2, 1] - 0.2), (blockmatrix[2, 1] + 0.2), length = 19), 2)
#check it
asm_cv
#extract the CV of asm across the parameter space
for(m in 1:nrow(asm_cv)){
  for(r in 2:ncol(asm_cv)){
    if(is.null(summary_stats[grep(paste("m",m,"",sep="_"),names(summary_stats))][[r-1]]$cv$asm) == T){
      asm_cv[m,r] <- NA
    }else{
      asm_cv[m,r] <- summary_stats[grep(paste("m",m,"",sep="_"),names(summary_stats))][[r-1]]$cv$asm
    }
  }
}
#check it
asm_cv

#afr
#create data frame to gather CVs of afr
afr_cv <- as.data.frame(matrix(NA,19,11))
colnames(afr_cv) <- c("p",1:10)
afr_cv$p <- round(seq((blockmatrix[2, 1] - 0.2), (blockmatrix[2, 1] + 0.2), length = 19), 2)
#check it
afr_cv
#extract the CV of afr across the parameter space
for(m in 1:nrow(afr_cv)){
  for(r in 2:ncol(afr_cv)){
    if(is.null(summary_stats[grep(paste("m",m,"",sep="_"),names(summary_stats))][[r-1]]$cv$afr) == T){
      afr_cv[m,r] <- NA
    }else{
      afr_cv[m,r] <- summary_stats[grep(paste("m",m,"",sep="_"),names(summary_stats))][[r-1]]$cv$afr
    }
  }
}
#check it
afr_cv

#alr
#create data frame to gather CVs of alr
alr_cv <- as.data.frame(matrix(NA,19,11))
colnames(alr_cv) <- c("p",1:10)
alr_cv$p <- round(seq((blockmatrix[2, 1] - 0.2), (blockmatrix[2, 1] + 0.2), length = 19), 2)
#check it
alr_cv
#extract the CV of alr across the parameter space
for(m in 1:nrow(alr_cv)){
  for(r in 2:ncol(alr_cv)){
    if(is.null(summary_stats[grep(paste("m",m,"",sep="_"),names(summary_stats))][[r-1]]$cv$alr) == T){
      alr_cv[m,r] <- NA
    }else{
      alr_cv[m,r] <- summary_stats[grep(paste("m",m,"",sep="_"),names(summary_stats))][[r-1]]$cv$alr
    }
  }
}
#check it
alr_cv

#meno
#create data frame to gather CVs of meno
meno_cv <- as.data.frame(matrix(NA,19,11))
colnames(meno_cv) <- c("p",1:10)
meno_cv$p <- round(seq((blockmatrix[2, 1] - 0.2), (blockmatrix[2, 1] + 0.2), length = 19), 2)
#check it
meno_cv
#extract the CV of meno across the parameter space
for(m in 1:nrow(meno_cv)){
  for(r in 2:ncol(meno_cv)){
    if(is.null(summary_stats[grep(paste("m",m,"",sep="_"),names(summary_stats))][[r-1]]$cv$meno) == T){
      meno_cv[m,r] <- NA
    }else{
      meno_cv[m,r] <- summary_stats[grep(paste("m",m,"",sep="_"),names(summary_stats))][[r-1]]$cv$meno
    }
  }
}
#check it
meno_cv

## Median CV per life history trait ----

#Longevity
#create data frame
lht_median <- as.data.frame(matrix(NA,length(sequence),7))
colnames(lht_median) <- c("p","lng","lro","asm","afr","alr","meno")
lht_median$p <- round(seq((blockmatrix[2, 1] - 0.2), (blockmatrix[2, 1] + 0.2), length = 19), 2)
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
#longevity
plot(c(0,max(cv_all)+0.5)~c(0.5,1),
     main="Longevity",
     xlab="Parameter value",
     ylab="CV longevity",
     ylim=c(0,max(cv_all)),
     type="n")
for(m in 1:nrow(lng_cv)){
  points(as.numeric(lng_cv[d,2:11])~rep(lng_cv[d,1],10),
         pch=16,
         col=alpha("grey",0.5)
  )    
}
points(lht_median$lng~lht_median$p,pch=16,cex=1)
#lro
plot(c(0,max(cv_all)+0.5)~c(0.5,1),
     main="Lifetime\nreproductive output",
     xlab="Parameter value",
     ylab="CV lifetime reproductive output",
     ylim=c(0,max(cv_all)),
     type="n")
for(m in 1:nrow(lro_cv)){
  points(as.numeric(lro_cv[d,2:11])~rep(lro_cv[d,1],10),
         pch=16,
         col=alpha("grey",0.5)
  )    
}
points(lht_median$lro~lht_median$p,pch=16,cex=1)
#asm
plot(c(0,max(cv_all)+0.5)~c(0.5,1),
     main="Age at\nsexual maturity",
     xlab="Parameter value",
     ylab="CV age at sexual maturity",
     ylim=c(0,max(cv_all)),
     type="n")
for(m in 1:nrow(asm_cv)){
  points(as.numeric(asm_cv[d,2:11])~rep(asm_cv[d,1],10),
         pch=16,
         col=alpha("grey",0.5)
  )    
}
points(lht_median$asm~lht_median$p,pch=16,cex=1)
#afr
plot(c(0,max(cv_all)+0.5)~c(0.5,1),
     main="Age at\nfirst reproduction",
     xlab="Parameter value",
     ylab="CV age at first reproduction",
     ylim=c(0,max(cv_all)),
     type="n")
for(m in 1:nrow(afr_cv)){
  points(as.numeric(afr_cv[d,2:11])~rep(afr_cv[d,1],10),
         pch=16,
         col=alpha("grey",0.5)
  )    
}
points(lht_median$afr~lht_median$p,pch=16,cex=1)
#alr
plot(c(0,max(cv_all)+0.5)~c(0.5,1),
     main="Age at\nlast reproduction",
     xlab="Parameter value",
     ylab="CV age at last reproduction",
     ylim=c(0,max(cv_all)),
     type="n")
for(m in 1:nrow(alr_cv)){
  points(as.numeric(alr_cv[d,2:11])~rep(alr_cv[d,1],10),
         pch=16,
         col=alpha("grey",0.5)
  )    
}
points(lht_median$alr~lht_median$p,pch=16,cex=1)
#meno
plot(c(0,max(cv_all)+0.5)~c(0.5,1),
     main="Age at\nmenopause",
     xlab="Parameter value",
     ylab="CV age at menopause",
     ylim=c(0,max(cv_all)),
     type="n")
for(m in 1:nrow(meno_cv)){
  points(as.numeric(meno_cv[d,2:11])~rep(meno_cv[d,1],10),
         pch=16,
         col=alpha("grey",0.5)
  )    
}
points(lht_median$meno~lht_median$p,pch=16,cex=1)

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
    lng_vector <- results[grep(paste("m", m, "", sep = "_"), names(results))][[r]]$lng
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

plot(c(0, 0.4) ~ c(0, 100),
     xlab = "Age", ylab = "Density",
     main = "Longevity",
     cex.main= 2,
     type = "n")

# Step 5: Loop through valid indices to calculate and plot the KDEs
for(i in 1:length(valid_indices)) {
  m <- valid_indices[i]
  data <- get(paste("m", m, sep = "_"))
  
  # Create a common grid for evaluation
  x_grid <- seq(0, 100, length.out = 200)
  
  # Calculate the KDE for each repetition and interpolate on the common grid
  kde_list <- lapply(1:nrow(data), function(j) {
    row <- data[j,1:101 ]  # Extract the j-th row (repetition)
    if (any(row > 0)) {  # Check if there are any individuals in this row
      # Create a weighted vector where age classes correspond to their counts
      ages <- rep(0:100, row)
      unique_ages <- unique(ages)  # Filter out zeros
      
      if (length(unique_ages) == 1) {
        # Case with exactly one unique non-zero age class: Create a spike density
        spike_density <- rep(0, length(x_grid))
        spike_index <- which.min(abs(x_grid - unique_ages))
        spike_density[spike_index] <- max(row)  # Set spike height proportional to the count
        return(spike_density)
        
      } else {
        # Case with multiple unique non-zero ages: calculate KDE
        kde <- density(ages, from = min(x_grid), to = max(x_grid), na.rm = TRUE, n = length(x_grid))
        return(approx(kde$x, kde$y, xout = x_grid)$y)  # Interpolate on the common grid
      }
      
    } else {
      # Case with no individuals: return a zero vector
      return(rep(0, length(x_grid)))
    }
  })
  
  # Bind the KDEs into a matrix
  kde_matrix <- do.call(rbind, kde_list)
  
  # Calculate the average KDE by averaging at each point
  avg_kde <- colMeans(kde_matrix)
  
  # Plot the average KDE with the corresponding color from the palette
  lines(x_grid, avg_kde, 
        type = "l",
        col = color_palette[i],
        lwd = 2,
        lty = i
  )
}

# Step 6: Add a legend
legend(x=105,y=0.42,
       title = "Parameter values",
       legend = sequence[valid_indices], # Use valid indices for the legend
       col = color_palette, # Use the corresponding colors from the palette
       lty = c(1:length(valid_indices)),
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
    lro_vector <- results[grep(paste("m", m, "", sep = "_"), names(results))][[r]]$lro
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

plot(c(0, 4.5) ~ c(0, 20),
     xlab = "Number of descendants", ylab = "Density",
     main = "Lifetime reproductive output",
     cex.main = 2,
     type = "n")

# Step 5: Loop through valid indices to calculate and plot the KDEs
for(i in 1:length(valid_indices)) {
  m <- valid_indices[i]
  data <- get(paste("m", m, sep = "_"))
  
  # Create a common grid for evaluation
  x_grid <- seq(0, 20, length.out = 200)
  
  # Calculate the KDE for each repetition and interpolate on the common grid
  kde_list <- lapply(1:nrow(data), function(j) {
    row <- data[j, ]  # Extract the j-th row (repetition)
    if (any(row > 0)) {  # Check if there are any individuals in this row
      # Create a weighted vector where age classes correspond to their counts
      ages <- rep(0:20, row)
      unique_ages <- unique(ages)  # Filter out zeros
      
      if (length(unique_ages) == 1) {
        # Case with exactly one unique non-zero age class: Create a spike density
        spike_density <- rep(0, length(x_grid))
        spike_index <- which.min(abs(x_grid - unique_ages))
        spike_density[spike_index] <- max(row)  # Set spike height proportional to the count
        return(spike_density)
        
      } else {
        # Case with multiple unique non-zero ages: calculate KDE
        kde <- density(ages, from = min(x_grid), to = max(x_grid), na.rm = TRUE, n = length(x_grid))
        return(approx(kde$x, kde$y, xout = x_grid)$y)  # Interpolate on the common grid
      }
      
    } else {
      # Case with no individuals: return a zero vector
      return(rep(0, length(x_grid)))
    }
  })
  
  # Bind the KDEs into a matrix
  kde_matrix <- do.call(rbind, kde_list)
  
  # Calculate the average KDE by averaging at each point
  avg_kde <- colMeans(kde_matrix)
  
  # Plot the average KDE with the corresponding color from the palette
  lines(x_grid, avg_kde, 
        type = "l",
        col = color_palette[i],
        lwd = 2,
        lty = i
  )
}

# Step 6: Add a legend
legend(x=21,y=4.7,
       title = "Parameter values",
       legend = sequence[valid_indices], # Use valid indices for the legend
       col = color_palette, # Use the corresponding colors from the palette
       lty = c(1:length(valid_indices)),
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
    asm_vector <- results[grep(paste("m", m, "", sep = "_"), names(results))][[r]]$asm
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

plot(c(0, 2) ~ c(10, 25),
     xlab = "Age", ylab = "Density",
     main = "Age at sexual maturity",
     type = "n")

# Step 5: Loop through valid indices to calculate and plot the KDEs
for(i in 1:length(valid_indices)) {
  m <- valid_indices[i]
  data <- get(paste("m", m, sep = "_"))
  
  # Create a common grid for evaluation
  x_grid <- seq(10, 25, length.out = 200)
  
  # Calculate the KDE for each repetition and interpolate on the common grid
  kde_list <- lapply(1:nrow(data), function(j) {
    row <- data[j,11:26 ]  # Extract the j-th row (repetition)
    if (any(row > 0)) {  # Check if there are any individuals in this row
      # Create a weighted vector where age classes correspond to their counts
      ages <- rep(10:25, row)
      unique_ages <- unique(ages)  # Filter out zeros
      
      if (length(unique_ages) == 1) {
        # Case with exactly one unique non-zero age class: Create a spike density
        spike_density <- rep(0, length(x_grid))
        spike_index <- which.min(abs(x_grid - unique_ages))
        spike_density[spike_index] <- max(row)  # Set spike height proportional to the count
        return(spike_density)
        
      } else {
        # Case with multiple unique non-zero ages: calculate KDE
        kde <- density(ages, from = min(x_grid), to = max(x_grid), na.rm = TRUE, n = length(x_grid))
        return(approx(kde$x, kde$y, xout = x_grid)$y)  # Interpolate on the common grid
      }
      
    } else {
      # Case with no individuals: return a zero vector
      return(rep(0, length(x_grid)))
    }
  })
  
  # Bind the KDEs into a matrix
  kde_matrix <- do.call(rbind, kde_list)
  
  # Calculate the average KDE by averaging at each point
  avg_kde <- colMeans(kde_matrix)
  
  # Plot the average KDE with the corresponding color from the palette
  lines(x_grid, avg_kde, 
        type = "l",
        col = color_palette[i],
        lwd = 2,
        lty = i
  )
}

# Step 6: Add a legend
legend(x=25.8,y=2.08,
       title = "Parameter values",
       legend = sequence[valid_indices], # Use valid indices for the legend
       col = color_palette, # Use the corresponding colors from the palette
       lty = c(1:length(valid_indices)),
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
    afr_vector <- results[grep(paste("m", m, "", sep = "_"), names(results))][[r]]$afr
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

plot(c(0, 0.25) ~ c(10, 100),
     xlab = "Age", ylab = "Density",
     main = "Age at first reproduction",
     type = "n")

# Step 5: Loop through valid indices to calculate and plot the KDEs
for(i in 1:length(valid_indices)) {
  m <- valid_indices[i]
  data <- get(paste("m", m, sep = "_"))
  
  # Create a common grid for evaluation
  x_grid <- seq(10, 100, length.out = 200)
  
  # Calculate the KDE for each repetition and interpolate on the common grid
  kde_list <- lapply(1:nrow(data), function(j) {
    row <- data[j,11:101 ]  # Extract the j-th row (repetition)
    if (any(row > 0)) {  # Check if there are any individuals in this row
      # Create a weighted vector where age classes correspond to their counts
      ages <- rep(10:100, row)
      unique_ages <- unique(ages)  # Filter out zeros
      
      if (length(unique_ages) == 1) {
        # Case with exactly one unique non-zero age class: Create a spike density
        spike_density <- rep(0, length(x_grid))
        spike_index <- which.min(abs(x_grid - unique_ages))
        spike_density[spike_index] <- max(row)  # Set spike height proportional to the count
        return(spike_density)
        
      } else {
        # Case with multiple unique non-zero ages: calculate KDE
        kde <- density(ages, from = min(x_grid), to = max(x_grid), na.rm = TRUE, n = length(x_grid))
        return(approx(kde$x, kde$y, xout = x_grid)$y)  # Interpolate on the common grid
      }
      
    } else {
      # Case with no individuals: return a zero vector
      return(rep(0, length(x_grid)))
    }
  })
  
  # Bind the KDEs into a matrix
  kde_matrix <- do.call(rbind, kde_list)
  
  # Calculate the average KDE by averaging at each point
  avg_kde <- colMeans(kde_matrix)
  
  # Plot the average KDE with the corresponding color from the palette
  lines(x_grid, avg_kde, 
        type = "l",
        col = color_palette[i],
        lwd = 2,
        lty = i
  )
}

# Step 6: Add a legend
legend(x=104,y=0.265,
       title = "Parameter values",
       legend = sequence[valid_indices], # Use valid indices for the legend
       col = color_palette, # Use the corresponding colors from the palette
       lty = c(1:length(valid_indices)),
       lwd = 2,
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
    alr_vector <- results[grep(paste("m", m, "", sep = "_"), names(results))][[r]]$alr
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

plot(c(0, 0.25) ~ c(20, 60),
     xlab = "Age", ylab = "Density",
     main = "Age at last reproduction",
     type = "n")

# Step 5: Loop through valid indices to calculate and plot the KDEs
for(i in 1:length(valid_indices)) {
  m <- valid_indices[i]
  data <- get(paste("m", m, sep = "_"))
  
  # Create a common grid for evaluation
  x_grid <- seq(20, 60, length.out = 200)
  
  # Calculate the KDE for each repetition and interpolate on the common grid
  kde_list <- lapply(1:nrow(data), function(j) {
    row <- data[j,21:61 ]  # Extract the j-th row (repetition)
    if (any(row > 0)) {  # Check if there are any individuals in this row
      # Create a weighted vector where age classes correspond to their counts
      ages <- rep(20:60, row)
      unique_ages <- unique(ages)  # Filter out zeros
      
      if (length(unique_ages) == 1) {
        # Case with exactly one unique non-zero age class: Create a spike density
        spike_density <- rep(0, length(x_grid))
        spike_index <- which.min(abs(x_grid - unique_ages))
        spike_density[spike_index] <- max(row)  # Set spike height proportional to the count
        return(spike_density)
        
      } else {
        # Case with multiple unique non-zero ages: calculate KDE
        kde <- density(ages, from = min(x_grid), to = max(x_grid), na.rm = TRUE, n = length(x_grid))
        return(approx(kde$x, kde$y, xout = x_grid)$y)  # Interpolate on the common grid
      }
      
    } else {
      # Case with no individuals: return a zero vector
      return(rep(0, length(x_grid)))
    }
  })
  
  # Bind the KDEs into a matrix
  kde_matrix <- do.call(rbind, kde_list)
  
  # Calculate the average KDE by averaging at each point
  avg_kde <- colMeans(kde_matrix)
  
  # Plot the average KDE with the corresponding color from the palette
  lines(x_grid, avg_kde, 
        type = "l",
        col = color_palette[i],
        lwd = 2,
        lty = i
  )
}

# Step 6: Add a legend
legend(x=62,y=0.26,
       title = "Parameter values",
       legend = sequence[valid_indices], # Use valid indices for the legend
       col = color_palette, # Use the corresponding colors from the palette
       lty = c(1:length(valid_indices)),
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
    meno_vector <- results[grep(paste("m", m, "", sep = "_"), names(results))][[r]]$meno
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

plot(c(0, 0.25) ~ c(20, 100),
     xlab = "Age", ylab = "Density",
     main = "Age at menopause",
     type = "n")

# Step 5: Loop through valid indices to calculate and plot the KDEs
for(i in 1:length(valid_indices)) {
  m <- valid_indices[i]
  data <- get(paste("m", m, sep = "_"))
  
  # Create a common grid for evaluation
  x_grid <- seq(20, 100, length.out = 200)
  
  # Calculate the KDE for each repetition and interpolate on the common grid
  kde_list <- lapply(1:nrow(data), function(j) {
    row <- data[j,21:101 ]  # Extract the j-th row (repetition)
    if (any(row > 0)) {  # Check if there are any individuals in this row
      # Create a weighted vector where age classes correspond to their counts
      ages <- rep(20:100, row)
      unique_ages <- unique(ages)  # Filter out zeros
      
      if (length(unique_ages) == 1) {
        # Case with exactly one unique non-zero age class: Create a spike density
        spike_density <- rep(0, length(x_grid))
        spike_index <- which.min(abs(x_grid - unique_ages))
        spike_density[spike_index] <- max(row)  # Set spike height proportional to the count
        return(spike_density)
        
      } else {
        # Case with multiple unique non-zero ages: calculate KDE
        kde <- density(ages, from = min(x_grid), to = max(x_grid), na.rm = TRUE, n = length(x_grid))
        return(approx(kde$x, kde$y, xout = x_grid)$y)  # Interpolate on the common grid
      }
      
    } else {
      # Case with no individuals: return a zero vector
      return(rep(0, length(x_grid)))
    }
  })
  
  # Bind the KDEs into a matrix
  kde_matrix <- do.call(rbind, kde_list)
  
  # Calculate the average KDE by averaging at each point
  avg_kde <- colMeans(kde_matrix)
  
  # Plot the average KDE with the corresponding color from the palette
  lines(x_grid, avg_kde, 
        type = "l",
        col = color_palette[i],
        lwd = 2,
        lty = i
  )
}

# Step 6: Add a legend
legend(x=104,y=0.265,
       title = "Parameter values",
       legend = sequence[valid_indices], # Use valid indices for the legend
       col = color_palette, # Use the corresponding colors from the palette
       lty = c(1:length(valid_indices)),
       lwd = 2,
       bty = "n",
       xpd=T)
