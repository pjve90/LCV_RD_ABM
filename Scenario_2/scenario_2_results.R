# Life cycle variation and resource dynamics ABM: Scenario 1 - results ----

#Here is the code to get the summary statistics and plots from the simulation of Scenario 1, which aims to understand the influence of resource production on the variability of life cycles.

#Production: parameter sweep between 0.1 and 0.9 (17 values)
#Transfers: null
#Habitat quality: baseline

#R settings ----

#set work directory
getwd()
setwd("./LCV_RD_ABM")

#install packages
#install.packages("scales")
library(scales)

#Data import ----

#import raw results from the simulation
results <- readRDS("./Scenario_2/lht_list.RData")

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
  }
}

#Plot it! ----

#get the parameter values for labeling

#source the functions
#Stage-specific maximum amount of resource production 
source("~/LCV_RD_ABM/Model_code/production_maxprod_fx.R")
#Stage-specific production probabilities
source("~/LCV_RD_ABM/Model_code/production_prodprob_fx.R")
#get the values
#habitat quality
habitat_quality <- 4
#stage-specific maximum amount of resource production.
maxprod <- max_production(habitat_quality)
#maximum production probability
max_prod_prob <- seq(0.1,0.9,length=17)
#stage-specific production probabilities.
prod_prob <- production_prob(max_prod_prob)

#Each parameter value separately ----

#Longevity ----

par(mfrow=c(4,5))

for(d in 1:17){
  plot(c(0,100),c(0,600),
       type = "n",
       main=paste("Parameter value",prod_prob[2,d]),
       xlab="Longevity",
       ylab="Frequency")
  for(r in 1:10){
    if(nrow(results[grep(paste("d",d,"",sep="_"),names(results))][[r]]) > 0)
      points(as.numeric(names(table(results[grep(paste("d",d,"",sep="_"),names(results))][[r]]$lng))),
             table(results[grep(paste("d",d,"",sep="_"),names(results))][[r]]$lng),
             col=alpha(hcl.colors(10,palette = "Zissou 1")[r],0.5),
             pch=16)
  }
}

#Lifetime reproductive output ----

par(mfrow=c(4,5))

for(d in 1:17){
  plot(c(0,20),c(0,600),
       type = "n",
       main=paste("Parameter value",prod_prob[2,d]),
       xlab="Lifetime reproductive output",
       ylab="Frequency")
  for(r in 1:10){
    if(nrow(results[grep(paste("d",d,"",sep="_"),names(results))][[r]]) > 0)
      points(as.numeric(names(table(results[grep(paste("d",d,"",sep="_"),names(results))][[r]]$lro))),
             table(results[grep(paste("d",d,"",sep="_"),names(results))][[r]]$lro),
             col=alpha(hcl.colors(10,palette = "Zissou 1")[r],0.5),
             pch=16)
  }
}

#Age at sexual maturity ----


par(mfrow=c(4,5))

for(d in 1:17){
  plot(c(0,100),c(0,600),
       type = "n",
       main=paste("Parameter value",prod_prob[2,d]),
       xlab="Age sexual maturity",
       ylab="Frequency")
  for(r in 1:10){
    if(nrow(results[grep(paste("d",d,"",sep="_"),names(results))][[r]]) > 0)
      points(as.numeric(names(table(results[grep(paste("d",d,"",sep="_"),names(results))][[r]]$asm))),
             table(results[grep(paste("d",d,"",sep="_"),names(results))][[r]]$asm),
             col=alpha(hcl.colors(10,palette = "Zissou 1")[r],0.5),
             pch=16)
  }
}

#Age at first reproduction ----


par(mfrow=c(4,5))

for(d in 1:17){
  plot(c(0,100),c(0,600),
       type = "n",
       main=paste("Parameter value",prod_prob[2,d]),
       xlab="Age first reproduction",
       ylab="Frequency")
  for(r in 1:10){
    if(nrow(results[grep(paste("d",d,"",sep="_"),names(results))][[r]] > 0))
      points(as.numeric(names(table(results[grep(paste("d",d,"",sep="_"),names(results))][[r]]$afr))),
             table(results[grep(paste("d",d,"",sep="_"),names(results))][[r]]$afr),
             col=alpha(hcl.colors(10,palette = "Zissou 1")[r],0.5),
             pch=16)
  }
}

#Age at last reproduction ----


par(mfrow=c(4,5))

for(d in 1:17){
  plot(c(0,100),c(0,600),
       type = "n",
       main=paste("Parameter value",prod_prob[2,d]),
       xlab="Age last reproduction",
       ylab="Frequency")
  for(r in 1:10){
    if(nrow(results[grep(paste("d",d,"",sep="_"),names(results))][[r]] > 0))
      points(as.numeric(names(table(results[grep(paste("d",d,"",sep="_"),names(results))][[r]]$alr))),
             table(results[grep(paste("d",d,"",sep="_"),names(results))][[r]]$alr),
             col=alpha(hcl.colors(10,palette = "Zissou 1")[r],0.5),
             pch=16)
  }
}

#Age at menopause ----


par(mfrow=c(4,5))

for(d in 1:17){
  plot(c(0,100),c(0,600),
       type = "n",
       main=paste("Parameter value",prod_prob[2,d]),
       xlab="Age menopause",
       ylab="Frequency")
  for(r in 1:10){
    if(nrow(results[grep(paste("d",d,"",sep="_"),names(results))][[r]] > 0))
      points(as.numeric(names(table(results[grep(paste("d",d,"",sep="_"),names(results))][[r]]$meno))),
             table(results[grep(paste("d",d,"",sep="_"),names(results))][[r]]$meno),
             col=alpha(hcl.colors(10,palette = "Zissou 1")[r],0.5),
             pch=16)
  }
}

# Compare parameter values ----

#Longevity ----

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

#plot the averages

#set margins
par(mfrow=c(1,1), mar=c(5, 4, 4, 8))

plot(c(0,100),c(0,200),
     type="n",
     main="Longevity",
     xlab="Longevity",
     ylab="Frequency"
)

# Step 1: Create a vector to store indices of data frames with positive longevity
valid_indices <- c()

# Step 2: First pass through to identify valid data frames
for(d in 1:17){
  # Check if the sum of means is greater than zero
  if(sum(apply(get(paste("d", d, sep = "_")), 2, mean)) > 0){
    valid_indices <- c(valid_indices, d)  # Store the valid index
  }
}

# Step 3: Create a color palette for only the valid data frames
color_palette <- hcl.colors(length(valid_indices), palette="zissou 1", alpha=0.5)

# Step 4: Second pass through to plot the valid data frames
for(i in 1:length(valid_indices)){
  d <- valid_indices[i]
  data <- get(paste("d", d, sep = "_"))
  
  # Plot the line for this valid data frame
  lines(0:(ncol(get(paste("d", d, sep = "_")))-1),
        apply(data, 2, mean),
        type="o",
        col=color_palette[i],
        pch=16
  )  
}

# Step 5: Adjust the legend to match the valid data frames
legend(x=105,y=208,
       legend=prod_prob[2,valid_indices],  # Use valid indices for the legend
       col=color_palette,     # Use the corresponding colors from the palette
       lty=1,
       pch=16,
       xpd=T)

#Lifetime reproductive output ----

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
    #extract the longevity
    lro_vector <- results[grep(paste("d", d, "", sep = "_"), names(results))][[r]]$lro
    #create a new row with zeros
    new_row <- rep(0, 21)
    #update the new row based on longevity values
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

#plot the averages

#set margins
par(mfrow=c(1,1), mar=c(5, 4, 4, 8))

plot(c(0,20),c(0,1500),
     type="n",
     main="Lifetime reproductive output",
     xlab="Lifetime reproductive output",
     ylab="Frequency"
)

# Step 1: Create a vector to store indices of data frames with positive longevity
valid_indices <- c()

# Step 2: First pass through to identify valid data frames
for(d in 1:17){
  # Check if the sum of means is greater than zero
  if(sum(apply(get(paste("d", d, sep = "_")), 2, mean)) > 0){
    valid_indices <- c(valid_indices, d)  # Store the valid index
  }
}

# Step 3: Create a color palette for only the valid data frames
color_palette <- hcl.colors(length(valid_indices), palette="zissou 1", alpha=0.5)

# Step 4: Second pass through to plot the valid data frames
for(i in 1:length(valid_indices)){
  d <- valid_indices[i]
  data <- get(paste("d", d, sep = "_"))
  
  # Plot the line for this valid data frame
  lines(0:(ncol(get(paste("d", d, sep = "_")))-1),
        apply(data, 2, mean),
        type="o",
        col=color_palette[i],
        pch=16
  )  
}

# Step 5: Adjust the legend to match the valid data frames
legend(x=21,y=1560,
       legend=prod_prob[2,valid_indices],  # Use valid indices for the legend
       col=color_palette,     # Use the corresponding colors from the palette
       lty=1,
       pch=16,
       xpd=T)

#Age at sexual maturity ----

#get the frequency of each age across parameter values and repetitions
for (d in 1:17) {
  # create a data frame for each parameter value
  assign(paste("d", d, sep = "_"), data.frame(matrix(0, nrow = 0, ncol = 26)))
  # retrieve the data frame
  temp_df <- get(paste("d", d, sep = "_"))
  # modify the column names
  colnames(temp_df) <- as.character(0:25)
  # reassign the modified data frame in the original data frame
  assign(paste("d", d, sep = "_"), temp_df)
  #loop through repetitions
  for (r in 1:10) {
    #extract the longevity
    asm_vector <- results[grep(paste("d", d, "", sep = "_"), names(results))][[r]]$asm
    #create a new row with zeros
    new_row <- rep(0, 26)
    #update the new row based on longevity values
    for (val in asm_vector) {
      if (val >= 0 & val <= 25 & !is.na(val)) {
        new_row[val + 1] <- new_row[val + 1] + 1  # +1 to account for index starting at 1 in R
      }
    }
    #change column names to match
    new_row_df <- as.data.frame(t(new_row))
    colnames(new_row_df) <- as.character(0:25)
    #add new row into the data frame
    temp_df <- get(paste("d", d, sep = "_"))  #current data frame
    temp_df <- rbind(temp_df, new_row_df)  #new row
    assign(paste("d", d, sep = "_"), temp_df)  #updated data frame
  }
}

#plot the averages

#set margins
par(mfrow=c(1,1), mar=c(5, 4, 4, 8))

plot(c(0,25),c(0,300),
     type="n",
     main="Age at sexual maturity",
     xlab="Age at sexual maturity",
     ylab="Frequency"
)

# Step 1: Create a vector to store indices of data frames with positive longevity
valid_indices <- c()

# Step 2: First pass through to identify valid data frames
for(d in 1:17){
  # Check if the sum of means is greater than zero
  if(sum(apply(get(paste("d", d, sep = "_")), 2, mean)) > 0){
    valid_indices <- c(valid_indices, d)  # Store the valid index
  }
}

# Step 3: Create a color palette for only the valid data frames
color_palette <- hcl.colors(length(valid_indices), palette="zissou 1", alpha=0.5)

# Step 4: Second pass through to plot the valid data frames
for(i in 1:length(valid_indices)){
  d <- valid_indices[i]
  data <- get(paste("d", d, sep = "_"))
  
  # Plot the line for this valid data frame
  lines(0:(ncol(get(paste("d", d, sep = "_")))-1),
        apply(data, 2, mean),
        type="o",
        col=color_palette[i],
        pch=16
  )  
}

# Step 5: Adjust the legend to match the valid data frames
legend(x=27,y=312,
       legend=prod_prob[2,valid_indices],  # Use valid indices for the legend
       col=color_palette,     # Use the corresponding colors from the palette
       lty=1,
       pch=16,
       xpd=T)

#Age at first reproduction ----

#get the frequency of each age across parameter values and repetitions
for (d in 1:17) {
  # create a data frame for each parameter value
  assign(paste("d", d, sep = "_"), data.frame(matrix(0, nrow = 0, ncol = 36)))
  # retrieve the data frame
  temp_df <- get(paste("d", d, sep = "_"))
  # modify the column names
  colnames(temp_df) <- as.character(0:35)
  # reassign the modified data frame in the original data frame
  assign(paste("d", d, sep = "_"), temp_df)
  #loop through repetitions
  for (r in 1:10) {
    #extract the longevity
    afr_vector <- results[grep(paste("d", d, "", sep = "_"), names(results))][[r]]$afr
    #create a new row with zeros
    new_row <- rep(0, 36)
    #update the new row based on longevity values
    for (val in afr_vector) {
      if (val >= 0 & val <= 35 & !is.na(val)) {
        new_row[val + 1] <- new_row[val + 1] + 1  # +1 to account for index starting at 1 in R
      }
    }
    #change column names to match
    new_row_df <- as.data.frame(t(new_row))
    colnames(new_row_df) <- as.character(0:35)
    #add new row into the data frame
    temp_df <- get(paste("d", d, sep = "_"))  #current data frame
    temp_df <- rbind(temp_df, new_row_df)  #new row
    assign(paste("d", d, sep = "_"), temp_df)  #updated data frame
  }
}

#plot the averages

#set margins
par(mfrow=c(1,1), mar=c(5, 4, 4, 8))

plot(c(0,35),c(0,200),
     type="n",
     main="Age at first reproduction",
     xlab="Age at first reproduction",
     ylab="Frequency"
)

# Step 1: Create a vector to store indices of data frames with positive longevity
valid_indices <- c()

# Step 2: First pass through to identify valid data frames
for(d in 1:17){
  # Check if the sum of means is greater than zero
  if(sum(apply(get(paste("d", d, sep = "_")), 2, mean)) > 0){
    valid_indices <- c(valid_indices, d)  # Store the valid index
  }
}

# Step 3: Create a color palette for only the valid data frames
color_palette <- hcl.colors(length(valid_indices), palette="zissou 1", alpha=0.5)

# Step 4: Second pass through to plot the valid data frames
for(i in 1:length(valid_indices)){
  d <- valid_indices[i]
  data <- get(paste("d", d, sep = "_"))
  
  # Plot the line for this valid data frame
  lines(0:(ncol(get(paste("d", d, sep = "_")))-1),
        apply(data, 2, mean),
        type="o",
        col=color_palette[i],
        pch=16
  )  
}

# Step 5: Adjust the legend to match the valid data frames
legend(x=37,y=208,
       legend=prod_prob[2,valid_indices],  # Use valid indices for the legend
       col=color_palette,     # Use the corresponding colors from the palette
       lty=1,
       pch=16,
       xpd=T)

#Age at last reproduction ----

#get the frequency of each age across parameter values and repetitions
for (d in 1:17) {
  # create a data frame for each parameter value
  assign(paste("d", d, sep = "_"), data.frame(matrix(0, nrow = 0, ncol = 61)))
  # retrieve the data frame
  temp_df <- get(paste("d", d, sep = "_"))
  # modify the column names
  colnames(temp_df) <- as.character(0:60)
  # reassign the modified data frame in the original data frame
  assign(paste("d", d, sep = "_"), temp_df)
  #loop through repetitions
  for (r in 1:10) {
    #extract the longevity
    alr_vector <- results[grep(paste("d", d, "", sep = "_"), names(results))][[r]]$alr
    #create a new row with zeros
    new_row <- rep(0, 61)
    #update the new row based on longevity values
    for (val in alr_vector) {
      if (val >= 0 & val <= 60 & !is.na(val)) {
        new_row[val + 1] <- new_row[val + 1] + 1  # +1 to account for index starting at 1 in R
      }
    }
    #change column names to match
    new_row_df <- as.data.frame(t(new_row))
    colnames(new_row_df) <- as.character(0:60)
    #add new row into the data frame
    temp_df <- get(paste("d", d, sep = "_"))  #current data frame
    temp_df <- rbind(temp_df, new_row_df)  #new row
    assign(paste("d", d, sep = "_"), temp_df)  #updated data frame
  }
}

#plot the averages

#set margins
par(mfrow=c(1,1), mar=c(5, 4, 4, 8))

plot(c(0,60),c(0,100),
     type="n",
     main="Age at last reproduction",
     xlab="Age at last reproduction",
     ylab="Frequency"
)

# Step 1: Create a vector to store indices of data frames with positive longevity
valid_indices <- c()

# Step 2: First pass through to identify valid data frames
for(d in 1:17){
  # Check if the sum of means is greater than zero
  if(sum(apply(get(paste("d", d, sep = "_")), 2, mean)) > 0){
    valid_indices <- c(valid_indices, d)  # Store the valid index
  }
}

# Step 3: Create a color palette for only the valid data frames
color_palette <- hcl.colors(length(valid_indices), palette="zissou 1", alpha=0.5)

# Step 4: Second pass through to plot the valid data frames
for(i in 1:length(valid_indices)){
  d <- valid_indices[i]
  data <- get(paste("d", d, sep = "_"))
  
  # Plot the line for this valid data frame
  lines(0:(ncol(get(paste("d", d, sep = "_")))-1),
        apply(data, 2, mean),
        type="o",
        col=color_palette[i],
        pch=16
  )  
}

# Step 5: Adjust the legend to match the valid data frames
legend(x=63,y=104,
       legend=prod_prob[2,valid_indices],  # Use valid indices for the legend
       col=color_palette,     # Use the corresponding colors from the palette
       lty=1,
       pch=16,
       xpd=T)

#Age at menopause ----

#get the frequency of each age across parameter values and repetitions
for (d in 1:17) {
  # create a data frame for each parameter value
  assign(paste("d", d, sep = "_"), data.frame(matrix(0, nrow = 0, ncol = 71)))
  # retrieve the data frame
  temp_df <- get(paste("d", d, sep = "_"))
  # modify the column names
  colnames(temp_df) <- as.character(0:70)
  # reassign the modified data frame in the original data frame
  assign(paste("d", d, sep = "_"), temp_df)
  #loop through repetitions
  for (r in 1:10) {
    #extract the longevity
    meno_vector <- results[grep(paste("d", d, "", sep = "_"), names(results))][[r]]$meno
    #create a new row with zeros
    new_row <- rep(0, 71)
    #update the new row based on longevity values
    for (val in meno_vector) {
      if (val >= 0 & val <= 70 & !is.na(val)) {
        new_row[val + 1] <- new_row[val + 1] + 1  # +1 to account for index starting at 1 in R
      }
    }
    #change column names to match
    new_row_df <- as.data.frame(t(new_row))
    colnames(new_row_df) <- as.character(0:70)
    #add new row into the data frame
    temp_df <- get(paste("d", d, sep = "_"))  #current data frame
    temp_df <- rbind(temp_df, new_row_df)  #new row
    assign(paste("d", d, sep = "_"), temp_df)  #updated data frame
  }
}

#plot the averages

#set margins
par(mfrow=c(1,1), mar=c(5, 4, 4, 8))

plot(c(0,70),c(0,300),
     type="n",
     main="Age at menopause",
     xlab="Age at menopause",
     ylab="Frequency"
)

# Step 1: Create a vector to store indices of data frames with positive longevity
valid_indices <- c()

# Step 2: First pass through to identify valid data frames
for(d in 1:17){
  # Check if the sum of means is greater than zero
  if(sum(apply(get(paste("d", d, sep = "_")), 2, mean)) > 0){
    valid_indices <- c(valid_indices, d)  # Store the valid index
  }
}

# Step 3: Create a color palette for only the valid data frames
color_palette <- hcl.colors(length(valid_indices), palette="zissou 1", alpha=0.5)

# Step 4: Second pass through to plot the valid data frames
for(i in 1:length(valid_indices)){
  d <- valid_indices[i]
  data <- get(paste("d", d, sep = "_"))
  
  # Plot the line for this valid data frame
  lines(0:(ncol(get(paste("d", d, sep = "_")))-1),
        apply(data, 2, mean),
        type="o",
        col=color_palette[i],
        pch=16
  )  
}

# Step 5: Adjust the legend to match the valid data frames
legend(x=75,y=312,
       legend=prod_prob[2,valid_indices],  # Use valid indices for the legend
       col=color_palette,     # Use the corresponding colors from the palette
       lty=1,
       pch=16,
       xpd=T)

