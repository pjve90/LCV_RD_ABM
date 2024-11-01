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
results <- readRDS("./Scenario 1/lht_list_s1.RData")

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
    if(summary_stats[[i]]$sd$lro == 0 & summary_stats[[i]]$mean$lro == 0){
      summary_stats[[i]]$cv$lro <- 0
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
par(mfrow=c(1,6))
layout(matrix(c(1,1,2,2,3,4,5,6),ncol=4,byrow=T))
#longevity
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
#lro
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
#asm
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
#afr
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
#alr
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
#meno
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

