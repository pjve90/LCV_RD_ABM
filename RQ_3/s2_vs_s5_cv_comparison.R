# Life cycle variation and resource dynamics ABM: Scenario 2 - results_s2 ----

#Here is the code to get the summary statistics and plots from the simulation of Scenario 2, which aims to understand the influence of resource production on the variability of life cycles.

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

#import data ----

#Scenario 2
lht_cv_s2 <- readRDS("./RQ_3/lht_cv_s2.RData")

#scenario 5
#cv per life history trait
lng_cv_s5 <- readRDS("./RQ_3/lng_cv_s5.RData")
lro_cv_s5 <- readRDS("./RQ_3/lro_cv_s5.RData")
asm_cv_s5 <- readRDS("./RQ_3/asm_cv_s5.RData")
afr_cv_s5 <- readRDS("./RQ_3/afr_cv_s5.RData")
alr_cv_s5 <- readRDS("./RQ_3/alr_cv_s5.RData")
meno_cv_s5 <- readRDS("./RQ_3/meno_cv_s5.RData")

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

#values for labeling
transf_prob <- round(seq((blockmatrix[2, 4] - 0.2), (blockmatrix[2, 4] + 0.2), length = 19), 2)

## Mean CV per life history trait ----

#Scenario 5
#create data frame
lht_mean_s5 <- as.data.frame(matrix(NA,length(transf_prob),7))
colnames(lht_mean_s5) <- c("m","lng","lro","asm","afr","alr","meno")
lht_mean_s5$m <- transf_prob
lht_mean_s5
#calculate the mean CV per life history trait
for(i in 1:nrow(lht_mean_s5)){
  lht_mean_s5[i,"lng"] <- ifelse(sum(is.na(lng_cv_s5[i,2:ncol(lng_cv_s5)]))==10,NA,mean(as.numeric(lng_cv_s5[i,2:ncol(lng_cv_s5)]),na.rm=T))
  lht_mean_s5[i,"lro"] <- ifelse(sum(is.na(lro_cv_s5[i,2:ncol(lro_cv_s5)]))==10,NA,mean(as.numeric(lro_cv_s5[i,2:ncol(lro_cv_s5)]),na.rm=T))
  lht_mean_s5[i,"asm"] <- ifelse(sum(is.na(asm_cv_s5[i,2:ncol(asm_cv_s5)]))==10,NA,mean(as.numeric(asm_cv_s5[i,2:ncol(asm_cv_s5)]),na.rm=T))
  lht_mean_s5[i,"afr"] <- ifelse(sum(is.na(afr_cv_s5[i,2:ncol(afr_cv_s5)]))==10,NA,mean(as.numeric(afr_cv_s5[i,2:ncol(afr_cv_s5)]),na.rm=T))
  lht_mean_s5[i,"alr"] <- ifelse(sum(is.na(alr_cv_s5[i,2:ncol(alr_cv_s5)]))==10,NA,mean(as.numeric(alr_cv_s5[i,2:ncol(alr_cv_s5)]),na.rm=T))
  lht_mean_s5[i,"meno"] <- ifelse(sum(is.na(meno_cv_s5[i,2:ncol(meno_cv_s5)]))==10,NA,mean(as.numeric(meno_cv_s5[i,2:ncol(meno_cv_s5)]),na.rm=T))
}
lht_mean_s5

#Plot it! ----

##Median all together ----

#get the maximum CV of all the life history traits for the scale in the plots

#scenario 2
#get the maximum CV of all the life history traits for the scale in the plots
# Get parameter ranges
r_values <- 1:10

# Directory for summary files
summary_dir <- "./RQ_3/lht_summaries_s2"

# Initialize an empty vector to store CV values
cv_all <- c()

# Loop through all combinations of d and r
for (r in r_values) {
  # Construct the file name for the corresponding summary file
  summary_file <- file.path(summary_dir, sprintf("summary_lht_list_r%d.fst", r))
  
  # Check if the summary file exists
  if (file.exists(summary_file)) {
    # Read the summary file
    summary_data <- read_fst(summary_file)
    
    # Extract CV values for all traits if they exist
    cv_values <- summary_data[summary_data$stat == "cv", -1] # Exclude the "stat" column
    
    # Flatten the CV values and remove NAs
    if (!is.null(cv_values)) {
      cv_all <- c(cv_all, as.numeric(unlist(cv_values)))
    }
  } else {
    # Log a warning if the file does not exist
    warning(sprintf("Summary file not found: %s", summary_file))
  }
}


# Remove NA values from the CV vector
cv_all_s2 <- cv_all[!is.na(cv_all)]

#scenario 5
# Get parameter ranges
m_values <- 1:19
r_values <- 1:10

# Directories for summary files
summary_dir_s5 <- "./RQ_3/lht_summaries_s5"

# Initialize an empty vector to store CV values for both scenarios
cv_all <- c()

# Helper function to process CVs for a given scenario directory
process_cv <- function(summary_dir) {
  # Initialize a vector to store CV values
  cv_values <- c()
  
  # Loop through all combinations of d and r
  for (m in m_values) {
    for (r in r_values) {
      # Construct the file name for the corresponding summary file
      summary_file <- file.path(summary_dir, sprintf("summary_lht_list_m%d_r%d.fst", m, r))
      
      # Check if the summary file exists
      if (file.exists(summary_file)) {
        # Read the summary file
        summary_data <- read_fst(summary_file)
        
        # Extract CV values for all traits if they exist
        cv <- summary_data[summary_data$stat == "cv", -1] # Exclude the "stat" column
        
        # Flatten the CV values and remove NAs
        if (!is.null(cv)) {
          cv_values <- c(cv_values, as.numeric(unlist(cv)))
        }
      } else {
        # Log a warning if the file does not exist
        warning(sprintf("Summary file not found: %s", summary_file))
      }
    }
  }
  
  # Return the CV values after removing NAs
  cv_values[!is.na(cv_values)]
}

# Process CV for  and combine the results
cv_all_s5 <- process_cv(summary_dir_s5)

#combine CVs
cv_all <- c(cv_all_s2,cv_all_s5)

# Get the maximum CV value
max_cv <- max(cv_all, na.rm = TRUE)

# Print the maximum CV
max_cv

#plot the CV together
par(mar = c(5, 4, 4, 4))  # Add space on the right side
layout(matrix(c(1, 1, 2, 2, 3, 4, 5, 6,7,7,7,7), ncol = 4, byrow = TRUE), heights = c(1, 1, 0.1))

#colour palette
palette <- hcl.colors(10,"ag_sunset")[c(3,8)]

# Define labels for each plot
plot_labels <- c("A)", "B)", "C)", "D)", "E)", "F)")

### Longevity ----

plot(c(0,max(cv_all)+0.5)~c(0,1),
     main="Longevity",
     xlab="Transfer probability",
     ylab="CV longevity",
     ylim=c(0,max(cv_all)),
     type="n")
mtext(plot_labels[1], side=3, adj=0, line=2, font=2) # Add bold (a)
#Scenario 2
#average
points(0,
       mean(lht_cv_s2$lng,na.rm=T),
       pch=1,
       cex=1.25,
       col=palette[1])
#lines
segments(x0 = 0,
         y0 = min(lht_cv_s2$lng,na.rm=T),
         x1 = 0,
         y1 = max(lht_cv_s2$lng,na.rm=T), 
         col = alpha(palette[1],0.5),
         lwd=1
    )

#Scenario 5
#average
points(lht_mean_s5$m,
       lht_mean_s5$lng,
       pch=2,
       cex=1.25,
       col=palette[2])
#lines
for(m in 1:nrow(lng_cv_s5)){
  if(sum(is.na(lng_cv_s5[m,2:11])) < 10){
    segments(x0 = lng_cv_s5[m,1],
             y0 = min(lng_cv_s5[m,2:11],na.rm=T),
             x1 = lng_cv_s5[m,1],
             y1 = max(lng_cv_s5[m,2:11],na.rm=T), 
             col = alpha(palette[2],0.5),
             lwd=1
    )
  }
}

### Lifetime reproductive output ----

plot(c(0,max(cv_all)+0.5)~c(0,1),
     main="Lifetime\nreproductive output",
     xlab="Transfer probability",
     ylab="CV lifetime reproductive output",
     ylim=c(0,max(cv_all)),
     type="n")
mtext(plot_labels[2], side=3, adj=0, line=2, font=2) # Add bold (a)
#Scenario 2
#average
points(0,
       mean(lht_cv_s2$lro,na.rm=T),
       pch=1,
       cex=1.25,
       col=palette[1])
#lines
segments(x0 = 0,
         y0 = min(lht_cv_s2$lro,na.rm=T),
         x1 = 0,
         y1 = max(lht_cv_s2$lro,na.rm=T), 
         col = alpha(palette[1],0.5),
         lwd=1
)

#Scenario 5
#average
points(lht_mean_s5$m,
       lht_mean_s5$lro,
       pch=2,
       cex=1.25,
       col=palette[2])
#lines
for(m in 1:nrow(lro_cv_s5)){
  if(sum(is.na(lro_cv_s5[m,2:11])) < 10){
    segments(x0 = lro_cv_s5[m,1],
             y0 = min(lro_cv_s5[m,2:11],na.rm=T),
             x1 = lro_cv_s5[m,1],
             y1 = max(lro_cv_s5[m,2:11],na.rm=T), 
             col = alpha(palette[2],0.5),
             lwd=1
    )
  }
}

### Age at sexual maturity ----

plot(c(0,max(cv_all)+0.5)~c(0,1),
     main="Age at\nsexual maturity",
     xlab="Transfer probability",
     ylab="CV age at sexual maturity",
     ylim=c(0,max(cv_all)),
     type="n")
mtext(plot_labels[3], side=3, adj=0, line=2, font=2) # Add bold (a)
#Scenario 2
#average
points(0,
       mean(lht_cv_s2$asm,na.rm=T),
       pch=1,
       cex=1.25,
       col=palette[1])
#lines
segments(x0 = 0,
         y0 = min(lht_cv_s2$asm,na.rm=T),
         x1 = 0,
         y1 = max(lht_cv_s2$asm,na.rm=T), 
         col = alpha(palette[1],0.5),
         lwd=1
)

#Scenario 5
#average
points(lht_mean_s5$m,
       lht_mean_s5$asm,
       pch=2,
       cex=1.25,
       col=palette[2])
#lines
for(m in 1:nrow(asm_cv_s5)){
  if(sum(is.na(asm_cv_s5[m,2:11])) < 10){
    segments(x0 = asm_cv_s5[m,1],
             y0 = min(asm_cv_s5[m,2:11],na.rm=T),
             x1 = asm_cv_s5[m,1],
             y1 = max(asm_cv_s5[m,2:11],na.rm=T), 
             col = alpha(palette[2],0.5),
             lwd=1
    )
  }
}

### Age at first reproduction ----

plot(c(0,max(cv_all)+0.5)~c(0,1),
     main="Age at\nfirst reproduction",
     xlab="Transfer probability",
     ylab="CV age at first reproduction",
     ylim=c(0,max(cv_all)),
     type="n")
mtext(plot_labels[4], side=3, adj=0, line=2, font=2) # Add bold (a)
#Scenario 2
#average
points(0,
       mean(lht_cv_s2$afr,na.rm=T),
       pch=1,
       cex=1.25,
       col=palette[1])
#lines
segments(x0 = 0,
         y0 = min(lht_cv_s2$afr,na.rm=T),
         x1 = 0,
         y1 = max(lht_cv_s2$afr,na.rm=T), 
         col = alpha(palette[1],0.5),
         lwd=1
)

#Scenario 5
#average
points(lht_mean_s5$m,
       lht_mean_s5$afr,
       pch=2,
       cex=1.25,
       col=palette[2])
#lines
for(m in 1:nrow(afr_cv_s5)){
  if(sum(is.na(afr_cv_s5[m,2:11])) < 10){
    segments(x0 = afr_cv_s5[m,1],
             y0 = min(afr_cv_s5[m,2:11],na.rm=T),
             x1 = afr_cv_s5[m,1],
             y1 = max(afr_cv_s5[m,2:11],na.rm=T), 
             col = alpha(palette[2],0.5),
             lwd=1
    )
  }
}

### Age at last reproduction ----

plot(c(0,max(cv_all)+0.5)~c(0,1),
     main="Age at\nlast reproduction",
     xlab="Transfer probability",
     ylab="CV age at last reproduction",
     ylim=c(0,max(cv_all)),
     type="n")
mtext(plot_labels[5], side=3, adj=0, line=2, font=2) # Add bold (a)
#Scenario 2
#average
points(0,
       mean(lht_cv_s2$alr,na.rm=T),
       pch=1,
       cex=1.25,
       col=palette[1])
#lines
segments(x0 = 0,
         y0 = min(lht_cv_s2$alr,na.rm=T),
         x1 = 0,
         y1 = max(lht_cv_s2$alr,na.rm=T), 
         col = alpha(palette[1],0.5),
         lwd=1
)

#Scenario 5
#average
points(lht_mean_s5$m,
       lht_mean_s5$alr,
       pch=2,
       cex=1.25,
       col=palette[2])
#lines
for(m in 1:nrow(alr_cv_s5)){
  if(sum(is.na(alr_cv_s5[m,2:11])) < 10){
    segments(x0 = alr_cv_s5[m,1],
             y0 = min(alr_cv_s5[m,2:11],na.rm=T),
             x1 = alr_cv_s5[m,1],
             y1 = max(alr_cv_s5[m,2:11],na.rm=T), 
             col = alpha(palette[2],0.5),
             lwd=1
    )
  }
}

### Age at menopause ----

plot(c(0,max(cv_all)+0.5)~c(0,1),
     main="Age at\nmenopause",
     xlab="Transfer probability",
     ylab="CV age at menopause",
     ylim=c(0,max(cv_all)),
     type="n")
mtext(plot_labels[6], side=3, adj=0, line=2, font=2) # Add bold (a)
#Scenario 2
#average
points(0,
       mean(lht_cv_s2$meno,na.rm=T),
       pch=1,
       cex=1.25,
       col=palette[1])
#lines
segments(x0 = 0,
         y0 = min(lht_cv_s2$meno,na.rm=T),
         x1 = 0,
         y1 = max(lht_cv_s2$meno,na.rm=T), 
         col = alpha(palette[1],0.5),
         lwd=1
)

#Scenario 5
#average
points(lht_mean_s5$m,
       lht_mean_s5$meno,
       pch=2,
       cex=1.25,
       col=palette[2])
#lines
for(m in 1:nrow(meno_cv_s5)){
  if(sum(is.na(meno_cv_s5[m,2:11])) < 10){
    segments(x0 = meno_cv_s5[m,1],
             y0 = min(meno_cv_s5[m,2:11],na.rm=T),
             x1 = meno_cv_s5[m,1],
             y1 = max(meno_cv_s5[m,2:11],na.rm=T), 
             col = alpha(palette[2],0.5),
             lwd=1
    )
  }
}
