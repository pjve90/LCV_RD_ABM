# Life cycle variation and resource dynamics ABM: Scenario 1 - results_s1 ----

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

#import data ----

#scenario 1
#cv per life history trait
lng_cv_s1 <- readRDS("./RQ_3/lng_cv_s1.RData")
lro_cv_s1 <- readRDS("./RQ_3/lro_cv_s1.RData")
asm_cv_s1 <- readRDS("./RQ_3/asm_cv_s1.RData")
afr_cv_s1 <- readRDS("./RQ_3/afr_cv_s1.RData")
alr_cv_s1 <- readRDS("./RQ_3/alr_cv_s1.RData")
meno_cv_s1 <- readRDS("./RQ_3/meno_cv_s1.RData")

#scenario 6
#cv per life history trait
lng_cv_s6 <- readRDS("./RQ_3/lng_cv_s6.RData")
lro_cv_s6 <- readRDS("./RQ_3/lro_cv_s6.RData")
asm_cv_s6 <- readRDS("./RQ_3/asm_cv_s6.RData")
afr_cv_s6 <- readRDS("./RQ_3/afr_cv_s6.RData")
alr_cv_s6 <- readRDS("./RQ_3/alr_cv_s6.RData")
meno_cv_s6 <- readRDS("./RQ_3/meno_cv_s6.RData")

#get the parameter values for data wrangling

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

#scenario 1
#create data frame
lht_mean_s1 <- as.data.frame(matrix(NA,ncol(prod_prob),7))
colnames(lht_mean_s1) <- c("p","lng","lro","asm","afr","alr","meno")
lht_mean_s1$p <- prod_prob[2,]
lht_mean_s1
#calculate the mean CV per life history trait
for(i in 1:nrow(lht_mean_s1)){
  lht_mean_s1[i,"lng"] <- ifelse(sum(is.na(lng_cv_s1[i,2:ncol(lng_cv_s1)]))==10,NA,mean(as.numeric(lng_cv_s1[i,2:ncol(lng_cv_s1)]),na.rm=T))
  lht_mean_s1[i,"lro"] <- ifelse(sum(is.na(lro_cv_s1[i,2:ncol(lro_cv_s1)]))==10,NA,mean(as.numeric(lro_cv_s1[i,2:ncol(lro_cv_s1)]),na.rm=T))
  lht_mean_s1[i,"asm"] <- ifelse(sum(is.na(asm_cv_s1[i,2:ncol(asm_cv_s1)]))==10,NA,mean(as.numeric(asm_cv_s1[i,2:ncol(asm_cv_s1)]),na.rm=T))
  lht_mean_s1[i,"afr"] <- ifelse(sum(is.na(afr_cv_s1[i,2:ncol(afr_cv_s1)]))==10,NA,mean(as.numeric(afr_cv_s1[i,2:ncol(afr_cv_s1)]),na.rm=T))
  lht_mean_s1[i,"alr"] <- ifelse(sum(is.na(alr_cv_s1[i,2:ncol(alr_cv_s1)]))==10,NA,mean(as.numeric(alr_cv_s1[i,2:ncol(alr_cv_s1)]),na.rm=T))
  lht_mean_s1[i,"meno"] <- ifelse(sum(is.na(meno_cv_s1[i,2:ncol(meno_cv_s1)]))==10,NA,mean(as.numeric(meno_cv_s1[i,2:ncol(meno_cv_s1)]),na.rm=T))
}
lht_mean_s1

#scenario 6
#list of all life history traits
lht_mean_s6 <- list(
  lng = lng_cv_s6,
  lro = lro_cv_s6,
  asm = asm_cv_s6,
  afr = afr_cv_s6,
  alr = alr_cv_s6,
  meno = meno_cv_s6
)
#check it
lht_mean_s6

#Plot it! ----

##Median all together ----

# Get parameter ranges
d_values <- 1:17
m_values <- 1:19
r_values <- 1:10

# Directories for summary files
summary_dir_s1 <- "./RQ_3/lht_summaries_s1"
summary_dir_s6 <- "./RQ_3/lht_summaries_s6"

# Initialize an empty vector to store CV values for both scenarios
cv_all <- c()

# Helper function to process CVs for a given scenario directory
process_cv_s1 <- function(summary_dir) {
  # Initialize a vector to store CV values
  cv_values <- c()
  
  # Loop through all combinations of d and r
  for (d in d_values) {
    for (r in r_values) {
      # Construct the file name for the corresponding summary file
      summary_file <- file.path(summary_dir, sprintf("summary_lht_list_d%d_r%d.fst", d, r))
      
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

# Helper function to process CVs for a given scenario directory
process_cv_s6 <- function(summary_dir) {
  # Initialize a vector to store CV values
  cv_values <- c()
  
  # Loop through all combinations of d and r
  for (d in d_values) {
    for(m in m_values){
      for (r in r_values) {
      # Construct the file name for the corresponding summary file
      summary_file <- file.path(summary_dir, sprintf("summary_lht_list_d%d_m%d_r%d.fst", d, m, r))
      
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
  }
  
  # Return the CV values after removing NAs
  cv_values[!is.na(cv_values)]
}

# Process CVs for both scenarios and combine the results
cv_all <- c(process_cv_s1(summary_dir_s1), process_cv_s6(summary_dir_s6))

# Get the maximum CV value
max_cv <- max(cv_all, na.rm = TRUE)

# Print the maximum CV
max_cv

#plot the CV together
par(mar = c(5, 4, 4, 8))  # Add space on the right side for the legend
layout(matrix(c(1, 1, 2, 2, 7, 3, 4, 5, 6, 7), ncol = 5, byrow = TRUE), widths = c(1, 1, 1, 1, 0.5))

#colour palette
palette <- c(hcl.colors(10,"ag_sunset")[4],hcl.colors(19,"zissou 1"))

# Define labels for each plot
plot_labels <- c("A)", "B)", "C)", "D)", "E)", "F)")

### Longevity ----

plot(c(0,max(cv_all)+0.5)~c(0,1),
     main="Longevity",
     xlab="Production probability",
     ylab="CV longevity",
     ylim=c(0,max(cv_all)),
     type="n")
mtext(plot_labels[1], side=3, adj=0, line=2, font=2) # Add bold (a)
#scenario 1
#average
points(lht_mean_s1$p,
       lht_mean_s1$lng,
       pch=1,
       cex=1.25,
       col=palette[1])
# #lines
# for(d in 1:nrow(lng_cv_s1)){
#   if(sum(is.na(lng_cv_s1[d,2:11])) < 10){
#     segments(x0 = lng_cv_s1[d,1],
#              y0 = min(lng_cv_s1[d,2:11],na.rm=T),
#              x1 = lng_cv_s1[d,1],
#              y1 = max(lng_cv_s1[d,2:11],na.rm=T), 
#              col = alpha(palette[1],0.5),
#              lwd=1
#     )
#   }
# }
#scenario 6
#average
for (i in 1:length(prod_prob[2, ])) {
  points(
    rep(prod_prob[2, i], 19),
    lht_mean_s6[["lng"]][, i + 1],
    pch = 2,
    cex = 1.25,
    col = palette[2:20]
  )
}
# #lines
# for(d in 1:nrow(lng_cv_s6)){
#   if(sum(is.na(lng_cv_s6[d,2:11])) < 10){
#     segments(x0 = lng_cv_s6[d,1],
#              y0 = min(lng_cv_s6[d,2:11],na.rm=T),
#              x1 = lng_cv_s6[d,1],
#              y1 = max(lng_cv_s6[d,2:11],na.rm=T), 
#              col = alpha(palette[2],0.5),
#              lwd=1
#     )
#   }
# }

### Lifetime reproductive output ----

plot(c(0,max(cv_all)+0.5)~c(0,1),
     main="Lifetime\nreproductive output",
     xlab="Production probability",
     ylab="CV lifetime reproductive output",
     ylim=c(0,max(cv_all)),
     type="n")
mtext(plot_labels[2], side=3, adj=0, line=2, font=2) # Add bold (a)
#scenario 1
#average
points(lht_mean_s1$p,
       lht_mean_s1$lro,
       pch=1,
       cex=1.25,
       col=palette[1])
# #lines
# for(d in 1:nrow(lro_cv_s1)){
#   if(sum(is.na(lro_cv_s1[d,2:11])) < 10){
#     segments(x0 = lro_cv_s1[d,1],
#              y0 = min(lro_cv_s1[d,2:11],na.rm=T),
#              x1 = lro_cv_s1[d,1],
#              y1 = max(lro_cv_s1[d,2:11],na.rm=T), 
#              col = alpha(palette[1],0.5),
#              lwd=1
#     )
#   }
# }
#scenario 6
#average
for (i in 1:length(prod_prob[2, ])) {
  points(
    rep(prod_prob[2, i], 19),
    lht_mean_s6[["lro"]][, i + 1],
    pch = 2,
    cex = 1.25,
    col = palette[2:20]
  )
}
# #lines
# for(d in 1:nrow(lro_cv_s6)){
#   if(sum(is.na(lro_cv_s6[d,2:11])) < 10){
#     segments(x0 = lro_cv_s6[d,1],
#              y0 = min(lro_cv_s6[d,2:11],na.rm=T),
#              x1 = lro_cv_s6[d,1],
#              y1 = max(lro_cv_s6[d,2:11],na.rm=T), 
#              col = alpha(palette[2],0.5),
#              lwd=1
#     )
#   }
# }

### Age at sexual maturity ----

plot(c(0,max(cv_all)+0.5)~c(0,1),
     main="Age at\nsexual maturity",
     xlab="Production probability",
     ylab="CV age at sexual maturity",
     ylim=c(0,max(cv_all)),
     type="n")
mtext(plot_labels[3], side=3, adj=0, line=2, font=2) # Add bold (a)
#scenario 1
#average
points(lht_mean_s1$p,
       lht_mean_s1$asm,
       pch=1,
       cex=1.25,
       col=palette[1])
# #lines
# for(d in 1:nrow(asm_cv_s1)){
#   if(sum(is.na(asm_cv_s1[d,2:11])) < 10){
#     segments(x0 = asm_cv_s1[d,1],
#              y0 = min(asm_cv_s1[d,2:11],na.rm=T),
#              x1 = asm_cv_s1[d,1],
#              y1 = max(asm_cv_s1[d,2:11],na.rm=T), 
#              col = alpha(palette[1],0.5),
#              lwd=1
#     )
#   }
# }
#scenario 6
#average
for (i in 1:length(prod_prob[2, ])) {
  points(
    rep(prod_prob[2, i], 19),
    lht_mean_s6[["asm"]][, i + 1],
    pch = 2,
    cex = 1.25,
    col = palette[2:20]
  )
}
# #lines
# for(d in 1:nrow(asm_cv_s6)){
#   if(sum(is.na(asm_cv_s6[d,2:11])) < 10){
#     segments(x0 = asm_cv_s6[d,1],
#              y0 = min(asm_cv_s6[d,2:11],na.rm=T),
#              x1 = asm_cv_s6[d,1],
#              y1 = max(asm_cv_s6[d,2:11],na.rm=T), 
#              col = alpha(palette[2],0.5),
#              lwd=1
#     )
#   }
# }

### Age at first reproduction ----

plot(c(0,max(cv_all)+0.5)~c(0,1),
     main="Age at\nfirst reproduction",
     xlab="Production probability",
     ylab="CV age at first reproduction",
     ylim=c(0,max(cv_all)),
     type="n")
mtext(plot_labels[4], side=3, adj=0, line=2, font=2) # Add bold (a)
#scenario 1
#average
points(lht_mean_s1$p,
       lht_mean_s1$afr,
       pch=1,
       cex=1.25,
       col=palette[1])
# #lines
# for(d in 1:nrow(afr_cv_s1)){
#   if(sum(is.na(afr_cv_s1[d,2:11])) < 10){
#     segments(x0 = afr_cv_s1[d,1],
#              y0 = min(afr_cv_s1[d,2:11],na.rm=T),
#              x1 = afr_cv_s1[d,1],
#              y1 = max(afr_cv_s1[d,2:11],na.rm=T), 
#              col = alpha(palette[1],0.5),
#              lwd=1
#     )
#   }
# }
#scenario 6
#average
for (i in 1:length(prod_prob[2, ])) {
  points(
    rep(prod_prob[2, i], 19),
    lht_mean_s6[["afr"]][, i + 1],
    pch = 2,
    cex = 1.25,
    col = palette[2:20]
  )
}
# #lines
# for(d in 1:nrow(afr_cv_s6)){
#   if(sum(is.na(afr_cv_s6[d,2:11])) < 10){
#     segments(x0 = afr_cv_s6[d,1],
#              y0 = min(afr_cv_s6[d,2:11],na.rm=T),
#              x1 = afr_cv_s6[d,1],
#              y1 = max(afr_cv_s6[d,2:11],na.rm=T), 
#              col = alpha(palette[2],0.5),
#              lwd=1
#     )
#   }
# }

### Age at last reproduction ----

plot(c(0,max(cv_all)+0.5)~c(0,1),
     main="Age at\nlast reproduction",
     xlab="Production probability",
     ylab="CV age at last reproduction",
     ylim=c(0,max(cv_all)),
     type="n")
mtext(plot_labels[5], side=3, adj=0, line=2, font=2) # Add bold (a)
#scenario 1
#average
points(lht_mean_s1$p,
       lht_mean_s1$alr,
       pch=1,
       cex=1.25,
       col=palette[1])
# #lines
# for(d in 1:nrow(alr_cv_s1)){
#   if(sum(is.na(alr_cv_s1[d,2:11])) < 10){
#     segments(x0 = alr_cv_s1[d,1],
#              y0 = min(alr_cv_s1[d,2:11],na.rm=T),
#              x1 = alr_cv_s1[d,1],
#              y1 = max(alr_cv_s1[d,2:11],na.rm=T), 
#              col = alpha(palette[1],0.5),
#              lwd=1
#     )
#   }
# }
#scenario 6
#average
for (i in 1:length(prod_prob[2, ])) {
  points(
    rep(prod_prob[2, i], 19),
    lht_mean_s6[["alr"]][, i + 1],
    pch = 2,
    cex = 1.25,
    col = palette[2:20]
  )
}
# #lines
# for(d in 1:nrow(alr_cv_s6)){
#   if(sum(is.na(alr_cv_s6[d,2:11])) < 10){
#     segments(x0 = alr_cv_s6[d,1],
#              y0 = min(alr_cv_s6[d,2:11],na.rm=T),
#              x1 = alr_cv_s6[d,1],
#              y1 = max(alr_cv_s6[d,2:11],na.rm=T), 
#              col = alpha(palette[2],0.5),
#              lwd=1
#     )
#   }
# }

### Age at menopause ----

plot(c(0,max(cv_all)+0.5)~c(0,1),
     main="Age at\nmenopause",
     xlab="Production probability",
     ylab="CV age at menopause",
     ylim=c(0,max(cv_all)),
     type="n")
mtext(plot_labels[6], side=3, adj=0, line=2, font=2) # Add bold (a)
#scenario 1
#average
points(lht_mean_s1$p,
       lht_mean_s1$meno,
       pch=1,
       cex=1.25,
       col=palette[1])
# #lines
# for(d in 1:nrow(meno_cv_s1)){
#   if(sum(is.na(meno_cv_s1[d,2:11])) < 10){
#     segments(x0 = meno_cv_s1[d,1],
#              y0 = min(meno_cv_s1[d,2:11],na.rm=T),
#              x1 = meno_cv_s1[d,1],
#              y1 = max(meno_cv_s1[d,2:11],na.rm=T), 
#              col = alpha(palette[1],0.5),
#              lwd=1
#     )
#   }
# }
#scenario 6
#average
for (i in 1:length(prod_prob[2, ])) {
  points(
    rep(prod_prob[2, i], 19),
    lht_mean_s6[["meno"]][, i + 1],
    pch = 2,
    cex = 1.25,
    col = palette[2:20]
  )
}
# #lines
# for(d in 1:nrow(meno_cv_s6)){
#   if(sum(is.na(meno_cv_s6[d,2:11])) < 10){
#     segments(x0 = meno_cv_s6[d,1],
#              y0 = min(meno_cv_s6[d,2:11],na.rm=T),
#              x1 = meno_cv_s6[d,1],
#              y1 = max(meno_cv_s6[d,2:11],na.rm=T), 
#              col = alpha(palette[2],0.5),
#              lwd=1
#     )
#   }
# }


#legend ----

# Step 1: Create a vector to store indices of data frames with positive longevity
valid_indices <- c()

# Step 2: First pass through to identify valid data frames
for(m in 1:19) {
  # Check if the sum of means is greater than zero
  if (sum(apply(get(paste("m", m, sep = "_")), 2, mean)) > 0) {
    valid_indices <- c(valid_indices, m)  # Store the valid index
  }
}

# Scenario 1 Legend (Slot 7, first row)
par(mar = c(0, 0, 0, 0))  # Adjust margins for the legend area
plot.new()  # Create an empty plot for the legend
legend("center", 
       title = "Scenario 1:\nProduction probability", 
       legend = 0.5, 
       col = palette[1], 
       pch = 1, 
       bty = "n", 
       cex = 1.2)

# Scenario 6 Legend (Slot 7, second row)
par(mar = c(0, 0, 0, 0))  # Adjust margins again
plot.new()  # Create another empty plot for the second legend
legend("center", 
       title = "Scenario 6:\nTransfer probabilities (19)", 
       legend = transf_prob[ valid_indices], 
       col = palette[2:20], 
       pch = rep(2, 19), 
       bty = "n", 
       cex = 1.2)