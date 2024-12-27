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
lng_cv_s1 <- readRDS("./RQ_2/lng_cv_s1.RData")
lro_cv_s1 <- readRDS("./RQ_2/lro_cv_s1.RData")
asm_cv_s1 <- readRDS("./RQ_2/asm_cv_s1.RData")
afr_cv_s1 <- readRDS("./RQ_2/afr_cv_s1.RData")
alr_cv_s1 <- readRDS("./RQ_2/alr_cv_s1.RData")
meno_cv_s1 <- readRDS("./RQ_2/meno_cv_s1.RData")

#scenario 4
#cv per life history trait
lng_cv_s4 <- readRDS("./RQ_2/lng_cv_s4.RData")
lro_cv_s4 <- readRDS("./RQ_2/lro_cv_s4.RData")
asm_cv_s4 <- readRDS("./RQ_2/asm_cv_s4.RData")
afr_cv_s4 <- readRDS("./RQ_2/afr_cv_s4.RData")
alr_cv_s4 <- readRDS("./RQ_2/alr_cv_s4.RData")
meno_cv_s4 <- readRDS("./RQ_2/meno_cv_s4.RData")

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

#scenario 4
#create data frame
lht_mean_s4 <- as.data.frame(matrix(NA,ncol(prod_prob),7))
colnames(lht_mean_s4) <- c("p","lng","lro","asm","afr","alr","meno")
lht_mean_s4$p <- prod_prob[2,]
lht_mean_s4
#calculate the mean CV per life history trait
for(i in 1:nrow(lht_mean_s4)){
  lht_mean_s4[i,"lng"] <- ifelse(sum(is.na(lng_cv_s4[i,2:ncol(lng_cv_s4)]))==10,NA,mean(as.numeric(lng_cv_s4[i,2:ncol(lng_cv_s4)]),na.rm=T))
  lht_mean_s4[i,"lro"] <- ifelse(sum(is.na(lro_cv_s4[i,2:ncol(lro_cv_s4)]))==10,NA,mean(as.numeric(lro_cv_s4[i,2:ncol(lro_cv_s4)]),na.rm=T))
  lht_mean_s4[i,"asm"] <- ifelse(sum(is.na(asm_cv_s4[i,2:ncol(asm_cv_s4)]))==10,NA,mean(as.numeric(asm_cv_s4[i,2:ncol(asm_cv_s4)]),na.rm=T))
  lht_mean_s4[i,"afr"] <- ifelse(sum(is.na(afr_cv_s4[i,2:ncol(afr_cv_s4)]))==10,NA,mean(as.numeric(afr_cv_s4[i,2:ncol(afr_cv_s4)]),na.rm=T))
  lht_mean_s4[i,"alr"] <- ifelse(sum(is.na(alr_cv_s4[i,2:ncol(alr_cv_s4)]))==10,NA,mean(as.numeric(alr_cv_s4[i,2:ncol(alr_cv_s4)]),na.rm=T))
  lht_mean_s4[i,"meno"] <- ifelse(sum(is.na(meno_cv_s4[i,2:ncol(meno_cv_s4)]))==10,NA,mean(as.numeric(meno_cv_s4[i,2:ncol(meno_cv_s4)]),na.rm=T))
}
lht_mean_s4

#Plot it! ----

##Median all together ----

# Get parameter ranges
d_values <- 1:17
r_values <- 1:10

# Directories for summary files
summary_dir_s1 <- "./RQ_2/lht_summaries_s1"
summary_dir_s4 <- "./RQ_2/lht_summaries_s4"

# Initialize an empty vector to store CV values for both scenarios
cv_all <- c()

# Helper function to process CVs for a given scenario directory
process_cv <- function(summary_dir) {
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

# Process CVs for both scenarios and combine the results
cv_all <- c(process_cv(summary_dir_s1), process_cv(summary_dir_s4))

# Get the maximum CV value
max_cv <- max(cv_all, na.rm = TRUE)

# Print the maximum CV
max_cv

#plot the CV together
par(mar = c(5, 4, 4, 4))  # Add space on the right side
layout(matrix(c(1, 1, 2, 2, 3, 4, 5, 6,7,7,7,7), ncol = 4, byrow = TRUE), heights = c(1, 1, 0.1))

#colour palette
palette <- hcl.colors(10,"ag_sunset")[c(1,6)]

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
#lines
for(d in 1:nrow(lng_cv_s1)){
  if(sum(is.na(lng_cv_s1[d,2:11])) < 10){
    segments(x0 = lng_cv_s1[d,1],
             y0 = min(lng_cv_s1[d,2:11],na.rm=T),
             x1 = lng_cv_s1[d,1],
             y1 = max(lng_cv_s1[d,2:11],na.rm=T), 
             col = alpha(palette[1],0.5),
             lwd=1
    )
  }
}
#scenario 4
#average
points(lht_mean_s4$p,
       lht_mean_s4$lng,
       pch=2,
       cex=1.25,
       col=palette[2])
#lines
for(d in 1:nrow(lng_cv_s4)){
  if(sum(is.na(lng_cv_s4[d,2:11])) < 10){
    segments(x0 = lng_cv_s4[d,1],
             y0 = min(lng_cv_s4[d,2:11],na.rm=T),
             x1 = lng_cv_s4[d,1],
             y1 = max(lng_cv_s4[d,2:11],na.rm=T), 
             col = alpha(palette[2],0.5),
             lwd=1
    )
  }
}

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
#lines
for(d in 1:nrow(lro_cv_s1)){
  if(sum(is.na(lro_cv_s1[d,2:11])) < 10){
    segments(x0 = lro_cv_s1[d,1],
             y0 = min(lro_cv_s1[d,2:11],na.rm=T),
             x1 = lro_cv_s1[d,1],
             y1 = max(lro_cv_s1[d,2:11],na.rm=T), 
             col = alpha(palette[1],0.5),
             lwd=1
    )
  }
}
#scenario 4
#average
points(lht_mean_s4$p,
       lht_mean_s4$lro,
       pch=2,
       cex=1.25,
       col=palette[2])
#lines
for(d in 1:nrow(lro_cv_s4)){
  if(sum(is.na(lro_cv_s4[d,2:11])) < 10){
    segments(x0 = lro_cv_s4[d,1],
             y0 = min(lro_cv_s4[d,2:11],na.rm=T),
             x1 = lro_cv_s4[d,1],
             y1 = max(lro_cv_s4[d,2:11],na.rm=T), 
             col = alpha(palette[2],0.5),
             lwd=1
    )
  }
}


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
#lines
for(d in 1:nrow(asm_cv_s1)){
  if(sum(is.na(asm_cv_s1[d,2:11])) < 10){
    segments(x0 = asm_cv_s1[d,1],
             y0 = min(asm_cv_s1[d,2:11],na.rm=T),
             x1 = asm_cv_s1[d,1],
             y1 = max(asm_cv_s1[d,2:11],na.rm=T), 
             col = alpha(palette[1],0.5),
             lwd=1
    )
  }
}
#scenario 4
#average
points(lht_mean_s4$p,
       lht_mean_s4$asm,
       pch=2,
       cex=1.25,
       col=palette[2])
#lines
for(d in 1:nrow(asm_cv_s4)){
  if(sum(is.na(asm_cv_s4[d,2:11])) < 10){
    segments(x0 = asm_cv_s4[d,1],
             y0 = min(asm_cv_s4[d,2:11],na.rm=T),
             x1 = asm_cv_s4[d,1],
             y1 = max(asm_cv_s4[d,2:11],na.rm=T), 
             col = alpha(palette[2],0.5),
             lwd=1
    )
  }
}

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
#lines
for(d in 1:nrow(afr_cv_s1)){
  if(sum(is.na(afr_cv_s1[d,2:11])) < 10){
    segments(x0 = afr_cv_s1[d,1],
             y0 = min(afr_cv_s1[d,2:11],na.rm=T),
             x1 = afr_cv_s1[d,1],
             y1 = max(afr_cv_s1[d,2:11],na.rm=T), 
             col = alpha(palette[1],0.5),
             lwd=1
    )
  }
}
#scenario 4
#average
points(lht_mean_s4$p,
       lht_mean_s4$afr,
       pch=2,
       cex=1.25,
       col=palette[2])
#lines
for(d in 1:nrow(afr_cv_s4)){
  if(sum(is.na(afr_cv_s4[d,2:11])) < 10){
    segments(x0 = afr_cv_s4[d,1],
             y0 = min(afr_cv_s4[d,2:11],na.rm=T),
             x1 = afr_cv_s4[d,1],
             y1 = max(afr_cv_s4[d,2:11],na.rm=T), 
             col = alpha(palette[2],0.5),
             lwd=1
    )
  }
}

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
#lines
for(d in 1:nrow(alr_cv_s1)){
  if(sum(is.na(alr_cv_s1[d,2:11])) < 10){
    segments(x0 = alr_cv_s1[d,1],
             y0 = min(alr_cv_s1[d,2:11],na.rm=T),
             x1 = alr_cv_s1[d,1],
             y1 = max(alr_cv_s1[d,2:11],na.rm=T), 
             col = alpha(palette[1],0.5),
             lwd=1
    )
  }
}
#scenario 4
#average
points(lht_mean_s4$p,
       lht_mean_s4$alr,
       pch=2,
       cex=1.25,
       col=palette[2])
#lines
for(d in 1:nrow(alr_cv_s4)){
  if(sum(is.na(alr_cv_s4[d,2:11])) < 10){
    segments(x0 = alr_cv_s4[d,1],
             y0 = min(alr_cv_s4[d,2:11],na.rm=T),
             x1 = alr_cv_s4[d,1],
             y1 = max(alr_cv_s4[d,2:11],na.rm=T), 
             col = alpha(palette[2],0.5),
             lwd=1
    )
  }
}

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
#lines
for(d in 1:nrow(meno_cv_s1)){
  if(sum(is.na(meno_cv_s1[d,2:11])) < 10){
    segments(x0 = meno_cv_s1[d,1],
             y0 = min(meno_cv_s1[d,2:11],na.rm=T),
             x1 = meno_cv_s1[d,1],
             y1 = max(meno_cv_s1[d,2:11],na.rm=T), 
             col = alpha(palette[1],0.5),
             lwd=1
    )
  }
}
#scenario 4
#average
points(lht_mean_s4$p,
       lht_mean_s4$meno,
       pch=2,
       cex=1.25,
       col=palette[2])
#lines
for(d in 1:nrow(meno_cv_s4)){
  if(sum(is.na(meno_cv_s4[d,2:11])) < 10){
    segments(x0 = meno_cv_s4[d,1],
             y0 = min(meno_cv_s4[d,2:11],na.rm=T),
             x1 = meno_cv_s4[d,1],
             y1 = max(meno_cv_s4[d,2:11],na.rm=T), 
             col = alpha(palette[2],0.5),
             lwd=1
    )
  }
}

#legend ----

# Adjust margins for the legend row 
par(mar = c(0, 0, 0, 0))  # Remove margins for the legend area
plot.new()  # Create an empty plot for the legend

# Add the legend horizontally below the entire plot area
legend("center", 
       legend = c("Scenario 1: Production probability", "Scenario 4: Production probability"), 
       col = palette, 
       pch = c(1, 2), 
       lty = 1, 
       horiz = TRUE, 
       bty = "n",  # Remove box around legend
       cex = 1.2,
       text.width = 0.3)  # Adjust spacing between legend elements)  # Adjust text size