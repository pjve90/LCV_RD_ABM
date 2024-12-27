#Script with code for plots with comparison of CV per life history trait

#import data ----

lng_cv_s1 <- readRDS("./RQ_1/lng_cv_s1.RData")
lro_cv_s1 <- readRDS("./RQ_1/lro_cv_s1.RData")
asm_cv_s1 <- readRDS("./RQ_1/asm_cv_s1.RData")
afr_cv_s1 <- readRDS("./RQ_1/afr_cv_s1.RData")
alr_cv_s1 <- readRDS("./RQ_1/alr_cv_s1.RData")
meno_cv_s1 <- readRDS("./RQ_1/meno_cv_s1.RData")

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


#Plot it! ----

##Median all together ----

#get the maximum CV of all the life history traits for the scale in the plots
# Get parameter ranges
d_values <- 1:17
r_values <- 1:10

# Directory for summary files
summary_dir <- "./RQ_1/lht_summaries"

# Initialize an empty vector to store CV values
cv_all <- c()

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
}

# Remove NA values from the CV vector
cv_all <- cv_all[!is.na(cv_all)]

# Get the maximum CV value
max_cv <- max(cv_all, na.rm = TRUE)

# Print the maximum CV
max_cv


#plot the CV together
par(mar=c(5, 4, 4, 2))
layout(matrix(c(1,1,2,2,3,4,5,6),ncol=4,byrow=T))

# Define labels for each plot
plot_labels <- c("A)", "B)", "C)", "D)", "E)", "F)")

### Longevity ----

plot(c(0,max_cv+0.5)~c(0,1),
     main="Longevity",
     xlab="Production probability",
     ylab="CV longevity",
     ylim=c(0,max_cv),
     type="n")
mtext(plot_labels[1], side=3, adj=0, line=2, font=2) # Add bold (a)
#scenario 1
#average
points(lht_mean_s1$p,
       lht_mean_s1$lng,
       pch=1,
       cex=1.25,
       col="black")
#lines
for(d in 1:nrow(lng_cv_s1)){
  if(sum(is.na(lng_cv_s1[d,2:11])) < 10){
    segments(x0 = lng_cv_s1[d,1],
             y0 = min(lng_cv_s1[d,2:11],na.rm=T),
             x1 = lng_cv_s1[d,1],
             y1 = max(lng_cv_s1[d,2:11],na.rm=T), 
             col = alpha("black",0.5),
             lwd=1
             )
  }
}

### Lifetime reproductive output ----

plot(c(0,max_cv+0.5)~c(0,1),
     main="Lifetime\nreproductive output",
     xlab="Production probability",
     ylab="CV lifetime reproductive output",
     ylim=c(0,max_cv),
     type="n")
mtext(plot_labels[2], side=3, adj=0, line=2, font=2) # Add bold (a)
#scenario 1
#average
points(lht_mean_s1$p,
       lht_mean_s1$lro,
       pch=1,
       cex=1.25,
       col="black")
#lines
for(d in 1:nrow(lro_cv_s1)){
  if(sum(is.na(lro_cv_s1[d,2:11])) < 10){
    segments(x0 = lro_cv_s1[d,1],
             y0 = min(lro_cv_s1[d,2:11],na.rm=T),
             x1 = lro_cv_s1[d,1],
             y1 = max(lro_cv_s1[d,2:11],na.rm=T), 
             col = alpha("black",0.5),
             lwd=1
    )
  }
}

### Age at sexual maturity ----

plot(c(0,max_cv+0.5)~c(0,1),
     main="Age at\nsexual maturity",
     xlab="Production probability",
     ylab="CV age at sexual maturity",
     ylim=c(0,max_cv),
     type="n")
mtext(plot_labels[3], side=3, adj=0, line=2, font=2) # Add bold (a)
#average
points(lht_mean_s1$p,
       lht_mean_s1$asm,
       pch=1,
       cex=1.25,
       col="black")
#lines
for(d in 1:nrow(asm_cv_s1)){
  if(sum(is.na(asm_cv_s1[d,2:11])) < 10){
    segments(x0 = asm_cv_s1[d,1],
             y0 = min(asm_cv_s1[d,2:11],na.rm=T),
             x1 = asm_cv_s1[d,1],
             y1 = max(asm_cv_s1[d,2:11],na.rm=T), 
             col = alpha("black",0.5),
             lwd=1
    )
  }
}

### Age at first reproduction ----

plot(c(0,max_cv+0.5)~c(0,1),
     main="Age at\nfirst reproduction",
     xlab="Production probability",
     ylab="CV age at first reproduction",
     ylim=c(0,max_cv),
     type="n")
mtext(plot_labels[4], side=3, adj=0, line=2, font=2) # Add bold (a)
#average
points(lht_mean_s1$p,
       lht_mean_s1$afr,
       pch=1,
       cex=1.25,
       col="black")
#lines
for(d in 1:nrow(afr_cv_s1)){
  if(sum(is.na(afr_cv_s1[d,2:11])) < 10){
    segments(x0 = afr_cv_s1[d,1],
             y0 = min(afr_cv_s1[d,2:11],na.rm=T),
             x1 = afr_cv_s1[d,1],
             y1 = max(afr_cv_s1[d,2:11],na.rm=T), 
             col = alpha("black",0.5),
             lwd=1
    )
  }
}

### Age at last reproduction ----

plot(c(0,max_cv+0.5)~c(0,1),
     main="Age at\nlast reproduction",
     xlab="Production probability",
     ylab="CV age at last reproduction",
     ylim=c(0,max_cv),
     type="n")
mtext(plot_labels[5], side=3, adj=0, line=2, font=2) # Add bold (a)
#average
points(lht_mean_s1$p,
       lht_mean_s1$alr,
       pch=1,
       cex=1.25,
       col="black")
#lines
for(d in 1:nrow(alr_cv_s1)){
  if(sum(is.na(alr_cv_s1[d,2:11])) < 10){
    segments(x0 = alr_cv_s1[d,1],
             y0 = min(alr_cv_s1[d,2:11],na.rm=T),
             x1 = alr_cv_s1[d,1],
             y1 = max(alr_cv_s1[d,2:11],na.rm=T), 
             col = alpha("black",0.5),
             lwd=1
    )
  }
}

### Age at menopause ----

plot(c(0,max_cv+0.5)~c(0,1),
     main="Age at\nmenopause",
     xlab="Production probability",
     ylab="CV age at menopause",
     ylim=c(0,max_cv),
     type="n")
mtext(plot_labels[6], side=3, adj=0, line=2, font=2) # Add bold (a)
#average
points(lht_mean_s1$p,
       lht_mean_s1$meno,
       pch=1,
       cex=1.25,
       col="black")
#lines
for(d in 1:nrow(meno_cv_s1)){
  if(sum(is.na(meno_cv_s1[d,2:11])) < 10){
    segments(x0 = meno_cv_s1[d,1],
             y0 = min(meno_cv_s1[d,2:11],na.rm=T),
             x1 = meno_cv_s1[d,1],
             y1 = max(meno_cv_s1[d,2:11],na.rm=T), 
             col = alpha("black",0.5),
             lwd=1
    )
  }
}

