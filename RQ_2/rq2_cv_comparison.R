#Script with code for plots with comparison of CV per life history trait

#import data ----

lht_cv_s2 <- readRDS("./RQ_2/lht_cv_s2.RData")

#Plot it! ----

##Median all together ----

#get the maximum CV of all the life history traits for the scale in the plots
# Get parameter ranges
r_values <- 1:10

# Directory for summary files
summary_dir <- "./RQ_2/lht_summaries_s2"

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

plot(c(0,max_cv+0.5)~c(1,10),
     main="Longevity",
     xlab="Repetition",
     ylab="CV longevity",
     ylim=c(0,max_cv),
     type="n")
mtext(plot_labels[1], side=3, adj=0, line=2, font=2) # Add bold (a)
#scenario 1
#average
points(lht_cv_s2$r,
       lht_cv_s2$lng,
       pch=1,
       cex=1.25,
       col="black")
#lines
for(r in 1:nrow(lht_cv_s2)){
  if(!(is.na(lht_cv_s2[r,"lng"])) == T){
    segments(x0 = lht_cv_s2[r,1],
             y0 = min(lht_cv_s2[r,"lng"],na.rm=T),
             x1 = lht_cv_s2[r,1],
             y1 = max(lht_cv_s2[r,"lng"],na.rm=T), 
             col = alpha("black",0.5),
             lwd=1
    )
  }
}

### Lifetime reproductive output ----

plot(c(0,max_cv+0.5)~c(1,10),
     main="Lifetime\nreproductive output",
     xlab="Repetition",
     ylab="CV lifetime reproductive output",
     ylim=c(0,max_cv),
     type="n")
mtext(plot_labels[2], side=3, adj=0, line=2, font=2) # Add bold (a)
#scenario 1
#average
points(lht_cv_s2$r,
       lht_cv_s2$lro,
       pch=1,
       cex=1.25,
       col="black")
#lines
for(r in 1:nrow(lht_cv_s2)){
  if(!(is.na(lht_cv_s2[r,"lro"])) == T){
    segments(x0 = lht_cv_s2[r,1],
             y0 = min(lht_cv_s2[r,"lro"],na.rm=T),
             x1 = lht_cv_s2[r,1],
             y1 = max(lht_cv_s2[r,"lro"],na.rm=T), 
             col = alpha("black",0.5),
             lwd=1
    )
  }
}

### Age at sexual maturity ----

plot(c(0,max_cv+0.5)~c(1,10),
     main="Age at\nsexual maturity",
     xlab="Repetition",
     ylab="CV age at sexual maturity",
     ylim=c(0,max_cv),
     type="n")
mtext(plot_labels[3], side=3, adj=0, line=2, font=2) # Add bold (a)
#average
points(lht_cv_s2$r,
       lht_cv_s2$asm,
       pch=1,
       cex=1.25,
       col="black")
#lines
for(r in 1:nrow(lht_cv_s2)){
  if(!(is.na(lht_cv_s2[r,"asm"])) == T){
    segments(x0 = lht_cv_s2[r,1],
             y0 = min(lht_cv_s2[r,"asm"],na.rm=T),
             x1 = lht_cv_s2[r,1],
             y1 = max(lht_cv_s2[r,"asm"],na.rm=T), 
             col = alpha("black",0.5),
             lwd=1
    )
  }
}

### Age at first reproduction ----

plot(c(0,max_cv+0.5)~c(1,10),
     main="Age at\nfirst reproduction",
     xlab="Repetition",
     ylab="CV age at first reproduction",
     ylim=c(0,max_cv),
     type="n")
mtext(plot_labels[4], side=3, adj=0, line=2, font=2) # Add bold (a)
#average
points(lht_cv_s2$r,
       lht_cv_s2$afr,
       pch=1,
       cex=1.25,
       col="black")
#lines
for(r in 1:nrow(lht_cv_s2)){
  if(!(is.na(lht_cv_s2[r,"afr"])) == T){
    segments(x0 = lht_cv_s2[r,1],
             y0 = min(lht_cv_s2[r,"afr"],na.rm=T),
             x1 = lht_cv_s2[r,1],
             y1 = max(lht_cv_s2[r,"afr"],na.rm=T), 
             col = alpha("black",0.5),
             lwd=1
    )
  }
}

### Age at last reproduction ----

plot(c(0,max_cv+0.5)~c(1,10),
     main="Age at\nlast reproduction",
     xlab="Repetition",
     ylab="CV age at last reproduction",
     ylim=c(0,max_cv),
     type="n")
mtext(plot_labels[5], side=3, adj=0, line=2, font=2) # Add bold (a)
#average
points(lht_cv_s2$r,
       lht_cv_s2$alr,
       pch=1,
       cex=1.25,
       col="black")
#lines
for(r in 1:nrow(lht_cv_s2)){
  if(!(is.na(lht_cv_s2[r,"alr"])) == T){
    segments(x0 = lht_cv_s2[r,1],
             y0 = min(lht_cv_s2[r,"alr"],na.rm=T),
             x1 = lht_cv_s2[r,1],
             y1 = max(lht_cv_s2[r,"alr"],na.rm=T), 
             col = alpha("black",0.5),
             lwd=1
    )
  }
}

### Age at menopause ----

plot(c(0,max_cv+0.5)~c(1,10),
     main="Age at\nmenopause",
     xlab="Repetition",
     ylab="CV age at menopause",
     ylim=c(0,max_cv),
     type="n")
mtext(plot_labels[6], side=3, adj=0, line=2, font=2) # Add bold (a)
#average
points(lht_cv_s2$r,
       lht_cv_s2$meno,
       pch=1,
       cex=1.25,
       col="black")
#lines
for(r in 1:nrow(lht_cv_s2)){
  if(!(is.na(lht_cv_s2[r,"meno"])) == T){
    segments(x0 = lht_cv_s2[r,1],
             y0 = min(lht_cv_s2[r,"meno"],na.rm=T),
             x1 = lht_cv_s2[r,1],
             y1 = max(lht_cv_s2[r,"meno"],na.rm=T), 
             col = alpha("black",0.5),
             lwd=1
    )
  }
}

