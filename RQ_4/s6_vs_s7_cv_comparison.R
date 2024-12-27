# Life cycle variation and resource dynamics ABM: Scenario 6 - results_s6 ----

#Here is the code to get the summary statistics and plots from the simulation of Scenario 6, which aims to understand the influence of resource production on the variability of life cycles.

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

#scenario 6
#cv per life history trait
lng_cv_s6 <- readRDS("./RQ_3/lng_cv_s6.RData")
lro_cv_s6 <- readRDS("./RQ_3/lro_cv_s6.RData")
asm_cv_s6 <- readRDS("./RQ_3/asm_cv_s6.RData")
afr_cv_s6 <- readRDS("./RQ_3/afr_cv_s6.RData")
alr_cv_s6 <- readRDS("./RQ_3/alr_cv_s6.RData")
meno_cv_s6 <- readRDS("./RQ_3/meno_cv_s6.RData")

#Scenario 7
#cv per life history trait
lng_cv_s7 <- readRDS("./RQ_3/lng_cv_s7.RData")
lro_cv_s7 <- readRDS("./RQ_3/lro_cv_s7.RData")
asm_cv_s7 <- readRDS("./RQ_3/asm_cv_s7.RData")
afr_cv_s7 <- readRDS("./RQ_3/afr_cv_s7.RData")
alr_cv_s7 <- readRDS("./RQ_3/alr_cv_s7.RData")
meno_cv_s7 <- readRDS("./RQ_3/meno_cv_s7.RData")

#get the parameter values for labeling

#Production probabilities
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

#Transfer probabilities
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

#Scenario 7
#list of all life history traits
lht_mean_s7 <- list(
  lng = lng_cv_s7,
  lro = lro_cv_s7,
  asm = asm_cv_s7,
  afr = afr_cv_s7,
  alr = alr_cv_s7,
  meno = meno_cv_s7
)
#check it
lht_mean_s7

#Plot it! ----

##Median all together ----

# Get parameter ranges
d_values <- 1:17
m_values <- 1:19
r_values <- 1:10

# Directories for summary files
summary_dir_s4 <- "./RQ_3/lht_summaries_s4"
summary_dir_s7 <- "./RQ_3/lht_summaries_s7"

# Initialize an empty vector to store CV values for both scenarios
cv_all <- c()

# Helper function to process CVs for a given scenario directory
process_cv <- function(summary_dir) {
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
cv_all <- c(process_cv(summary_dir_s4), process_cv(summary_dir_s7))

# Get the maximum CV value
max_cv <- max(cv_all, na.rm = TRUE)

# Print the maximum CV
max_cv

layout(matrix(c(1:12, 13,13,13,13), nrow = 4, ncol = 4, byrow=T), heights = c(1, 1, 1, 0.2))
palette <- hcl.colors(256,"oryel")
#Longevity ----

#scenario 6
image(lht_mean_s6[["lng"]],
        main = "Poor",
        xlab = "Production probability",
        ylab = "Transfer probability",
        col = palette
        )
mtext("A)", side=3, adj=0, line=2.5, font=2) # Add bold (a)
mtext("Longevity", side=3, adj=1.75, line=2.5, font=2) # Add bold (a)
#scenario 7
image(lht_mean_s7[["lng"]],
      main = "Rich",
      xlab = "Production probability",
      ylab = "Transfer probability",
      col = palette
)

#Lifetime reproductive output ----

#scenario 6
image(lht_mean_s6[["lro"]],
      main = "Poor",
      xlab = "Production probability",
      ylab = "Transfer probability",
      col = heat.colors(256)
)
mtext("B)", side=3, adj=0, line=2.5, font=2) # Add bold (a)
mtext("Lifetime\nreproductive output", side=3, adj=1.75, line=2.5, font=2) # Add bold (a)
#scenario 7
image(lht_mean_s7[["lro"]],
      main = "Rich",
      xlab = "Production probability",
      ylab = "Transfer probability",
      col = heat.colors(256)
)

#Age at sexual maturity ----

#scenario 6
image(lht_mean_s6[["asm"]],
      main = "Poor",
      xlab = "Production probability",
      ylab = "Transfer probability",
      col = heat.colors(256)
)
mtext("C)", side=3, adj=0, line=2.5, font=2) # Add bold (a)
mtext("Age at\nsexual maturity", side=3, adj=1.75, line=2.5, font=2) # Add bold (a)
#scenario 7
image(lht_mean_s7[["asm"]],
      main = "Rich",
      xlab = "Production probability",
      ylab = "Transfer probability",
      col = heat.colors(256)
)

#Age at first reproduction ----

#scenario 6
image(lht_mean_s6[["afr"]],
      main = "Poor",
      xlab = "Production probability",
      ylab = "Transfer probability",
      col = heat.colors(256)
)
mtext("C)", side=3, adj=0, line=2.5, font=2) # Add bold (a)
mtext("Age at\nfirst reproduction", side=3, adj=1.75, line=2.5, font=2) # Add bold (a)
#scenario 7
image(lht_mean_s7[["afr"]],
      main = "Rich",
      xlab = "Production probability",
      ylab = "Transfer probability",
      col = heat.colors(256)
)

#Age at last reproduction ----

#scenario 6
image(lht_mean_s6[["alr"]],
      main = "Poor",
      xlab = "Production probability",
      ylab = "Transfer probability",
      col = heat.colors(256)
)
mtext("C)", side=3, adj=0, line=2.5, font=2) # Add bold (a)
mtext("Age at\nlast reproduction", side=3, adj=1.75, line=2.5, font=2) # Add bold (a)
#scenario 7
image(lht_mean_s7[["alr"]],
      main = "Rich",
      xlab = "Production probability",
      ylab = "Transfer probability",
      col = heat.colors(256)
)

# Age at menopause ----

#scenario 6
image(lht_mean_s6[["meno"]],
      main = "Poor",
      xlab = "Production probability",
      ylab = "Transfer probability",
      col = heat.colors(256)
)
mtext("C)", side=3, adj=0, line=2.5, font=2) # Add bold (a)
mtext("Age at\nmenopause", side=3, adj=1.75, line=2.5, font=2) # Add bold (a)
#scenario 7
image(lht_mean_s7[["meno"]],
      main = "Rich",
      xlab = "Production probability",
      ylab = "Transfer probability",
      col = heat.colors(256)
)

# Legend at the bottom
par(mar = c(3, 2, 0.3, 1))  # Adjust margins for the legend
zlim <- range(c(unlist(lht_mean_s6), unlist(lht_mean_s7)))  # Define the range of data

# Create the legend using image()
image(1:256, 1:1, t(matrix(seq(1, 256), nrow = 1)), 
      col = palette, 
      xaxt = "n", 
      yaxt = "n", 
      bty = "n")
axis(1, at = seq(1, 256, length.out = 5), 
     labels = round(seq(zlim[1], zlim[2], length.out = 5), 2))
mtext("CV values", side = 1, line = 2, cex=0.75)  # Add label for the x-axis
