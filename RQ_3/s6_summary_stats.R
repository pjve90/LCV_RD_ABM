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
#install.packages("fst")
library(fst)

#Scenario 6 ----

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

transf_prob <- round(seq((blockmatrix[i, j] - 0.2), (blockmatrix[i, j] + 0.2), length = 19), 2)

# Directory containing the input .fst files
input_dir <- "./Scenario_6/lht_lists"

# Directory to save the output .fst files
output_dir <- "./RQ_3/lht_summaries_s6"

# Create the output directory if it doesn't exist
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Get all file names in the directory
file_names <- list.files(input_dir, pattern = "lht_list_d\\d+_m\\d+_r\\d+\\.fst", full.names = TRUE)

# Loop through each file
for (file_path in file_names) {
  
  # Extract the parameter values for naming
  file_name <- basename(file_path)
  param_name <- sub("\\.fst$", "", file_name) # Remove ".fst" extension
  
  # Read the data from the .fst file
  data <- read_fst(file_path)
  
  # Initialize a list to store this file's statistics
  stats <- list()
  
  # Check if the data is empty
  if (nrow(data) == 0) {
    warning(sprintf("Empty file skipped: %s", file_path))
    next # Skip to the next file
  }
  
  # Calculate summary statistics
  stats$summary <- summary(data[, c("lng", "lro", "asm", "afr", "alr", "meno")])
  stats$sd <- as.data.frame(lapply(data[, c("lng", "lro", "asm", "afr", "alr", "meno")], sd, na.rm = TRUE))
  stats$mean <- as.data.frame(lapply(data[, c("lng", "lro", "asm", "afr", "alr", "meno")], mean, na.rm = TRUE))
  stats$cv <- stats$sd / stats$mean
  stats$min <- as.data.frame(lapply(data[, c("lng", "lro", "asm", "afr", "alr", "meno")], min, na.rm = TRUE))
  stats$max <- as.data.frame(lapply(data[, c("lng", "lro", "asm", "afr", "alr", "meno")], max, na.rm = TRUE))
  
  # Handle special cases for CV
  if (nrow(data) == 1 || all(stats$sd$lng == 0 & stats$sd$lro == 0, na.rm = TRUE)) {
    stats$cv[, c("lng", "lro")] <- 0 # Set CV for "lng" and "lro" to 0
    stats$sd[, c("lng", "lro")][is.na(stats$sd[, c("lng", "lro")])] <- 0 # Replace NA SD with 0
  }
  
  # Combine all statistics into a single data frame
  combined_stats <- cbind(
    stat = c("sd", "mean", "cv", "min", "max"),
    do.call(rbind, list(stats$sd, stats$mean, stats$cv, stats$min, stats$max))
  )
  
  # Write the statistics to an output .fst file
  output_file <- file.path(output_dir, sprintf("summary_%s.fst", param_name))
  write_fst(combined_stats, output_file)
}

## Mean sorted by life history trait ----

# Get parameter ranges
d_values <- 1:17
m_values <- 1:19
r_values <- 1:10

### Longevity ----

# Reinitialize lng_mean_s6 dynamically
lng_mean_s6 <- as.data.frame(matrix(NA, nrow = length(d_values), ncol = length(m_values)))
rownames(lng_mean_s6) <- prod_prob[2, ]
colnames(lng_mean_s6) <- transf_prob
lng_mean_s6

# Directory for summary files
summary_dir <- "./RQ_3/lht_summaries_s6"

# Dynamically calculate lng_mean_s6
for (d in 1:nrow(lng_mean_s6)) {
  for (m in 1:ncol(lng_mean_s6)){
    
    lng_values <- c()  # Initialize an empty vector to store lng values for current (d, m)
    
    for (r in 1:length(r_values)) {
      # Construct the file name for the corresponding summary file
      summary_file <- file.path(summary_dir, sprintf("summary_lht_list_d%d_m%d_r%d.fst", d, m, r))
      
      # Check if the summary file exists
      if (file.exists(summary_file)) {
        # Read the summary file
        summary_data <- read_fst(summary_file)
        
        # Extract the mean for "lng" if it exists
        lng_mean <- summary_data[summary_data$stat == "mean" & !is.na(summary_data$lng), "lng"]
        
        # Append lng_mean to the lng_values vector if not empty
        if (length(lng_mean) > 0) {
          lng_values <- c(lng_values, lng_mean)
        }
      } else {
        # File doesn't exist; log a warning
        warning(sprintf("Summary file not found: %s", summary_file))
      }
    }
    
    # Calculate the mean of lng_values if there are any valid values
    if (length(lng_values) > 0) {
      lng_mean_s6[d, m] <- mean(lng_values, na.rm = TRUE)
    } else {
      lng_mean_s6[d, m] <- NA  # Assign NA if no valid values
    }
  }
}
#check it
lng_mean_s6

### Lifetime reproductive ouput ----
# Reinitialize lro_mean_s6 dynamically
lro_mean_s6 <- as.data.frame(matrix(NA, nrow = length(d_values), ncol = length(m_values)))
rownames(lro_mean_s6) <- prod_prob[2, ]
colnames(lro_mean_s6) <- transf_prob
lro_mean_s6

# Directory for summary files
summary_dir <- "./RQ_3/lht_summaries_s6"

# Dynamically calculate lro_mean_s6
for (d in 1:nrow(lro_mean_s6)) {
  for (m in 1:ncol(lro_mean_s6)){
    
    lro_values <- c()  # Initialize an empty vector to store lro values for current (d, m)
    
    for (r in 1:length(r_values)) {
      # Construct the file name for the corresponding summary file
      summary_file <- file.path(summary_dir, sprintf("summary_lht_list_d%d_m%d_r%d.fst", d, m, r))
      
      # Check if the summary file exists
      if (file.exists(summary_file)) {
        # Read the summary file
        summary_data <- read_fst(summary_file)
        
        # Extract the mean for "lro" if it exists
        lro_mean <- summary_data[summary_data$stat == "mean" & !is.na(summary_data$lro), "lro"]
        
        # Append lro_mean to the lro_values vector if not empty
        if (length(lro_mean) > 0) {
          lro_values <- c(lro_values, lro_mean)
        }
      } else {
        # File doesn't exist; log a warning
        warning(sprintf("Summary file not found: %s", summary_file))
      }
    }
    
    # Calculate the mean of lro_values if there are any valid values
    if (length(lro_values) > 0) {
      lro_mean_s6[d, m] <- mean(lro_values, na.rm = TRUE)
    } else {
      lro_mean_s6[d, m] <- NA  # Assign NA if no valid values
    }
  }
}
#check it
lro_mean_s6

### Age at sexual maturity ----

# Reinitialize asm_mean_s6 dynamically
asm_mean_s6 <- as.data.frame(matrix(NA, nrow = length(d_values), ncol = length(m_values)))
rownames(asm_mean_s6) <- prod_prob[2, ]
colnames(asm_mean_s6) <- transf_prob
asm_mean_s6

# Directory for summary files
summary_dir <- "./RQ_3/lht_summaries_s6"

# Dynamically calculate asm_mean_s6
for (d in 1:nrow(asm_mean_s6)) {
  for (m in 1:ncol(asm_mean_s6)){
    
    asm_values <- c()  # Initialize an empty vector to store asm values for current (d, m)
    
    for (r in 1:length(r_values)) {
      # Construct the file name for the corresponding summary file
      summary_file <- file.path(summary_dir, sprintf("summary_lht_list_d%d_m%d_r%d.fst", d, m, r))
      
      # Check if the summary file exists
      if (file.exists(summary_file)) {
        # Read the summary file
        summary_data <- read_fst(summary_file)
        
        # Extract the mean for "asm" if it exists
        asm_mean <- summary_data[summary_data$stat == "mean" & !is.na(summary_data$asm), "asm"]
        
        # Append asm_mean to the asm_values vector if not empty
        if (length(asm_mean) > 0) {
          asm_values <- c(asm_values, asm_mean)
        }
      } else {
        # File doesn't exist; log a warning
        warning(sprintf("Summary file not found: %s", summary_file))
      }
    }
    
    # Calculate the mean of asm_values if there are any valid values
    if (length(asm_values) > 0) {
      asm_mean_s6[d, m] <- mean(asm_values, na.rm = TRUE)
    } else {
      asm_mean_s6[d, m] <- NA  # Assign NA if no valid values
    }
  }
}
#check it
asm_mean_s6

### Age at first reproduction ----

# Reinitialize afr_mean_s6 dynamically
afr_mean_s6 <- as.data.frame(matrix(NA, nrow = length(d_values), ncol = length(m_values)))
rownames(afr_mean_s6) <- prod_prob[2, ]
colnames(afr_mean_s6) <- transf_prob
afr_mean_s6

# Directory for summary files
summary_dir <- "./RQ_3/lht_summaries_s6"

# Dynamically calculate afr_mean_s6
for (d in 1:nrow(afr_mean_s6)) {
  for (m in 1:ncol(afr_mean_s6)){
    
    afr_values <- c()  # Initialize an empty vector to store afr values for current (d, m)
    
    for (r in 1:length(r_values)) {
      # Construct the file name for the corresponding summary file
      summary_file <- file.path(summary_dir, sprintf("summary_lht_list_d%d_m%d_r%d.fst", d, m, r))
      
      # Check if the summary file exists
      if (file.exists(summary_file)) {
        # Read the summary file
        summary_data <- read_fst(summary_file)
        
        # Extract the mean for "afr" if it exists
        afr_mean <- summary_data[summary_data$stat == "mean" & !is.na(summary_data$afr), "afr"]
        
        # Append afr_mean to the afr_values vector if not empty
        if (length(afr_mean) > 0) {
          afr_values <- c(afr_values, afr_mean)
        }
      } else {
        # File doesn't exist; log a warning
        warning(sprintf("Summary file not found: %s", summary_file))
      }
    }
    
    # Calculate the mean of afr_values if there are any valid values
    if (length(afr_values) > 0) {
      afr_mean_s6[d, m] <- mean(afr_values, na.rm = TRUE)
    } else {
      afr_mean_s6[d, m] <- NA  # Assign NA if no valid values
    }
  }
}
#check it
afr_mean_s6

### Age at last reproduction ----

# Reinitialize alr_mean_s6 dynamically
alr_mean_s6 <- as.data.frame(matrix(NA, nrow = length(d_values), ncol = length(m_values)))
rownames(alr_mean_s6) <- prod_prob[2, ]
colnames(alr_mean_s6) <- transf_prob
alr_mean_s6

# Directory for summary files
summary_dir <- "./RQ_3/lht_summaries_s6"

# Dynamically calculate alr_mean_s6
for (d in 1:nrow(alr_mean_s6)) {
  for (m in 1:ncol(alr_mean_s6)){
    
    alr_values <- c()  # Initialize an empty vector to store alr values for current (d, m)
    
    for (r in 1:length(r_values)) {
      # Construct the file name for the corresponding summary file
      summary_file <- file.path(summary_dir, sprintf("summary_lht_list_d%d_m%d_r%d.fst", d, m, r))
      
      # Check if the summary file exists
      if (file.exists(summary_file)) {
        # Read the summary file
        summary_data <- read_fst(summary_file)
        
        # Extract the mean for "alr" if it exists
        alr_mean <- summary_data[summary_data$stat == "mean" & !is.na(summary_data$alr), "alr"]
        
        # Append alr_mean to the alr_values vector if not empty
        if (length(alr_mean) > 0) {
          alr_values <- c(alr_values, alr_mean)
        }
      } else {
        # File doesn't exist; log a warning
        warning(sprintf("Summary file not found: %s", summary_file))
      }
    }
    
    # Calculate the mean of alr_values if there are any valid values
    if (length(alr_values) > 0) {
      alr_mean_s6[d, m] <- mean(alr_values, na.rm = TRUE)
    } else {
      alr_mean_s6[d, m] <- NA  # Assign NA if no valid values
    }
  }
}
#check it
alr_mean_s6

### Age at menopause ----

# Reinitialize meno_mean_s6 dynamically
meno_mean_s6 <- as.data.frame(matrix(NA, nrow = length(d_values), ncol = length(m_values)))
rownames(meno_mean_s6) <- prod_prob[2, ]
colnames(meno_mean_s6) <- transf_prob
meno_mean_s6

# Directory for summary files
summary_dir <- "./RQ_3/lht_summaries_s6"

# Dynamically calculate meno_mean_s6
for (d in 1:nrow(meno_mean_s6)) {
  for (m in 1:ncol(meno_mean_s6)){
    
    meno_values <- c()  # Initialize an empty vector to store meno values for current (d, m)
    
    for (r in 1:length(r_values)) {
      # Construct the file name for the corresponding summary file
      summary_file <- file.path(summary_dir, sprintf("summary_lht_list_d%d_m%d_r%d.fst", d, m, r))
      
      # Check if the summary file exists
      if (file.exists(summary_file)) {
        # Read the summary file
        summary_data <- read_fst(summary_file)
        
        # Extract the mean for "meno" if it exists
        meno_mean <- summary_data[summary_data$stat == "mean" & !is.na(summary_data$meno), "meno"]
        
        # Append meno_mean to the meno_values vector if not empty
        if (length(meno_mean) > 0) {
          meno_values <- c(meno_values, meno_mean)
        }
      } else {
        # File doesn't exist; log a warning
        warning(sprintf("Summary file not found: %s", summary_file))
      }
    }
    
    # Calculate the mean of meno_values if there are any valid values
    if (length(meno_values) > 0) {
      meno_mean_s6[d, m] <- mean(meno_values, na.rm = TRUE)
    } else {
      meno_mean_s6[d, m] <- NA  # Assign NA if no valid values
    }
  }
}
#check it
meno_mean_s6

### Save the data ----

saveRDS(lng_mean_s6,file="./RQ_3/lng_mean_s6.RData")
saveRDS(lro_mean_s6,file="./RQ_3/lro_mean_s6.RData")
saveRDS(asm_mean_s6,file="./RQ_3/asm_mean_s6.RData")
saveRDS(afr_mean_s6,file="./RQ_3/afr_mean_s6.RData")
saveRDS(alr_mean_s6,file="./RQ_3/alr_mean_s6.RData")
saveRDS(meno_mean_s6,file="./RQ_3/meno_mean_s6.RData")

## SD sorted by life history trait ----

# Get parameter ranges
d_values <- 1:17
m_values <- 1:19
r_values <- 1:10

### Longevity ----

# Reinitialize lng_sd_s6 dynamically
lng_sd_s6 <- as.data.frame(matrix(NA, nrow = length(d_values), ncol = length(m_values)))
rownames(lng_sd_s6) <- prod_prob[2, ]
colnames(lng_sd_s6) <- transf_prob
lng_sd_s6

# Directory for summary files
summary_dir <- "./RQ_3/lht_summaries_s6"

# Dynamically calculate lng_sd_s6
for (d in 1:nrow(lng_sd_s6)) {
  for (m in 1:ncol(lng_sd_s6)){
    
    lng_values <- c()  # Initialize an empty vector to store lng values for current (d, m)
    
    for (r in 1:length(r_values)) {
      # Construct the file name for the corresponding summary file
      summary_file <- file.path(summary_dir, sprintf("summary_lht_list_d%d_m%d_r%d.fst", d, m, r))
      
      # Check if the summary file exists
      if (file.exists(summary_file)) {
        # Read the summary file
        summary_data <- read_fst(summary_file)
        
        # Extract the sd for "lng" if it exists
        lng_sd <- summary_data[summary_data$stat == "sd" & !is.na(summary_data$lng), "lng"]
        
        # Append lng_sd to the lng_values vector if not empty
        if (length(lng_sd) > 0) {
          lng_values <- c(lng_values, lng_sd)
        }
      } else {
        # File doesn't exist; log a warning
        warning(sprintf("Summary file not found: %s", summary_file))
      }
    }
    
    # Calculate the sd of lng_values if there are any valid values
    if (length(lng_values) > 0) {
      lng_sd_s6[d, m] <- sd(lng_values, na.rm = TRUE)
    } else {
      lng_sd_s6[d, m] <- NA  # Assign NA if no valid values
    }
  }
}
#check it
lng_sd_s6

### Lifetime reproductive ouput ----
# Reinitialize lro_sd_s6 dynamically
lro_sd_s6 <- as.data.frame(matrix(NA, nrow = length(d_values), ncol = length(m_values)))
rownames(lro_sd_s6) <- prod_prob[2, ]
colnames(lro_sd_s6) <- transf_prob
lro_sd_s6

# Directory for summary files
summary_dir <- "./RQ_3/lht_summaries_s6"

# Dynamically calculate lro_sd_s6
for (d in 1:nrow(lro_sd_s6)) {
  for (m in 1:ncol(lro_sd_s6)){
    
    lro_values <- c()  # Initialize an empty vector to store lro values for current (d, m)
    
    for (r in 1:length(r_values)) {
      # Construct the file name for the corresponding summary file
      summary_file <- file.path(summary_dir, sprintf("summary_lht_list_d%d_m%d_r%d.fst", d, m, r))
      
      # Check if the summary file exists
      if (file.exists(summary_file)) {
        # Read the summary file
        summary_data <- read_fst(summary_file)
        
        # Extract the sd for "lro" if it exists
        lro_sd <- summary_data[summary_data$stat == "sd" & !is.na(summary_data$lro), "lro"]
        
        # Append lro_sd to the lro_values vector if not empty
        if (length(lro_sd) > 0) {
          lro_values <- c(lro_values, lro_sd)
        }
      } else {
        # File doesn't exist; log a warning
        warning(sprintf("Summary file not found: %s", summary_file))
      }
    }
    
    # Calculate the sd of lro_values if there are any valid values
    if (length(lro_values) > 0) {
      lro_sd_s6[d, m] <- sd(lro_values, na.rm = TRUE)
    } else {
      lro_sd_s6[d, m] <- NA  # Assign NA if no valid values
    }
  }
}
#check it
lro_sd_s6

### Age at sexual maturity ----

# Reinitialize asm_sd_s6 dynamically
asm_sd_s6 <- as.data.frame(matrix(NA, nrow = length(d_values), ncol = length(m_values)))
rownames(asm_sd_s6) <- prod_prob[2, ]
colnames(asm_sd_s6) <- transf_prob
asm_sd_s6

# Directory for summary files
summary_dir <- "./RQ_3/lht_summaries_s6"

# Dynamically calculate asm_sd_s6
for (d in 1:nrow(asm_sd_s6)) {
  for (m in 1:ncol(asm_sd_s6)){
    
    asm_values <- c()  # Initialize an empty vector to store asm values for current (d, m)
    
    for (r in 1:length(r_values)) {
      # Construct the file name for the corresponding summary file
      summary_file <- file.path(summary_dir, sprintf("summary_lht_list_d%d_m%d_r%d.fst", d, m, r))
      
      # Check if the summary file exists
      if (file.exists(summary_file)) {
        # Read the summary file
        summary_data <- read_fst(summary_file)
        
        # Extract the sd for "asm" if it exists
        asm_sd <- summary_data[summary_data$stat == "sd" & !is.na(summary_data$asm), "asm"]
        
        # Append asm_sd to the asm_values vector if not empty
        if (length(asm_sd) > 0) {
          asm_values <- c(asm_values, asm_sd)
        }
      } else {
        # File doesn't exist; log a warning
        warning(sprintf("Summary file not found: %s", summary_file))
      }
    }
    
    # Calculate the sd of asm_values if there are any valid values
    if (length(asm_values) > 0) {
      asm_sd_s6[d, m] <- sd(asm_values, na.rm = TRUE)
    } else {
      asm_sd_s6[d, m] <- NA  # Assign NA if no valid values
    }
  }
}
#check it
asm_sd_s6

### Age at first reproduction ----

# Reinitialize afr_sd_s6 dynamically
afr_sd_s6 <- as.data.frame(matrix(NA, nrow = length(d_values), ncol = length(m_values)))
rownames(afr_sd_s6) <- prod_prob[2, ]
colnames(afr_sd_s6) <- transf_prob
afr_sd_s6

# Directory for summary files
summary_dir <- "./RQ_3/lht_summaries_s6"

# Dynamically calculate afr_sd_s6
for (d in 1:nrow(afr_sd_s6)) {
  for (m in 1:ncol(afr_sd_s6)){
    
    afr_values <- c()  # Initialize an empty vector to store afr values for current (d, m)
    
    for (r in 1:length(r_values)) {
      # Construct the file name for the corresponding summary file
      summary_file <- file.path(summary_dir, sprintf("summary_lht_list_d%d_m%d_r%d.fst", d, m, r))
      
      # Check if the summary file exists
      if (file.exists(summary_file)) {
        # Read the summary file
        summary_data <- read_fst(summary_file)
        
        # Extract the sd for "afr" if it exists
        afr_sd <- summary_data[summary_data$stat == "sd" & !is.na(summary_data$afr), "afr"]
        
        # Append afr_sd to the afr_values vector if not empty
        if (length(afr_sd) > 0) {
          afr_values <- c(afr_values, afr_sd)
        }
      } else {
        # File doesn't exist; log a warning
        warning(sprintf("Summary file not found: %s", summary_file))
      }
    }
    
    # Calculate the sd of afr_values if there are any valid values
    if (length(afr_values) > 0) {
      afr_sd_s6[d, m] <- sd(afr_values, na.rm = TRUE)
    } else {
      afr_sd_s6[d, m] <- NA  # Assign NA if no valid values
    }
  }
}
#check it
afr_sd_s6

### Age at last reproduction ----

# Reinitialize alr_sd_s6 dynamically
alr_sd_s6 <- as.data.frame(matrix(NA, nrow = length(d_values), ncol = length(m_values)))
rownames(alr_sd_s6) <- prod_prob[2, ]
colnames(alr_sd_s6) <- transf_prob
alr_sd_s6

# Directory for summary files
summary_dir <- "./RQ_3/lht_summaries_s6"

# Dynamically calculate alr_sd_s6
for (d in 1:nrow(alr_sd_s6)) {
  for (m in 1:ncol(alr_sd_s6)){
    
    alr_values <- c()  # Initialize an empty vector to store alr values for current (d, m)
    
    for (r in 1:length(r_values)) {
      # Construct the file name for the corresponding summary file
      summary_file <- file.path(summary_dir, sprintf("summary_lht_list_d%d_m%d_r%d.fst", d, m, r))
      
      # Check if the summary file exists
      if (file.exists(summary_file)) {
        # Read the summary file
        summary_data <- read_fst(summary_file)
        
        # Extract the sd for "alr" if it exists
        alr_sd <- summary_data[summary_data$stat == "sd" & !is.na(summary_data$alr), "alr"]
        
        # Append alr_sd to the alr_values vector if not empty
        if (length(alr_sd) > 0) {
          alr_values <- c(alr_values, alr_sd)
        }
      } else {
        # File doesn't exist; log a warning
        warning(sprintf("Summary file not found: %s", summary_file))
      }
    }
    
    # Calculate the sd of alr_values if there are any valid values
    if (length(alr_values) > 0) {
      alr_sd_s6[d, m] <- sd(alr_values, na.rm = TRUE)
    } else {
      alr_sd_s6[d, m] <- NA  # Assign NA if no valid values
    }
  }
}
#check it
alr_sd_s6

### Age at menopause ----

# Reinitialize meno_sd_s6 dynamically
meno_sd_s6 <- as.data.frame(matrix(NA, nrow = length(d_values), ncol = length(m_values)))
rownames(meno_sd_s6) <- prod_prob[2, ]
colnames(meno_sd_s6) <- transf_prob
meno_sd_s6

# Directory for summary files
summary_dir <- "./RQ_3/lht_summaries_s6"

# Dynamically calculate meno_sd_s6
for (d in 1:nrow(meno_sd_s6)) {
  for (m in 1:ncol(meno_sd_s6)){
    
    meno_values <- c()  # Initialize an empty vector to store meno values for current (d, m)
    
    for (r in 1:length(r_values)) {
      # Construct the file name for the corresponding summary file
      summary_file <- file.path(summary_dir, sprintf("summary_lht_list_d%d_m%d_r%d.fst", d, m, r))
      
      # Check if the summary file exists
      if (file.exists(summary_file)) {
        # Read the summary file
        summary_data <- read_fst(summary_file)
        
        # Extract the sd for "meno" if it exists
        meno_sd <- summary_data[summary_data$stat == "sd" & !is.na(summary_data$meno), "meno"]
        
        # Append meno_sd to the meno_values vector if not empty
        if (length(meno_sd) > 0) {
          meno_values <- c(meno_values, meno_sd)
        }
      } else {
        # File doesn't exist; log a warning
        warning(sprintf("Summary file not found: %s", summary_file))
      }
    }
    
    # Calculate the sd of meno_values if there are any valid values
    if (length(meno_values) > 0) {
      meno_sd_s6[d, m] <- sd(meno_values, na.rm = TRUE)
    } else {
      meno_sd_s6[d, m] <- NA  # Assign NA if no valid values
    }
  }
}
#check it
meno_sd_s6

### Save the data ----

saveRDS(lng_sd_s6,file="./RQ_3/lng_sd_s6.RData")
saveRDS(lro_sd_s6,file="./RQ_3/lro_sd_s6.RData")
saveRDS(asm_sd_s6,file="./RQ_3/asm_sd_s6.RData")
saveRDS(afr_sd_s6,file="./RQ_3/afr_sd_s6.RData")
saveRDS(alr_sd_s6,file="./RQ_3/alr_sd_s6.RData")
saveRDS(meno_sd_s6,file="./RQ_3/meno_sd_s6.RData")

## Min sorted by life history trait ----

# Get parameter ranges
d_values <- 1:17
m_values <- 1:19
r_values <- 1:10

### Longevity ----

# Reinitialize lng_min_s6 dynamically
lng_min_s6 <- as.data.frame(matrix(NA, nrow = length(d_values), ncol = length(m_values)))
rownames(lng_min_s6) <- prod_prob[2, ]
colnames(lng_min_s6) <- transf_prob
lng_min_s6

# Directory for summary files
summary_dir <- "./RQ_3/lht_summaries_s6"

# Dynamically calculate lng_min_s6
for (d in 1:nrow(lng_min_s6)) {
  for (m in 1:ncol(lng_min_s6)){
    
    lng_values <- c()  # Initialize an empty vector to store lng values for current (d, m)
    
    for (r in 1:length(r_values)) {
      # Construct the file name for the corresponding summary file
      summary_file <- file.path(summary_dir, sprintf("summary_lht_list_d%d_m%d_r%d.fst", d, m, r))
      
      # Check if the summary file exists
      if (file.exists(summary_file)) {
        # Read the summary file
        summary_data <- read_fst(summary_file)
        
        # Extract the min for "lng" if it exists
        lng_min <- summary_data[summary_data$stat == "min" & !is.na(summary_data$lng), "lng"]
        
        # Append lng_min to the lng_values vector if not empty
        if (length(lng_min) > 0) {
          lng_values <- c(lng_values, lng_min)
        }
      } else {
        # File doesn't exist; log a warning
        warning(sprintf("Summary file not found: %s", summary_file))
      }
    }
    
    # Calculate the min of lng_values if there are any valid values
    if (length(lng_values) > 0) {
      lng_min_s6[d, m] <- min(lng_values, na.rm = TRUE)
    } else {
      lng_min_s6[d, m] <- NA  # Assign NA if no valid values
    }
  }
}
#check it
lng_min_s6

### Lifetime reproductive ouput ----
# Reinitialize lro_min_s6 dynamically
lro_min_s6 <- as.data.frame(matrix(NA, nrow = length(d_values), ncol = length(m_values)))
rownames(lro_min_s6) <- prod_prob[2, ]
colnames(lro_min_s6) <- transf_prob
lro_min_s6

# Directory for summary files
summary_dir <- "./RQ_3/lht_summaries_s6"

# Dynamically calculate lro_min_s6
for (d in 1:nrow(lro_min_s6)) {
  for (m in 1:ncol(lro_min_s6)){
    
    lro_values <- c()  # Initialize an empty vector to store lro values for current (d, m)
    
    for (r in 1:length(r_values)) {
      # Construct the file name for the corresponding summary file
      summary_file <- file.path(summary_dir, sprintf("summary_lht_list_d%d_m%d_r%d.fst", d, m, r))
      
      # Check if the summary file exists
      if (file.exists(summary_file)) {
        # Read the summary file
        summary_data <- read_fst(summary_file)
        
        # Extract the min for "lro" if it exists
        lro_min <- summary_data[summary_data$stat == "min" & !is.na(summary_data$lro), "lro"]
        
        # Append lro_min to the lro_values vector if not empty
        if (length(lro_min) > 0) {
          lro_values <- c(lro_values, lro_min)
        }
      } else {
        # File doesn't exist; log a warning
        warning(sprintf("Summary file not found: %s", summary_file))
      }
    }
    
    # Calculate the min of lro_values if there are any valid values
    if (length(lro_values) > 0) {
      lro_min_s6[d, m] <- min(lro_values, na.rm = TRUE)
    } else {
      lro_min_s6[d, m] <- NA  # Assign NA if no valid values
    }
  }
}
#check it
lro_min_s6

### Age at sexual maturity ----

# Reinitialize asm_min_s6 dynamically
asm_min_s6 <- as.data.frame(matrix(NA, nrow = length(d_values), ncol = length(m_values)))
rownames(asm_min_s6) <- prod_prob[2, ]
colnames(asm_min_s6) <- transf_prob
asm_min_s6

# Directory for summary files
summary_dir <- "./RQ_3/lht_summaries_s6"

# Dynamically calculate asm_min_s6
for (d in 1:nrow(asm_min_s6)) {
  for (m in 1:ncol(asm_min_s6)){
    
    asm_values <- c()  # Initialize an empty vector to store asm values for current (d, m)
    
    for (r in 1:length(r_values)) {
      # Construct the file name for the corresponding summary file
      summary_file <- file.path(summary_dir, sprintf("summary_lht_list_d%d_m%d_r%d.fst", d, m, r))
      
      # Check if the summary file exists
      if (file.exists(summary_file)) {
        # Read the summary file
        summary_data <- read_fst(summary_file)
        
        # Extract the min for "asm" if it exists
        asm_min <- summary_data[summary_data$stat == "min" & !is.na(summary_data$asm), "asm"]
        
        # Append asm_min to the asm_values vector if not empty
        if (length(asm_min) > 0) {
          asm_values <- c(asm_values, asm_min)
        }
      } else {
        # File doesn't exist; log a warning
        warning(sprintf("Summary file not found: %s", summary_file))
      }
    }
    
    # Calculate the min of asm_values if there are any valid values
    if (length(asm_values) > 0) {
      asm_min_s6[d, m] <- min(asm_values, na.rm = TRUE)
    } else {
      asm_min_s6[d, m] <- NA  # Assign NA if no valid values
    }
  }
}
#check it
asm_min_s6

### Age at first reproduction ----

# Reinitialize afr_min_s6 dynamically
afr_min_s6 <- as.data.frame(matrix(NA, nrow = length(d_values), ncol = length(m_values)))
rownames(afr_min_s6) <- prod_prob[2, ]
colnames(afr_min_s6) <- transf_prob
afr_min_s6

# Directory for summary files
summary_dir <- "./RQ_3/lht_summaries_s6"

# Dynamically calculate afr_min_s6
for (d in 1:nrow(afr_min_s6)) {
  for (m in 1:ncol(afr_min_s6)){
    
    afr_values <- c()  # Initialize an empty vector to store afr values for current (d, m)
    
    for (r in 1:length(r_values)) {
      # Construct the file name for the corresponding summary file
      summary_file <- file.path(summary_dir, sprintf("summary_lht_list_d%d_m%d_r%d.fst", d, m, r))
      
      # Check if the summary file exists
      if (file.exists(summary_file)) {
        # Read the summary file
        summary_data <- read_fst(summary_file)
        
        # Extract the min for "afr" if it exists
        afr_min <- summary_data[summary_data$stat == "min" & !is.na(summary_data$afr), "afr"]
        
        # Append afr_min to the afr_values vector if not empty
        if (length(afr_min) > 0) {
          afr_values <- c(afr_values, afr_min)
        }
      } else {
        # File doesn't exist; log a warning
        warning(sprintf("Summary file not found: %s", summary_file))
      }
    }
    
    # Calculate the min of afr_values if there are any valid values
    if (length(afr_values) > 0) {
      afr_min_s6[d, m] <- min(afr_values, na.rm = TRUE)
    } else {
      afr_min_s6[d, m] <- NA  # Assign NA if no valid values
    }
  }
}
#check it
afr_min_s6

### Age at last reproduction ----

# Reinitialize alr_min_s6 dynamically
alr_min_s6 <- as.data.frame(matrix(NA, nrow = length(d_values), ncol = length(m_values)))
rownames(alr_min_s6) <- prod_prob[2, ]
colnames(alr_min_s6) <- transf_prob
alr_min_s6

# Directory for summary files
summary_dir <- "./RQ_3/lht_summaries_s6"

# Dynamically calculate alr_min_s6
for (d in 1:nrow(alr_min_s6)) {
  for (m in 1:ncol(alr_min_s6)){
    
    alr_values <- c()  # Initialize an empty vector to store alr values for current (d, m)
    
    for (r in 1:length(r_values)) {
      # Construct the file name for the corresponding summary file
      summary_file <- file.path(summary_dir, sprintf("summary_lht_list_d%d_m%d_r%d.fst", d, m, r))
      
      # Check if the summary file exists
      if (file.exists(summary_file)) {
        # Read the summary file
        summary_data <- read_fst(summary_file)
        
        # Extract the min for "alr" if it exists
        alr_min <- summary_data[summary_data$stat == "min" & !is.na(summary_data$alr), "alr"]
        
        # Append alr_min to the alr_values vector if not empty
        if (length(alr_min) > 0) {
          alr_values <- c(alr_values, alr_min)
        }
      } else {
        # File doesn't exist; log a warning
        warning(sprintf("Summary file not found: %s", summary_file))
      }
    }
    
    # Calculate the min of alr_values if there are any valid values
    if (length(alr_values) > 0) {
      alr_min_s6[d, m] <- min(alr_values, na.rm = TRUE)
    } else {
      alr_min_s6[d, m] <- NA  # Assign NA if no valid values
    }
  }
}
#check it
alr_min_s6

### Age at menopause ----

# Reinitialize meno_min_s6 dynamically
meno_min_s6 <- as.data.frame(matrix(NA, nrow = length(d_values), ncol = length(m_values)))
rownames(meno_min_s6) <- prod_prob[2, ]
colnames(meno_min_s6) <- transf_prob
meno_min_s6

# Directory for summary files
summary_dir <- "./RQ_3/lht_summaries_s6"

# Dynamically calculate meno_min_s6
for (d in 1:nrow(meno_min_s6)) {
  for (m in 1:ncol(meno_min_s6)){
    
    meno_values <- c()  # Initialize an empty vector to store meno values for current (d, m)
    
    for (r in 1:length(r_values)) {
      # Construct the file name for the corresponding summary file
      summary_file <- file.path(summary_dir, sprintf("summary_lht_list_d%d_m%d_r%d.fst", d, m, r))
      
      # Check if the summary file exists
      if (file.exists(summary_file)) {
        # Read the summary file
        summary_data <- read_fst(summary_file)
        
        # Extract the min for "meno" if it exists
        meno_min <- summary_data[summary_data$stat == "min" & !is.na(summary_data$meno), "meno"]
        
        # Append meno_min to the meno_values vector if not empty
        if (length(meno_min) > 0) {
          meno_values <- c(meno_values, meno_min)
        }
      } else {
        # File doesn't exist; log a warning
        warning(sprintf("Summary file not found: %s", summary_file))
      }
    }
    
    # Calculate the min of meno_values if there are any valid values
    if (length(meno_values) > 0) {
      meno_min_s6[d, m] <- min(meno_values, na.rm = TRUE)
    } else {
      meno_min_s6[d, m] <- NA  # Assign NA if no valid values
    }
  }
}
#check it
meno_min_s6

### Save the data ----

saveRDS(lng_min_s6,file="./RQ_3/lng_min_s6.RData")
saveRDS(lro_min_s6,file="./RQ_3/lro_min_s6.RData")
saveRDS(asm_min_s6,file="./RQ_3/asm_min_s6.RData")
saveRDS(afr_min_s6,file="./RQ_3/afr_min_s6.RData")
saveRDS(alr_min_s6,file="./RQ_3/alr_min_s6.RData")
saveRDS(meno_min_s6,file="./RQ_3/meno_min_s6.RData")

## Max sorted by life history trait ----

# Get parameter ranges
d_values <- 1:17
m_values <- 1:19
r_values <- 1:10

### Longevity ----

# Reinitialize lng_max_s6 dynamically
lng_max_s6 <- as.data.frame(matrix(NA, nrow = length(d_values), ncol = length(m_values)))
rownames(lng_max_s6) <- prod_prob[2, ]
colnames(lng_max_s6) <- transf_prob
lng_max_s6

# Directory for summary files
summary_dir <- "./RQ_3/lht_summaries_s6"

# Dynamically calculate lng_max_s6
for (d in 1:nrow(lng_max_s6)) {
  for (m in 1:ncol(lng_max_s6)){
    
    lng_values <- c()  # Initialize an empty vector to store lng values for current (d, m)
    
    for (r in 1:length(r_values)) {
      # Construct the file name for the corresponding summary file
      summary_file <- file.path(summary_dir, sprintf("summary_lht_list_d%d_m%d_r%d.fst", d, m, r))
      
      # Check if the summary file exists
      if (file.exists(summary_file)) {
        # Read the summary file
        summary_data <- read_fst(summary_file)
        
        # Extract the max for "lng" if it exists
        lng_max <- summary_data[summary_data$stat == "max" & !is.na(summary_data$lng), "lng"]
        
        # Append lng_max to the lng_values vector if not empty
        if (length(lng_max) > 0) {
          lng_values <- c(lng_values, lng_max)
        }
      } else {
        # File doesn't exist; log a warning
        warning(sprintf("Summary file not found: %s", summary_file))
      }
    }
    
    # Calculate the max of lng_values if there are any valid values
    if (length(lng_values) > 0) {
      lng_max_s6[d, m] <- max(lng_values, na.rm = TRUE)
    } else {
      lng_max_s6[d, m] <- NA  # Assign NA if no valid values
    }
  }
}
#check it
lng_max_s6

### Lifetime reproductive ouput ----
# Reinitialize lro_max_s6 dynamically
lro_max_s6 <- as.data.frame(matrix(NA, nrow = length(d_values), ncol = length(m_values)))
rownames(lro_max_s6) <- prod_prob[2, ]
colnames(lro_max_s6) <- transf_prob
lro_max_s6

# Directory for summary files
summary_dir <- "./RQ_3/lht_summaries_s6"

# Dynamically calculate lro_max_s6
for (d in 1:nrow(lro_max_s6)) {
  for (m in 1:ncol(lro_max_s6)){
    
    lro_values <- c()  # Initialize an empty vector to store lro values for current (d, m)
    
    for (r in 1:length(r_values)) {
      # Construct the file name for the corresponding summary file
      summary_file <- file.path(summary_dir, sprintf("summary_lht_list_d%d_m%d_r%d.fst", d, m, r))
      
      # Check if the summary file exists
      if (file.exists(summary_file)) {
        # Read the summary file
        summary_data <- read_fst(summary_file)
        
        # Extract the max for "lro" if it exists
        lro_max <- summary_data[summary_data$stat == "max" & !is.na(summary_data$lro), "lro"]
        
        # Append lro_max to the lro_values vector if not empty
        if (length(lro_max) > 0) {
          lro_values <- c(lro_values, lro_max)
        }
      } else {
        # File doesn't exist; log a warning
        warning(sprintf("Summary file not found: %s", summary_file))
      }
    }
    
    # Calculate the max of lro_values if there are any valid values
    if (length(lro_values) > 0) {
      lro_max_s6[d, m] <- max(lro_values, na.rm = TRUE)
    } else {
      lro_max_s6[d, m] <- NA  # Assign NA if no valid values
    }
  }
}
#check it
lro_max_s6

### Age at sexual maturity ----

# Reinitialize asm_max_s6 dynamically
asm_max_s6 <- as.data.frame(matrix(NA, nrow = length(d_values), ncol = length(m_values)))
rownames(asm_max_s6) <- prod_prob[2, ]
colnames(asm_max_s6) <- transf_prob
asm_max_s6

# Directory for summary files
summary_dir <- "./RQ_3/lht_summaries_s6"

# Dynamically calculate asm_max_s6
for (d in 1:nrow(asm_max_s6)) {
  for (m in 1:ncol(asm_max_s6)){
    
    asm_values <- c()  # Initialize an empty vector to store asm values for current (d, m)
    
    for (r in 1:length(r_values)) {
      # Construct the file name for the corresponding summary file
      summary_file <- file.path(summary_dir, sprintf("summary_lht_list_d%d_m%d_r%d.fst", d, m, r))
      
      # Check if the summary file exists
      if (file.exists(summary_file)) {
        # Read the summary file
        summary_data <- read_fst(summary_file)
        
        # Extract the max for "asm" if it exists
        asm_max <- summary_data[summary_data$stat == "max" & !is.na(summary_data$asm), "asm"]
        
        # Append asm_max to the asm_values vector if not empty
        if (length(asm_max) > 0) {
          asm_values <- c(asm_values, asm_max)
        }
      } else {
        # File doesn't exist; log a warning
        warning(sprintf("Summary file not found: %s", summary_file))
      }
    }
    
    # Calculate the max of asm_values if there are any valid values
    if (length(asm_values) > 0) {
      asm_max_s6[d, m] <- max(asm_values, na.rm = TRUE)
    } else {
      asm_max_s6[d, m] <- NA  # Assign NA if no valid values
    }
  }
}
#check it
asm_max_s6

### Age at first reproduction ----

# Reinitialize afr_max_s6 dynamically
afr_max_s6 <- as.data.frame(matrix(NA, nrow = length(d_values), ncol = length(m_values)))
rownames(afr_max_s6) <- prod_prob[2, ]
colnames(afr_max_s6) <- transf_prob
afr_max_s6

# Directory for summary files
summary_dir <- "./RQ_3/lht_summaries_s6"

# Dynamically calculate afr_max_s6
for (d in 1:nrow(afr_max_s6)) {
  for (m in 1:ncol(afr_max_s6)){
    
    afr_values <- c()  # Initialize an empty vector to store afr values for current (d, m)
    
    for (r in 1:length(r_values)) {
      # Construct the file name for the corresponding summary file
      summary_file <- file.path(summary_dir, sprintf("summary_lht_list_d%d_m%d_r%d.fst", d, m, r))
      
      # Check if the summary file exists
      if (file.exists(summary_file)) {
        # Read the summary file
        summary_data <- read_fst(summary_file)
        
        # Extract the max for "afr" if it exists
        afr_max <- summary_data[summary_data$stat == "max" & !is.na(summary_data$afr), "afr"]
        
        # Append afr_max to the afr_values vector if not empty
        if (length(afr_max) > 0) {
          afr_values <- c(afr_values, afr_max)
        }
      } else {
        # File doesn't exist; log a warning
        warning(sprintf("Summary file not found: %s", summary_file))
      }
    }
    
    # Calculate the max of afr_values if there are any valid values
    if (length(afr_values) > 0) {
      afr_max_s6[d, m] <- max(afr_values, na.rm = TRUE)
    } else {
      afr_max_s6[d, m] <- NA  # Assign NA if no valid values
    }
  }
}
#check it
afr_max_s6

### Age at last reproduction ----

# Reinitialize alr_max_s6 dynamically
alr_max_s6 <- as.data.frame(matrix(NA, nrow = length(d_values), ncol = length(m_values)))
rownames(alr_max_s6) <- prod_prob[2, ]
colnames(alr_max_s6) <- transf_prob
alr_max_s6

# Directory for summary files
summary_dir <- "./RQ_3/lht_summaries_s6"

# Dynamically calculate alr_max_s6
for (d in 1:nrow(alr_max_s6)) {
  for (m in 1:ncol(alr_max_s6)){
    
    alr_values <- c()  # Initialize an empty vector to store alr values for current (d, m)
    
    for (r in 1:length(r_values)) {
      # Construct the file name for the corresponding summary file
      summary_file <- file.path(summary_dir, sprintf("summary_lht_list_d%d_m%d_r%d.fst", d, m, r))
      
      # Check if the summary file exists
      if (file.exists(summary_file)) {
        # Read the summary file
        summary_data <- read_fst(summary_file)
        
        # Extract the max for "alr" if it exists
        alr_max <- summary_data[summary_data$stat == "max" & !is.na(summary_data$alr), "alr"]
        
        # Append alr_max to the alr_values vector if not empty
        if (length(alr_max) > 0) {
          alr_values <- c(alr_values, alr_max)
        }
      } else {
        # File doesn't exist; log a warning
        warning(sprintf("Summary file not found: %s", summary_file))
      }
    }
    
    # Calculate the max of alr_values if there are any valid values
    if (length(alr_values) > 0) {
      alr_max_s6[d, m] <- max(alr_values, na.rm = TRUE)
    } else {
      alr_max_s6[d, m] <- NA  # Assign NA if no valid values
    }
  }
}
#check it
alr_max_s6

### Age at menopause ----

# Reinitialize meno_max_s6 dynamically
meno_max_s6 <- as.data.frame(matrix(NA, nrow = length(d_values), ncol = length(m_values)))
rownames(meno_max_s6) <- prod_prob[2, ]
colnames(meno_max_s6) <- transf_prob
meno_max_s6

# Directory for summary files
summary_dir <- "./RQ_3/lht_summaries_s6"

# Dynamically calculate meno_max_s6
for (d in 1:nrow(meno_max_s6)) {
  for (m in 1:ncol(meno_max_s6)){
    
    meno_values <- c()  # Initialize an empty vector to store meno values for current (d, m)
    
    for (r in 1:length(r_values)) {
      # Construct the file name for the corresponding summary file
      summary_file <- file.path(summary_dir, sprintf("summary_lht_list_d%d_m%d_r%d.fst", d, m, r))
      
      # Check if the summary file exists
      if (file.exists(summary_file)) {
        # Read the summary file
        summary_data <- read_fst(summary_file)
        
        # Extract the max for "meno" if it exists
        meno_max <- summary_data[summary_data$stat == "max" & !is.na(summary_data$meno), "meno"]
        
        # Append meno_max to the meno_values vector if not empty
        if (length(meno_max) > 0) {
          meno_values <- c(meno_values, meno_max)
        }
      } else {
        # File doesn't exist; log a warning
        warning(sprintf("Summary file not found: %s", summary_file))
      }
    }
    
    # Calculate the max of meno_values if there are any valid values
    if (length(meno_values) > 0) {
      meno_max_s6[d, m] <- max(meno_values, na.rm = TRUE)
    } else {
      meno_max_s6[d, m] <- NA  # Assign NA if no valid values
    }
  }
}
#check it
meno_max_s6

### Save the data ----

saveRDS(lng_max_s6,file="./RQ_3/lng_max_s6.RData")
saveRDS(lro_max_s6,file="./RQ_3/lro_max_s6.RData")
saveRDS(asm_max_s6,file="./RQ_3/asm_max_s6.RData")
saveRDS(afr_max_s6,file="./RQ_3/afr_max_s6.RData")
saveRDS(alr_max_s6,file="./RQ_3/alr_max_s6.RData")
saveRDS(meno_max_s6,file="./RQ_3/meno_max_s6.RData")

## CV sorted by life history trait ----

# Get parameter ranges
d_values <- 1:17
m_values <- 1:19
r_values <- 1:10

### Longevity ----

# Reinitialize lng_cv_s6 dynamically
lng_cv_s6 <- as.data.frame(matrix(NA, nrow = length(d_values), ncol = length(m_values)))
rownames(lng_cv_s6) <- prod_prob[2, ]
colnames(lng_cv_s6) <- transf_prob
lng_cv_s6

# Directory for summary files
summary_dir <- "./RQ_3/lht_summaries_s6"

# Dynamically calculate lng_cv_s6
for (d in 1:nrow(lng_cv_s6)) {
  for (m in 1:ncol(lng_cv_s6)){
    
    lng_values <- c()  # Initialize an empty vector to store lng values for current (d, m)
    
    for (r in 1:length(r_values)) {
      # Construct the file name for the corresponding summary file
      summary_file <- file.path(summary_dir, sprintf("summary_lht_list_d%d_m%d_r%d.fst", d, m, r))
      
      # Check if the summary file exists
      if (file.exists(summary_file)) {
        # Read the summary file
        summary_data <- read_fst(summary_file)
        
        # Extract the cv for "lng" if it exists
        lng_cv <- summary_data[summary_data$stat == "cv" & !is.na(summary_data$lng), "lng"]
        
        # Append lng_cv to the lng_values vector if not empty
        if (length(lng_cv) > 0) {
          lng_values <- c(lng_values, lng_cv)
        }
      } else {
        # File doesn't exist; log a warning
        warning(sprintf("Summary file not found: %s", summary_file))
      }
    }
    
    # Calculate the cv of lng_values if there are any valid values
    if (length(lng_values) > 0) {
      lng_cv_s6[d, m] <- cv(lng_values, na.rm = TRUE)
    } else {
      lng_cv_s6[d, m] <- NA  # Assign NA if no valid values
    }
  }
}
#check it
lng_cv_s6

### Lifetime reproductive ouput ----
# Reinitialize lro_cv_s6 dynamically
lro_cv_s6 <- as.data.frame(matrix(NA, nrow = length(d_values), ncol = length(m_values)))
rownames(lro_cv_s6) <- prod_prob[2, ]
colnames(lro_cv_s6) <- transf_prob
lro_cv_s6

# Directory for summary files
summary_dir <- "./RQ_3/lht_summaries_s6"

# Dynamically calculate lro_cv_s6
for (d in 1:nrow(lro_cv_s6)) {
  for (m in 1:ncol(lro_cv_s6)){
    
    lro_values <- c()  # Initialize an empty vector to store lro values for current (d, m)
    
    for (r in 1:length(r_values)) {
      # Construct the file name for the corresponding summary file
      summary_file <- file.path(summary_dir, sprintf("summary_lht_list_d%d_m%d_r%d.fst", d, m, r))
      
      # Check if the summary file exists
      if (file.exists(summary_file)) {
        # Read the summary file
        summary_data <- read_fst(summary_file)
        
        # Extract the cv for "lro" if it exists
        lro_cv <- summary_data[summary_data$stat == "cv" & !is.na(summary_data$lro), "lro"]
        
        # Append lro_cv to the lro_values vector if not empty
        if (length(lro_cv) > 0) {
          lro_values <- c(lro_values, lro_cv)
        }
      } else {
        # File doesn't exist; log a warning
        warning(sprintf("Summary file not found: %s", summary_file))
      }
    }
    
    # Calculate the cv of lro_values if there are any valid values
    if (length(lro_values) > 0) {
      lro_cv_s6[d, m] <- cv(lro_values, na.rm = TRUE)
    } else {
      lro_cv_s6[d, m] <- NA  # Assign NA if no valid values
    }
  }
}
#check it
lro_cv_s6

### Age at sexual maturity ----

# Reinitialize asm_cv_s6 dynamically
asm_cv_s6 <- as.data.frame(matrix(NA, nrow = length(d_values), ncol = length(m_values)))
rownames(asm_cv_s6) <- prod_prob[2, ]
colnames(asm_cv_s6) <- transf_prob
asm_cv_s6

# Directory for summary files
summary_dir <- "./RQ_3/lht_summaries_s6"

# Dynamically calculate asm_cv_s6
for (d in 1:nrow(asm_cv_s6)) {
  for (m in 1:ncol(asm_cv_s6)){
    
    asm_values <- c()  # Initialize an empty vector to store asm values for current (d, m)
    
    for (r in 1:length(r_values)) {
      # Construct the file name for the corresponding summary file
      summary_file <- file.path(summary_dir, sprintf("summary_lht_list_d%d_m%d_r%d.fst", d, m, r))
      
      # Check if the summary file exists
      if (file.exists(summary_file)) {
        # Read the summary file
        summary_data <- read_fst(summary_file)
        
        # Extract the cv for "asm" if it exists
        asm_cv <- summary_data[summary_data$stat == "cv" & !is.na(summary_data$asm), "asm"]
        
        # Append asm_cv to the asm_values vector if not empty
        if (length(asm_cv) > 0) {
          asm_values <- c(asm_values, asm_cv)
        }
      } else {
        # File doesn't exist; log a warning
        warning(sprintf("Summary file not found: %s", summary_file))
      }
    }
    
    # Calculate the cv of asm_values if there are any valid values
    if (length(asm_values) > 0) {
      asm_cv_s6[d, m] <- cv(asm_values, na.rm = TRUE)
    } else {
      asm_cv_s6[d, m] <- NA  # Assign NA if no valid values
    }
  }
}
#check it
asm_cv_s6

### Age at first reproduction ----

# Reinitialize afr_cv_s6 dynamically
afr_cv_s6 <- as.data.frame(matrix(NA, nrow = length(d_values), ncol = length(m_values)))
rownames(afr_cv_s6) <- prod_prob[2, ]
colnames(afr_cv_s6) <- transf_prob
afr_cv_s6

# Directory for summary files
summary_dir <- "./RQ_3/lht_summaries_s6"

# Dynamically calculate afr_cv_s6
for (d in 1:nrow(afr_cv_s6)) {
  for (m in 1:ncol(afr_cv_s6)){
    
    afr_values <- c()  # Initialize an empty vector to store afr values for current (d, m)
    
    for (r in 1:length(r_values)) {
      # Construct the file name for the corresponding summary file
      summary_file <- file.path(summary_dir, sprintf("summary_lht_list_d%d_m%d_r%d.fst", d, m, r))
      
      # Check if the summary file exists
      if (file.exists(summary_file)) {
        # Read the summary file
        summary_data <- read_fst(summary_file)
        
        # Extract the cv for "afr" if it exists
        afr_cv <- summary_data[summary_data$stat == "cv" & !is.na(summary_data$afr), "afr"]
        
        # Append afr_cv to the afr_values vector if not empty
        if (length(afr_cv) > 0) {
          afr_values <- c(afr_values, afr_cv)
        }
      } else {
        # File doesn't exist; log a warning
        warning(sprintf("Summary file not found: %s", summary_file))
      }
    }
    
    # Calculate the cv of afr_values if there are any valid values
    if (length(afr_values) > 0) {
      afr_cv_s6[d, m] <- cv(afr_values, na.rm = TRUE)
    } else {
      afr_cv_s6[d, m] <- NA  # Assign NA if no valid values
    }
  }
}
#check it
afr_cv_s6

### Age at last reproduction ----

# Reinitialize alr_cv_s6 dynamically
alr_cv_s6 <- as.data.frame(matrix(NA, nrow = length(d_values), ncol = length(m_values)))
rownames(alr_cv_s6) <- prod_prob[2, ]
colnames(alr_cv_s6) <- transf_prob
alr_cv_s6

# Directory for summary files
summary_dir <- "./RQ_3/lht_summaries_s6"

# Dynamically calculate alr_cv_s6
for (d in 1:nrow(alr_cv_s6)) {
  for (m in 1:ncol(alr_cv_s6)){
    
    alr_values <- c()  # Initialize an empty vector to store alr values for current (d, m)
    
    for (r in 1:length(r_values)) {
      # Construct the file name for the corresponding summary file
      summary_file <- file.path(summary_dir, sprintf("summary_lht_list_d%d_m%d_r%d.fst", d, m, r))
      
      # Check if the summary file exists
      if (file.exists(summary_file)) {
        # Read the summary file
        summary_data <- read_fst(summary_file)
        
        # Extract the cv for "alr" if it exists
        alr_cv <- summary_data[summary_data$stat == "cv" & !is.na(summary_data$alr), "alr"]
        
        # Append alr_cv to the alr_values vector if not empty
        if (length(alr_cv) > 0) {
          alr_values <- c(alr_values, alr_cv)
        }
      } else {
        # File doesn't exist; log a warning
        warning(sprintf("Summary file not found: %s", summary_file))
      }
    }
    
    # Calculate the cv of alr_values if there are any valid values
    if (length(alr_values) > 0) {
      alr_cv_s6[d, m] <- cv(alr_values, na.rm = TRUE)
    } else {
      alr_cv_s6[d, m] <- NA  # Assign NA if no valid values
    }
  }
}
#check it
alr_cv_s6

### Age at menopause ----

# Reinitialize meno_cv_s6 dynamically
meno_cv_s6 <- as.data.frame(matrix(NA, nrow = length(d_values), ncol = length(m_values)))
rownames(meno_cv_s6) <- prod_prob[2, ]
colnames(meno_cv_s6) <- transf_prob
meno_cv_s6

# Directory for summary files
summary_dir <- "./RQ_3/lht_summaries_s6"

# Dynamically calculate meno_cv_s6
for (d in 1:nrow(meno_cv_s6)) {
  for (m in 1:ncol(meno_cv_s6)){
    
    meno_values <- c()  # Initialize an empty vector to store meno values for current (d, m)
    
    for (r in 1:length(r_values)) {
      # Construct the file name for the corresponding summary file
      summary_file <- file.path(summary_dir, sprintf("summary_lht_list_d%d_m%d_r%d.fst", d, m, r))
      
      # Check if the summary file exists
      if (file.exists(summary_file)) {
        # Read the summary file
        summary_data <- read_fst(summary_file)
        
        # Extract the cv for "meno" if it exists
        meno_cv <- summary_data[summary_data$stat == "cv" & !is.na(summary_data$meno), "meno"]
        
        # Append meno_cv to the meno_values vector if not empty
        if (length(meno_cv) > 0) {
          meno_values <- c(meno_values, meno_cv)
        }
      } else {
        # File doesn't exist; log a warning
        warning(sprintf("Summary file not found: %s", summary_file))
      }
    }
    
    # Calculate the cv of meno_values if there are any valid values
    if (length(meno_values) > 0) {
      meno_cv_s6[d, m] <- cv(meno_values, na.rm = TRUE)
    } else {
      meno_cv_s6[d, m] <- NA  # Assign NA if no valid values
    }
  }
}
#check it
meno_cv_s6

### Save the data ----

saveRDS(lng_cv_s6,file="./RQ_3/lng_cv_s6.RData")
saveRDS(lro_cv_s6,file="./RQ_3/lro_cv_s6.RData")
saveRDS(asm_cv_s6,file="./RQ_3/asm_cv_s6.RData")
saveRDS(afr_cv_s6,file="./RQ_3/afr_cv_s6.RData")
saveRDS(alr_cv_s6,file="./RQ_3/alr_cv_s6.RData")
saveRDS(meno_cv_s6,file="./RQ_3/meno_cv_s6.RData")

# ##Average life cycle ----
# 
# ###Longevity ----
# 
# #p = 0.75
# apply(lng_mean_s6[2:11],1,mean,na.rm=T)[which(lng_mean_s6$p == 0.75)]
# apply(lng_sd_s6[2:11],1,mean,na.rm=T)[which(lng_mean_s6$p == 0.75)]
# apply(lng_min_s6[2:11],1,mean,na.rm=T)[which(lng_mean_s6$p == 0.75)]
# apply(lng_max_s6[2:11],1,mean,na.rm=T)[which(lng_mean_s6$p == 0.75)]
# #p = 0.55
# apply(lng_mean_s6[2:11],1,mean,na.rm=T)[which(lng_mean_s6$p == 0.6)]
# apply(lng_sd_s6[2:11],1,mean,na.rm=T)[which(lng_mean_s6$p == 0.6)]
# apply(lng_min_s6[2:11],1,mean,na.rm=T)[which(lng_mean_s6$p == 0.6)]
# apply(lng_max_s6[2:11],1,mean,na.rm=T)[which(lng_mean_s6$p == 0.6)]
# #p = 0.90
# apply(lng_mean_s6[2:11],1,mean,na.rm=T)[which(lng_mean_s6$p == 0.9)]
# apply(lng_sd_s6[2:11],1,mean,na.rm=T)[which(lng_mean_s6$p == 0.9)]
# apply(lng_min_s6[2:11],1,mean,na.rm=T)[which(lng_mean_s6$p == 0.9)]
# apply(lng_max_s6[2:11],1,mean,na.rm=T)[which(lng_mean_s6$p == 0.9)]
# 
# ###Lifetime reproductive output ----
# 
# #p = 0.7
# apply(lro_mean_s6[2:11],1,mean,na.rm=T)[which(lro_mean_s6$p == 0.75)]
# apply(lro_sd_s6[2:11],1,mean,na.rm=T)[which(lro_mean_s6$p == 0.75)]
# apply(lro_min_s6[2:11],1,mean,na.rm=T)[which(lro_mean_s6$p == 0.75)]
# apply(lro_max_s6[2:11],1,mean,na.rm=T)[which(lro_mean_s6$p == 0.75)]
# #p = 0.55
# apply(lro_mean_s6[2:11],1,mean,na.rm=T)[which(lro_mean_s6$p == 0.6)]
# apply(lro_sd_s6[2:11],1,mean,na.rm=T)[which(lro_mean_s6$p == 0.6)]
# apply(lro_min_s6[2:11],1,mean,na.rm=T)[which(lro_mean_s6$p == 0.6)]
# apply(lro_max_s6[2:11],1,mean,na.rm=T)[which(lro_mean_s6$p == 0.6)]
# #p = 0.9
# apply(lro_mean_s6[2:11],1,mean,na.rm=T)[which(lro_mean_s6$p == 0.9)]
# apply(lro_sd_s6[2:11],1,mean,na.rm=T)[which(lro_mean_s6$p == 0.9)]
# apply(lro_min_s6[2:11],1,mean,na.rm=T)[which(lro_mean_s6$p == 0.9)]
# apply(lro_max_s6[2:11],1,mean,na.rm=T)[which(lro_mean_s6$p == 0.9)]
# 
# ###Age at sexual maturity ----
# 
# #p = 0.75
# apply(asm_mean_s6[2:11],1,mean,na.rm=T)[which(asm_mean_s6$p == 0.75)]
# apply(asm_sd_s6[2:11],1,mean,na.rm=T)[which(asm_mean_s6$p == 0.75)]
# apply(asm_min_s6[2:11],1,mean,na.rm=T)[which(asm_mean_s6$p == 0.75)]
# apply(asm_max_s6[2:11],1,mean,na.rm=T)[which(asm_mean_s6$p == 0.75)]
# #p = 0.55
# apply(asm_mean_s6[2:11],1,mean,na.rm=T)[which(asm_mean_s6$p == 0.65)]
# apply(asm_sd_s6[2:11],1,mean,na.rm=T)[which(asm_mean_s6$p == 0.65)]
# apply(asm_min_s6[2:11],1,mean,na.rm=T)[which(asm_mean_s6$p == 0.65)]
# apply(asm_max_s6[2:11],1,mean,na.rm=T)[which(asm_mean_s6$p == 0.65)]
# #p = 0.9
# apply(asm_mean_s6[2:11],1,mean,na.rm=T)[which(asm_mean_s6$p == 0.9)]
# apply(asm_sd_s6[2:11],1,mean,na.rm=T)[which(asm_mean_s6$p == 0.9)]
# apply(asm_min_s6[2:11],1,mean,na.rm=T)[which(asm_mean_s6$p == 0.9)]
# apply(asm_max_s6[2:11],1,mean,na.rm=T)[which(asm_mean_s6$p == 0.9)]
# 
# ###Age at first reproduction ----
# 
# #p = 0.75
# apply(afr_mean_s6[2:11],1,mean,na.rm=T)[which(afr_mean_s6$p == 0.75)]
# apply(afr_sd_s6[2:11],1,mean,na.rm=T)[which(afr_mean_s6$p == 0.75)]
# apply(afr_min_s6[2:11],1,mean,na.rm=T)[which(afr_mean_s6$p == 0.75)]
# apply(afr_max_s6[2:11],1,mean,na.rm=T)[which(afr_mean_s6$p == 0.75)]
# #p = 0.55
# apply(afr_mean_s6[2:11],1,mean,na.rm=T)[which(afr_mean_s6$p == 0.65)]
# apply(afr_sd_s6[2:11],1,mean,na.rm=T)[which(afr_mean_s6$p == 0.65)]
# apply(afr_min_s6[2:11],1,mean,na.rm=T)[which(afr_mean_s6$p == 0.65)]
# apply(afr_max_s6[2:11],1,mean,na.rm=T)[which(afr_mean_s6$p == 0.65)]
# #p = 0.9
# apply(afr_mean_s6[2:11],1,mean,na.rm=T)[which(afr_mean_s6$p == 0.9)]
# apply(afr_sd_s6[2:11],1,mean,na.rm=T)[which(afr_mean_s6$p == 0.9)]
# apply(afr_min_s6[2:11],1,mean,na.rm=T)[which(afr_mean_s6$p == 0.9)]
# apply(afr_max_s6[2:11],1,mean,na.rm=T)[which(afr_mean_s6$p == 0.9)]
# 
# ###Age at last reproduction ----
# 
# #p = 0.7
# apply(alr_mean_s6[2:11],1,mean,na.rm=T)[which(alr_mean_s6$p == 0.75)]
# apply(alr_sd_s6[2:11],1,mean,na.rm=T)[which(alr_mean_s6$p == 0.75)]
# apply(alr_min_s6[2:11],1,mean,na.rm=T)[which(alr_mean_s6$p == 0.75)]
# apply(alr_max_s6[2:11],1,mean,na.rm=T)[which(alr_mean_s6$p == 0.75)]
# #p = 0.55
# apply(alr_mean_s6[2:11],1,mean,na.rm=T)[which(alr_mean_s6$p == 0.65)]
# apply(alr_sd_s6[2:11],1,mean,na.rm=T)[which(alr_mean_s6$p == 0.65)]
# apply(alr_min_s6[2:11],1,mean,na.rm=T)[which(alr_mean_s6$p == 0.65)]
# apply(alr_max_s6[2:11],1,mean,na.rm=T)[which(alr_mean_s6$p == 0.65)]
# #p = 0.9
# apply(alr_mean_s6[2:11],1,mean,na.rm=T)[which(alr_mean_s6$p == 0.9)]
# apply(alr_sd_s6[2:11],1,mean,na.rm=T)[which(alr_mean_s6$p == 0.9)]
# apply(alr_min_s6[2:11],1,mean,na.rm=T)[which(alr_mean_s6$p == 0.9)]
# apply(alr_max_s6[2:11],1,mean,na.rm=T)[which(alr_mean_s6$p == 0.9)]
# 
# ###Age at menopause ----
# 
# #p = 0.7
# apply(meno_mean_s6[2:11],1,mean,na.rm=T)[which(meno_mean_s6$p == 0.75)]
# apply(meno_sd_s6[2:11],1,mean,na.rm=T)[which(meno_mean_s6$p == 0.75)]
# apply(meno_min_s6[2:11],1,mean,na.rm=T)[which(meno_mean_s6$p == 0.75)]
# apply(meno_max_s6[2:11],1,mean,na.rm=T)[which(meno_mean_s6$p == 0.75)]
# #p = 0.55
# apply(meno_mean_s6[2:11],1,mean,na.rm=T)[which(meno_mean_s6$p == 0.65)]
# apply(meno_sd_s6[2:11],1,mean,na.rm=T)[which(meno_mean_s6$p == 0.65)]
# apply(meno_min_s6[2:11],1,mean,na.rm=T)[which(meno_mean_s6$p == 0.65)]
# apply(meno_max_s6[2:11],1,mean,na.rm=T)[which(meno_mean_s6$p == 0.65)]
# #p = 0.9
# apply(meno_mean_s6[2:11],1,mean,na.rm=T)[which(meno_mean_s6$p == 0.9)]
# apply(meno_sd_s6[2:11],1,mean,na.rm=T)[which(meno_mean_s6$p == 0.9)]
# apply(meno_min_s6[2:11],1,mean,na.rm=T)[which(meno_mean_s6$p == 0.9)]
# apply(meno_max_s6[2:11],1,mean,na.rm=T)[which(meno_mean_s6$p == 0.9)]
# 

