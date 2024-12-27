# Life cycle variation and resource dynamics ABM: Scenario 3 - results_s3 ----

#Here is the code to get the summary statistics and plots from the simulation of Scenario 3, which aims to understand the influence of resource production on the variability of life cycles.

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

#Scenario 3 ----

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

transf_prob <- round(seq((blockmatrix[i, j] - 0.2), (blockmatrix[i, j] + 0.2), length = 19), 2)

# Directory containing the input .fst files
input_dir <- "./Scenario_3/lht_lists"

# Directory to save the output .fst files
output_dir <- "./RQ_3/lht_summaries_s3"

# Create the output directory if it doesn't exist
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Get all file names in the directory
file_names <- list.files(input_dir, pattern = "lht_list_m\\d+_r\\d+\\.fst", full.names = TRUE)

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
m_values <- 1:19
r_values <- 1:10

### Longevity ----

# Reinitialize lng_mean_s3 dynamically
lng_mean_s3 <- as.data.frame(matrix(NA, nrow = length(m_values), ncol = length(r_values) + 1))
colnames(lng_mean_s3) <- c("m", paste0("r", r_values))
lng_mean_s3$m <- transf_prob # Update to match your specific production probabilities

# Directory for summary files
summary_dir <- "./RQ_3/lht_summaries_s3"

# Dynamically calculate lng_mean_s3
for (m in 1:nrow(lng_mean_s3)) {
  for (r in 1:10) {
    # Construct the file name for the corresponding summary file
    summary_file <- file.path(summary_dir, sprintf("summary_lht_list_m%d_r%d.fst", m, r))
    
    # Check if the summary file exists
    if (file.exists(summary_file)) {
      # Read the summary file
      summary_data <- read_fst(summary_file)
      
      # Extract the mean for "lng" if it exists
      lng_mean <- summary_data[summary_data$stat == "mean" & !is.na(summary_data$lng), "lng"]
      
      # Assign the value to lng_mean_s3
      if (length(lng_mean) > 0) {
        lng_mean_s3[m, r + 1] <- lng_mean
      } else {
        lng_mean_s3[m, r + 1] <- NA
      }
    } else {
      # File doesn't exist; log a warning and set to NA
      warning(sprintf("Summary file not found: %s", summary_file))
      lng_mean_s3[m, r + 1] <- NA
    }
  }
}
#check it
lng_mean_s3

### Lifetime reproductive ouput ----
# Reinitialize lro_mean_s3 dynamically
lro_mean_s3 <- as.data.frame(matrix(NA, nrow = length(m_values), ncol = length(r_values) + 1))
colnames(lro_mean_s3) <- c("m", paste0("r", r_values))
lro_mean_s3$m <- transf_prob # Update to match your specific production probabilities

# Directory for summary files
summary_dir <- "./RQ_3/lht_summaries_s3"

# Dynamically calculate lro_mean_s3
for (m in 1:nrow(lro_mean_s3)) {
  for (r in 1:10) {
    # Construct the file name for the corresponding summary file
    summary_file <- file.path(summary_dir, sprintf("summary_lht_list_m%d_r%d.fst", m, r))
    
    # Check if the summary file exists
    if (file.exists(summary_file)) {
      # Read the summary file
      summary_data <- read_fst(summary_file)
      
      # Extract the mean for "lro" if it exists
      lro_mean <- summary_data[summary_data$stat == "mean" & !is.na(summary_data$lro), "lro"]
      
      # Assign the value to lro_mean_s3
      if (length(lro_mean) > 0) {
        lro_mean_s3[m, r + 1] <- lro_mean
      } else {
        lro_mean_s3[m, r + 1] <- NA
      }
    } else {
      # File doesn't exist; log a warning and set to NA
      warning(sprintf("Summary file not found: %s", summary_file))
      lro_mean_s3[m, r + 1] <- NA
    }
  }
}

# Check it
lro_mean_s3

### Age at sexual maturity ----

# Reinitialize asm_mean_s3 dynamically
asm_mean_s3 <- as.data.frame(matrix(NA, nrow = length(m_values), ncol = length(r_values) + 1))
colnames(asm_mean_s3) <- c("m", paste0("r", r_values))
asm_mean_s3$m <- transf_prob # Update to match your specific production probabilities

# Directory for summary files
summary_dir <- "./RQ_3/lht_summaries_s3"

# Dynamically calculate asm_mean_s3
for (m in 1:nrow(asm_mean_s3)) {
  for (r in 1:10) {
    # Construct the file name for the corresponding summary file
    summary_file <- file.path(summary_dir, sprintf("summary_lht_list_m%d_r%d.fst", m, r))
    
    # Check if the summary file exists
    if (file.exists(summary_file)) {
      # Read the summary file
      summary_data <- read_fst(summary_file)
      
      # Extract the mean for "asm" if it exists
      asm_mean <- summary_data[summary_data$stat == "mean" & !is.na(summary_data$asm), "asm"]
      
      # Assign the value to asm_mean_s3
      if (length(asm_mean) > 0) {
        asm_mean_s3[m, r + 1] <- asm_mean
      } else {
        asm_mean_s3[m, r + 1] <- NA
      }
    } else {
      # File doesn't exist; log a warning and set to NA
      warning(sprintf("Summary file not found: %s", summary_file))
      asm_mean_s3[m, r + 1] <- NA
    }
  }
}

# Check it
asm_mean_s3

### Age at first reproduction ----
# Reinitialize afr_mean_s3 dynamically
afr_mean_s3 <- as.data.frame(matrix(NA, nrow = length(m_values), ncol = length(r_values) + 1))
colnames(afr_mean_s3) <- c("m", paste0("r", r_values))
afr_mean_s3$m <- transf_prob # Update to match your specific production probabilities

# Directory for summary files
summary_dir <- "./RQ_3/lht_summaries_s3"

# Dynamically calculate afr_mean_s3
for (m in 1:nrow(afr_mean_s3)) {
  for (r in 1:10) {
    # Construct the file name for the corresponding summary file
    summary_file <- file.path(summary_dir, sprintf("summary_lht_list_m%d_r%d.fst", m, r))
    
    # Check if the summary file exists
    if (file.exists(summary_file)) {
      # Read the summary file
      summary_data <- read_fst(summary_file)
      
      # Extract the mean for "afr" if it exists
      afr_mean <- summary_data[summary_data$stat == "mean" & !is.na(summary_data$afr), "afr"]
      
      # Assign the value to afr_mean_s3
      if (length(afr_mean) > 0) {
        afr_mean_s3[m, r + 1] <- afr_mean
      } else {
        afr_mean_s3[m, r + 1] <- NA
      }
    } else {
      # File doesn't exist; log a warning and set to NA
      warning(sprintf("Summary file not found: %s", summary_file))
      afr_mean_s3[m, r + 1] <- NA
    }
  }
}

# Check it
afr_mean_s3

### Age at last reproduction ----
# Reinitialize alr_mean_s3 dynamically
alr_mean_s3 <- as.data.frame(matrix(NA, nrow = length(m_values), ncol = length(r_values) + 1))
colnames(alr_mean_s3) <- c("m", paste0("r", r_values))
alr_mean_s3$m <- transf_prob # Update to match your specific production probabilities

# Directory for summary files
summary_dir <- "./RQ_3/lht_summaries_s3"

# Dynamically calculate alr_mean_s3
for (m in 1:nrow(alr_mean_s3)) {
  for (r in 1:10) {
    # Construct the file name for the corresponding summary file
    summary_file <- file.path(summary_dir, sprintf("summary_lht_list_m%d_r%d.fst", m, r))
    
    # Check if the summary file exists
    if (file.exists(summary_file)) {
      # Read the summary file
      summary_data <- read_fst(summary_file)
      
      # Extract the mean for "alr" if it exists
      alr_mean <- summary_data[summary_data$stat == "mean" & !is.na(summary_data$alr), "alr"]
      
      # Assign the value to alr_mean_s3
      if (length(alr_mean) > 0) {
        alr_mean_s3[m, r + 1] <- alr_mean
      } else {
        alr_mean_s3[m, r + 1] <- NA
      }
    } else {
      # File doesn't exist; log a warning and set to NA
      warning(sprintf("Summary file not found: %s", summary_file))
      alr_mean_s3[m, r + 1] <- NA
    }
  }
}

# Check it
alr_mean_s3

### Age at menopause ----
# Reinitialize meno_mean_s3 dynamically
meno_mean_s3 <- as.data.frame(matrix(NA, nrow = length(m_values), ncol = length(r_values) + 1))
colnames(meno_mean_s3) <- c("m", paste0("r", r_values))
meno_mean_s3$m <- transf_prob # Update to match your specific production probabilities

# Directory for summary files
summary_dir <- "./RQ_3/lht_summaries_s3"

# Dynamically calculate meno_mean_s3
for (m in 1:nrow(meno_mean_s3)) {
  for (r in 1:10) {
    # Construct the file name for the corresponding summary file
    summary_file <- file.path(summary_dir, sprintf("summary_lht_list_m%d_r%d.fst", m, r))
    
    # Check if the summary file exists
    if (file.exists(summary_file)) {
      # Read the summary file
      summary_data <- read_fst(summary_file)
      
      # Extract the mean for "meno" if it exists
      meno_mean <- summary_data[summary_data$stat == "mean" & !is.na(summary_data$meno), "meno"]
      
      # Assign the value to meno_mean_s3
      if (length(meno_mean) > 0) {
        meno_mean_s3[m, r + 1] <- meno_mean
      } else {
        meno_mean_s3[m, r + 1] <- NA
      }
    } else {
      # File doesn't exist; log a warning and set to NA
      warning(sprintf("Summary file not found: %s", summary_file))
      meno_mean_s3[m, r + 1] <- NA
    }
  }
}

# Check it
meno_mean_s3

### Save the data ----

saveRDS(lng_mean_s3,file="./RQ_3/lng_mean_s3.RData")
saveRDS(lro_mean_s3,file="./RQ_3/lro_mean_s3.RData")
saveRDS(asm_mean_s3,file="./RQ_3/asm_mean_s3.RData")
saveRDS(afr_mean_s3,file="./RQ_3/afr_mean_s3.RData")
saveRDS(alr_mean_s3,file="./RQ_3/alr_mean_s3.RData")
saveRDS(meno_mean_s3,file="./RQ_3/meno_mean_s3.RData")

## SD sorted by life history trait ----

# Get parameter ranges
m_values <- 1:19
r_values <- 1:10

### Longevity ----

# Reinitialize lng_sd_s3 dynamically
lng_sd_s3 <- as.data.frame(matrix(NA, nrow = length(m_values), ncol = length(r_values) + 1))
colnames(lng_sd_s3) <- c("m", paste0("r", r_values))
lng_sd_s3$m <- transf_prob # Update to match your specific production probabilities

# Directory for summary files
summary_dir <- "./RQ_3/lht_summaries_s3"

# Dynamically calculate lng_sd_s3
for (m in 1:nrow(lng_sd_s3)) {
  for (r in 1:10) {
    # Construct the file name for the corresponding summary file
    summary_file <- file.path(summary_dir, sprintf("summary_lht_list_m%d_r%d.fst", m, r))
    
    # Check if the summary file exists
    if (file.exists(summary_file)) {
      # Read the summary file
      summary_data <- read_fst(summary_file)
      
      # Extract the sd for "lng" if it exists
      lng_sd <- summary_data[summary_data$stat == "sd" & !is.na(summary_data$lng), "lng"]
      
      # Assign the value to lng_sd_s3
      if (length(lng_sd) > 0) {
        lng_sd_s3[m, r + 1] <- lng_sd
      } else {
        lng_sd_s3[m, r + 1] <- NA
      }
    } else {
      # File doesn't exist; log a warning and set to NA
      warning(sprintf("Summary file not found: %s", summary_file))
      lng_sd_s3[m, r + 1] <- NA
    }
  }
}
#check it
lng_sd_s3

### Lifetime reproductive ouput ----
# Reinitialize lro_sd_s3 dynamically
lro_sd_s3 <- as.data.frame(matrix(NA, nrow = length(m_values), ncol = length(r_values) + 1))
colnames(lro_sd_s3) <- c("m", paste0("r", r_values))
lro_sd_s3$m <- transf_prob # Update to match your specific production probabilities

# Directory for summary files
summary_dir <- "./RQ_3/lht_summaries_s3"

# Dynamically calculate lro_sd_s3
for (m in 1:nrow(lro_sd_s3)) {
  for (r in 1:10) {
    # Construct the file name for the corresponding summary file
    summary_file <- file.path(summary_dir, sprintf("summary_lht_list_m%d_r%d.fst", m, r))
    
    # Check if the summary file exists
    if (file.exists(summary_file)) {
      # Read the summary file
      summary_data <- read_fst(summary_file)
      
      # Extract the sd for "lro" if it exists
      lro_sd <- summary_data[summary_data$stat == "sd" & !is.na(summary_data$lro), "lro"]
      
      # Assign the value to lro_sd_s3
      if (length(lro_sd) > 0) {
        lro_sd_s3[m, r + 1] <- lro_sd
      } else {
        lro_sd_s3[m, r + 1] <- NA
      }
    } else {
      # File doesn't exist; log a warning and set to NA
      warning(sprintf("Summary file not found: %s", summary_file))
      lro_sd_s3[m, r + 1] <- NA
    }
  }
}

# Check it
lro_sd_s3

### Age at sexual maturity ----

# Reinitialize asm_sd_s3 dynamically
asm_sd_s3 <- as.data.frame(matrix(NA, nrow = length(m_values), ncol = length(r_values) + 1))
colnames(asm_sd_s3) <- c("m", paste0("r", r_values))
asm_sd_s3$m <- transf_prob # Update to match your specific production probabilities

# Directory for summary files
summary_dir <- "./RQ_3/lht_summaries_s3"

# Dynamically calculate asm_sd_s3
for (m in 1:nrow(asm_sd_s3)) {
  for (r in 1:10) {
    # Construct the file name for the corresponding summary file
    summary_file <- file.path(summary_dir, sprintf("summary_lht_list_m%d_r%d.fst", m, r))
    
    # Check if the summary file exists
    if (file.exists(summary_file)) {
      # Read the summary file
      summary_data <- read_fst(summary_file)
      
      # Extract the sd for "asm" if it exists
      asm_sd <- summary_data[summary_data$stat == "sd" & !is.na(summary_data$asm), "asm"]
      
      # Assign the value to asm_sd_s3
      if (length(asm_sd) > 0) {
        asm_sd_s3[m, r + 1] <- asm_sd
      } else {
        asm_sd_s3[m, r + 1] <- NA
      }
    } else {
      # File doesn't exist; log a warning and set to NA
      warning(sprintf("Summary file not found: %s", summary_file))
      asm_sd_s3[m, r + 1] <- NA
    }
  }
}

# Check it
asm_sd_s3

### Age at first reproduction ----
# Reinitialize afr_sd_s3 dynamically
afr_sd_s3 <- as.data.frame(matrix(NA, nrow = length(m_values), ncol = length(r_values) + 1))
colnames(afr_sd_s3) <- c("m", paste0("r", r_values))
afr_sd_s3$m <- transf_prob # Update to match your specific production probabilities

# Directory for summary files
summary_dir <- "./RQ_3/lht_summaries_s3"

# Dynamically calculate afr_sd_s3
for (m in 1:nrow(afr_sd_s3)) {
  for (r in 1:10) {
    # Construct the file name for the corresponding summary file
    summary_file <- file.path(summary_dir, sprintf("summary_lht_list_m%d_r%d.fst", m, r))
    
    # Check if the summary file exists
    if (file.exists(summary_file)) {
      # Read the summary file
      summary_data <- read_fst(summary_file)
      
      # Extract the sd for "afr" if it exists
      afr_sd <- summary_data[summary_data$stat == "sd" & !is.na(summary_data$afr), "afr"]
      
      # Assign the value to afr_sd_s3
      if (length(afr_sd) > 0) {
        afr_sd_s3[m, r + 1] <- afr_sd
      } else {
        afr_sd_s3[m, r + 1] <- NA
      }
    } else {
      # File doesn't exist; log a warning and set to NA
      warning(sprintf("Summary file not found: %s", summary_file))
      afr_sd_s3[m, r + 1] <- NA
    }
  }
}

# Check it
afr_sd_s3

### Age at last reproduction ----
# Reinitialize alr_sd_s3 dynamically
alr_sd_s3 <- as.data.frame(matrix(NA, nrow = length(m_values), ncol = length(r_values) + 1))
colnames(alr_sd_s3) <- c("m", paste0("r", r_values))
alr_sd_s3$m <- transf_prob # Update to match your specific production probabilities

# Directory for summary files
summary_dir <- "./RQ_3/lht_summaries_s3"

# Dynamically calculate alr_sd_s3
for (m in 1:nrow(alr_sd_s3)) {
  for (r in 1:10) {
    # Construct the file name for the corresponding summary file
    summary_file <- file.path(summary_dir, sprintf("summary_lht_list_m%d_r%d.fst", m, r))
    
    # Check if the summary file exists
    if (file.exists(summary_file)) {
      # Read the summary file
      summary_data <- read_fst(summary_file)
      
      # Extract the sd for "alr" if it exists
      alr_sd <- summary_data[summary_data$stat == "sd" & !is.na(summary_data$alr), "alr"]
      
      # Assign the value to alr_sd_s3
      if (length(alr_sd) > 0) {
        alr_sd_s3[m, r + 1] <- alr_sd
      } else {
        alr_sd_s3[m, r + 1] <- NA
      }
    } else {
      # File doesn't exist; log a warning and set to NA
      warning(sprintf("Summary file not found: %s", summary_file))
      alr_sd_s3[m, r + 1] <- NA
    }
  }
}

# Check it
alr_sd_s3

### Age at menopause ----
# Reinitialize meno_sd_s3 dynamically
meno_sd_s3 <- as.data.frame(matrix(NA, nrow = length(m_values), ncol = length(r_values) + 1))
colnames(meno_sd_s3) <- c("m", paste0("r", r_values))
meno_sd_s3$m <- transf_prob # Update to match your specific production probabilities

# Directory for summary files
summary_dir <- "./RQ_3/lht_summaries_s3"

# Dynamically calculate meno_sd_s3
for (m in 1:nrow(meno_sd_s3)) {
  for (r in 1:10) {
    # Construct the file name for the corresponding summary file
    summary_file <- file.path(summary_dir, sprintf("summary_lht_list_m%d_r%d.fst", m, r))
    
    # Check if the summary file exists
    if (file.exists(summary_file)) {
      # Read the summary file
      summary_data <- read_fst(summary_file)
      
      # Extract the sd for "meno" if it exists
      meno_sd <- summary_data[summary_data$stat == "sd" & !is.na(summary_data$meno), "meno"]
      
      # Assign the value to meno_sd_s3
      if (length(meno_sd) > 0) {
        meno_sd_s3[m, r + 1] <- meno_sd
      } else {
        meno_sd_s3[m, r + 1] <- NA
      }
    } else {
      # File doesn't exist; log a warning and set to NA
      warning(sprintf("Summary file not found: %s", summary_file))
      meno_sd_s3[m, r + 1] <- NA
    }
  }
}

# Check it
meno_sd_s3

### Save the data ----

saveRDS(lng_sd_s3,file="./RQ_3/lng_sd_s3.RData")
saveRDS(lro_sd_s3,file="./RQ_3/lro_sd_s3.RData")
saveRDS(asm_sd_s3,file="./RQ_3/asm_sd_s3.RData")
saveRDS(afr_sd_s3,file="./RQ_3/afr_sd_s3.RData")
saveRDS(alr_sd_s3,file="./RQ_3/alr_sd_s3.RData")
saveRDS(meno_sd_s3,file="./RQ_3/meno_sd_s3.RData")

## Min sorted by life history trait ----

# Get parameter ranges
m_values <- 1:19
r_values <- 1:10

### Longevity ----

# Reinitialize lng_min_s3 dynamically
lng_min_s3 <- as.data.frame(matrix(NA, nrow = length(m_values), ncol = length(r_values) + 1))
colnames(lng_min_s3) <- c("m", paste0("r", r_values))
lng_min_s3$m <- transf_prob # Update to match your specific production probabilities

# Directory for summary files
summary_dir <- "./RQ_3/lht_summaries_s3"

# Dynamically calculate lng_min_s3
for (m in 1:nrow(lng_min_s3)) {
  for (r in 1:10) {
    # Construct the file name for the corresponding summary file
    summary_file <- file.path(summary_dir, sprintf("summary_lht_list_m%d_r%d.fst", m, r))
    
    # Check if the summary file exists
    if (file.exists(summary_file)) {
      # Read the summary file
      summary_data <- read_fst(summary_file)
      
      # Extract the min for "lng" if it exists
      lng_min <- summary_data[summary_data$stat == "min" & !is.na(summary_data$lng), "lng"]
      
      # Assign the value to lng_min_s3
      if (length(lng_min) > 0) {
        lng_min_s3[m, r + 1] <- lng_min
      } else {
        lng_min_s3[m, r + 1] <- NA
      }
    } else {
      # File doesn't exist; log a warning and set to NA
      warning(sprintf("Summary file not found: %s", summary_file))
      lng_min_s3[m, r + 1] <- NA
    }
  }
}
#check it
lng_min_s3

### Lifetime reproductive ouput ----
# Reinitialize lro_min_s3 dynamically
lro_min_s3 <- as.data.frame(matrix(NA, nrow = length(m_values), ncol = length(r_values) + 1))
colnames(lro_min_s3) <- c("m", paste0("r", r_values))
lro_min_s3$m <- transf_prob # Update to match your specific production probabilities

# Directory for summary files
summary_dir <- "./RQ_3/lht_summaries_s3"

# Dynamically calculate lro_min_s3
for (m in 1:nrow(lro_min_s3)) {
  for (r in 1:10) {
    # Construct the file name for the corresponding summary file
    summary_file <- file.path(summary_dir, sprintf("summary_lht_list_m%d_r%d.fst", m, r))
    
    # Check if the summary file exists
    if (file.exists(summary_file)) {
      # Read the summary file
      summary_data <- read_fst(summary_file)
      
      # Extract the min for "lro" if it exists
      lro_min <- summary_data[summary_data$stat == "min" & !is.na(summary_data$lro), "lro"]
      
      # Assign the value to lro_min_s3
      if (length(lro_min) > 0) {
        lro_min_s3[m, r + 1] <- lro_min
      } else {
        lro_min_s3[m, r + 1] <- NA
      }
    } else {
      # File doesn't exist; log a warning and set to NA
      warning(sprintf("Summary file not found: %s", summary_file))
      lro_min_s3[m, r + 1] <- NA
    }
  }
}

# Check it
lro_min_s3

### Age at sexual maturity ----

# Reinitialize asm_min_s3 dynamically
asm_min_s3 <- as.data.frame(matrix(NA, nrow = length(m_values), ncol = length(r_values) + 1))
colnames(asm_min_s3) <- c("m", paste0("r", r_values))
asm_min_s3$m <- transf_prob # Update to match your specific production probabilities

# Directory for summary files
summary_dir <- "./RQ_3/lht_summaries_s3"

# Dynamically calculate asm_min_s3
for (m in 1:nrow(asm_min_s3)) {
  for (r in 1:10) {
    # Construct the file name for the corresponding summary file
    summary_file <- file.path(summary_dir, sprintf("summary_lht_list_m%d_r%d.fst", m, r))
    
    # Check if the summary file exists
    if (file.exists(summary_file)) {
      # Read the summary file
      summary_data <- read_fst(summary_file)
      
      # Extract the min for "asm" if it exists
      asm_min <- summary_data[summary_data$stat == "min" & !is.na(summary_data$asm), "asm"]
      
      # Assign the value to asm_min_s3
      if (length(asm_min) > 0) {
        asm_min_s3[m, r + 1] <- asm_min
      } else {
        asm_min_s3[m, r + 1] <- NA
      }
    } else {
      # File doesn't exist; log a warning and set to NA
      warning(sprintf("Summary file not found: %s", summary_file))
      asm_min_s3[m, r + 1] <- NA
    }
  }
}

# Check it
asm_min_s3

### Age at first reproduction ----
# Reinitialize afr_min_s3 dynamically
afr_min_s3 <- as.data.frame(matrix(NA, nrow = length(m_values), ncol = length(r_values) + 1))
colnames(afr_min_s3) <- c("m", paste0("r", r_values))
afr_min_s3$m <- transf_prob # Update to match your specific production probabilities

# Directory for summary files
summary_dir <- "./RQ_3/lht_summaries_s3"

# Dynamically calculate afr_min_s3
for (m in 1:nrow(afr_min_s3)) {
  for (r in 1:10) {
    # Construct the file name for the corresponding summary file
    summary_file <- file.path(summary_dir, sprintf("summary_lht_list_m%d_r%d.fst", m, r))
    
    # Check if the summary file exists
    if (file.exists(summary_file)) {
      # Read the summary file
      summary_data <- read_fst(summary_file)
      
      # Extract the min for "afr" if it exists
      afr_min <- summary_data[summary_data$stat == "min" & !is.na(summary_data$afr), "afr"]
      
      # Assign the value to afr_min_s3
      if (length(afr_min) > 0) {
        afr_min_s3[m, r + 1] <- afr_min
      } else {
        afr_min_s3[m, r + 1] <- NA
      }
    } else {
      # File doesn't exist; log a warning and set to NA
      warning(sprintf("Summary file not found: %s", summary_file))
      afr_min_s3[m, r + 1] <- NA
    }
  }
}

# Check it
afr_min_s3

### Age at last reproduction ----
# Reinitialize alr_min_s3 dynamically
alr_min_s3 <- as.data.frame(matrix(NA, nrow = length(m_values), ncol = length(r_values) + 1))
colnames(alr_min_s3) <- c("m", paste0("r", r_values))
alr_min_s3$m <- transf_prob # Update to match your specific production probabilities

# Directory for summary files
summary_dir <- "./RQ_3/lht_summaries_s3"

# Dynamically calculate alr_min_s3
for (m in 1:nrow(alr_min_s3)) {
  for (r in 1:10) {
    # Construct the file name for the corresponding summary file
    summary_file <- file.path(summary_dir, sprintf("summary_lht_list_m%d_r%d.fst", m, r))
    
    # Check if the summary file exists
    if (file.exists(summary_file)) {
      # Read the summary file
      summary_data <- read_fst(summary_file)
      
      # Extract the min for "alr" if it exists
      alr_min <- summary_data[summary_data$stat == "min" & !is.na(summary_data$alr), "alr"]
      
      # Assign the value to alr_min_s3
      if (length(alr_min) > 0) {
        alr_min_s3[m, r + 1] <- alr_min
      } else {
        alr_min_s3[m, r + 1] <- NA
      }
    } else {
      # File doesn't exist; log a warning and set to NA
      warning(sprintf("Summary file not found: %s", summary_file))
      alr_min_s3[m, r + 1] <- NA
    }
  }
}

# Check it
alr_min_s3

### Age at menopause ----
# Reinitialize meno_min_s3 dynamically
meno_min_s3 <- as.data.frame(matrix(NA, nrow = length(m_values), ncol = length(r_values) + 1))
colnames(meno_min_s3) <- c("m", paste0("r", r_values))
meno_min_s3$m <- transf_prob # Update to match your specific production probabilities

# Directory for summary files
summary_dir <- "./RQ_3/lht_summaries_s3"

# Dynamically calculate meno_min_s3
for (m in 1:nrow(meno_min_s3)) {
  for (r in 1:10) {
    # Construct the file name for the corresponding summary file
    summary_file <- file.path(summary_dir, sprintf("summary_lht_list_m%d_r%d.fst", m, r))
    
    # Check if the summary file exists
    if (file.exists(summary_file)) {
      # Read the summary file
      summary_data <- read_fst(summary_file)
      
      # Extract the min for "meno" if it exists
      meno_min <- summary_data[summary_data$stat == "min" & !is.na(summary_data$meno), "meno"]
      
      # Assign the value to meno_min_s3
      if (length(meno_min) > 0) {
        meno_min_s3[m, r + 1] <- meno_min
      } else {
        meno_min_s3[m, r + 1] <- NA
      }
    } else {
      # File doesn't exist; log a warning and set to NA
      warning(sprintf("Summary file not found: %s", summary_file))
      meno_min_s3[m, r + 1] <- NA
    }
  }
}

# Check it
meno_min_s3

### Save the data ----

saveRDS(lng_min_s3,file="./RQ_3/lng_min_s3.RData")
saveRDS(lro_min_s3,file="./RQ_3/lro_min_s3.RData")
saveRDS(asm_min_s3,file="./RQ_3/asm_min_s3.RData")
saveRDS(afr_min_s3,file="./RQ_3/afr_min_s3.RData")
saveRDS(alr_min_s3,file="./RQ_3/alr_min_s3.RData")
saveRDS(meno_min_s3,file="./RQ_3/meno_min_s3.RData")

## Max sorted by life history trait ----

# Get parameter ranges
m_values <- 1:19
r_values <- 1:10

### Longevity ----

# Reinitialize lng_max_s3 dynamically
lng_max_s3 <- as.data.frame(matrix(NA, nrow = length(m_values), ncol = length(r_values) + 1))
colnames(lng_max_s3) <- c("m", paste0("r", r_values))
lng_max_s3$m <- transf_prob # Update to match your specific production probabilities

# Directory for summary files
summary_dir <- "./RQ_3/lht_summaries_s3"

# Dynamically calculate lng_max_s3
for (m in 1:nrow(lng_max_s3)) {
  for (r in 1:10) {
    # Construct the file name for the corresponding summary file
    summary_file <- file.path(summary_dir, sprintf("summary_lht_list_m%d_r%d.fst", m, r))
    
    # Check if the summary file exists
    if (file.exists(summary_file)) {
      # Read the summary file
      summary_data <- read_fst(summary_file)
      
      # Extract the max for "lng" if it exists
      lng_max <- summary_data[summary_data$stat == "max" & !is.na(summary_data$lng), "lng"]
      
      # Assign the value to lng_max_s3
      if (length(lng_max) > 0) {
        lng_max_s3[m, r + 1] <- lng_max
      } else {
        lng_max_s3[m, r + 1] <- NA
      }
    } else {
      # File doesn't exist; log a warning and set to NA
      warning(sprintf("Summary file not found: %s", summary_file))
      lng_max_s3[m, r + 1] <- NA
    }
  }
}
#check it
lng_max_s3

### Lifetime reproductive ouput ----
# Reinitialize lro_max_s3 dynamically
lro_max_s3 <- as.data.frame(matrix(NA, nrow = length(m_values), ncol = length(r_values) + 1))
colnames(lro_max_s3) <- c("m", paste0("r", r_values))
lro_max_s3$m <- transf_prob # Update to match your specific production probabilities

# Directory for summary files
summary_dir <- "./RQ_3/lht_summaries_s3"

# Dynamically calculate lro_max_s3
for (m in 1:nrow(lro_max_s3)) {
  for (r in 1:10) {
    # Construct the file name for the corresponding summary file
    summary_file <- file.path(summary_dir, sprintf("summary_lht_list_m%d_r%d.fst", m, r))
    
    # Check if the summary file exists
    if (file.exists(summary_file)) {
      # Read the summary file
      summary_data <- read_fst(summary_file)
      
      # Extract the max for "lro" if it exists
      lro_max <- summary_data[summary_data$stat == "max" & !is.na(summary_data$lro), "lro"]
      
      # Assign the value to lro_max_s3
      if (length(lro_max) > 0) {
        lro_max_s3[m, r + 1] <- lro_max
      } else {
        lro_max_s3[m, r + 1] <- NA
      }
    } else {
      # File doesn't exist; log a warning and set to NA
      warning(sprintf("Summary file not found: %s", summary_file))
      lro_max_s3[m, r + 1] <- NA
    }
  }
}

# Check it
lro_max_s3

### Age at sexual maturity ----

# Reinitialize asm_max_s3 dynamically
asm_max_s3 <- as.data.frame(matrix(NA, nrow = length(m_values), ncol = length(r_values) + 1))
colnames(asm_max_s3) <- c("m", paste0("r", r_values))
asm_max_s3$m <- transf_prob # Update to match your specific production probabilities

# Directory for summary files
summary_dir <- "./RQ_3/lht_summaries_s3"

# Dynamically calculate asm_max_s3
for (m in 1:nrow(asm_max_s3)) {
  for (r in 1:10) {
    # Construct the file name for the corresponding summary file
    summary_file <- file.path(summary_dir, sprintf("summary_lht_list_m%d_r%d.fst", m, r))
    
    # Check if the summary file exists
    if (file.exists(summary_file)) {
      # Read the summary file
      summary_data <- read_fst(summary_file)
      
      # Extract the max for "asm" if it exists
      asm_max <- summary_data[summary_data$stat == "max" & !is.na(summary_data$asm), "asm"]
      
      # Assign the value to asm_max_s3
      if (length(asm_max) > 0) {
        asm_max_s3[m, r + 1] <- asm_max
      } else {
        asm_max_s3[m, r + 1] <- NA
      }
    } else {
      # File doesn't exist; log a warning and set to NA
      warning(sprintf("Summary file not found: %s", summary_file))
      asm_max_s3[m, r + 1] <- NA
    }
  }
}

# Check it
asm_max_s3

### Age at first reproduction ----
# Reinitialize afr_max_s3 dynamically
afr_max_s3 <- as.data.frame(matrix(NA, nrow = length(m_values), ncol = length(r_values) + 1))
colnames(afr_max_s3) <- c("m", paste0("r", r_values))
afr_max_s3$m <- transf_prob # Update to match your specific production probabilities

# Directory for summary files
summary_dir <- "./RQ_3/lht_summaries_s3"

# Dynamically calculate afr_max_s3
for (m in 1:nrow(afr_max_s3)) {
  for (r in 1:10) {
    # Construct the file name for the corresponding summary file
    summary_file <- file.path(summary_dir, sprintf("summary_lht_list_m%d_r%d.fst", m, r))
    
    # Check if the summary file exists
    if (file.exists(summary_file)) {
      # Read the summary file
      summary_data <- read_fst(summary_file)
      
      # Extract the max for "afr" if it exists
      afr_max <- summary_data[summary_data$stat == "max" & !is.na(summary_data$afr), "afr"]
      
      # Assign the value to afr_max_s3
      if (length(afr_max) > 0) {
        afr_max_s3[m, r + 1] <- afr_max
      } else {
        afr_max_s3[m, r + 1] <- NA
      }
    } else {
      # File doesn't exist; log a warning and set to NA
      warning(sprintf("Summary file not found: %s", summary_file))
      afr_max_s3[m, r + 1] <- NA
    }
  }
}

# Check it
afr_max_s3

### Age at last reproduction ----
# Reinitialize alr_max_s3 dynamically
alr_max_s3 <- as.data.frame(matrix(NA, nrow = length(m_values), ncol = length(r_values) + 1))
colnames(alr_max_s3) <- c("m", paste0("r", r_values))
alr_max_s3$m <- transf_prob # Update to match your specific production probabilities

# Directory for summary files
summary_dir <- "./RQ_3/lht_summaries_s3"

# Dynamically calculate alr_max_s3
for (m in 1:nrow(alr_max_s3)) {
  for (r in 1:10) {
    # Construct the file name for the corresponding summary file
    summary_file <- file.path(summary_dir, sprintf("summary_lht_list_m%d_r%d.fst", m, r))
    
    # Check if the summary file exists
    if (file.exists(summary_file)) {
      # Read the summary file
      summary_data <- read_fst(summary_file)
      
      # Extract the max for "alr" if it exists
      alr_max <- summary_data[summary_data$stat == "max" & !is.na(summary_data$alr), "alr"]
      
      # Assign the value to alr_max_s3
      if (length(alr_max) > 0) {
        alr_max_s3[m, r + 1] <- alr_max
      } else {
        alr_max_s3[m, r + 1] <- NA
      }
    } else {
      # File doesn't exist; log a warning and set to NA
      warning(sprintf("Summary file not found: %s", summary_file))
      alr_max_s3[m, r + 1] <- NA
    }
  }
}

# Check it
alr_max_s3

### Age at menopause ----
# Reinitialize meno_max_s3 dynamically
meno_max_s3 <- as.data.frame(matrix(NA, nrow = length(m_values), ncol = length(r_values) + 1))
colnames(meno_max_s3) <- c("m", paste0("r", r_values))
meno_max_s3$m <- transf_prob # Update to match your specific production probabilities

# Directory for summary files
summary_dir <- "./RQ_3/lht_summaries_s3"

# Dynamically calculate meno_max_s3
for (m in 1:nrow(meno_max_s3)) {
  for (r in 1:10) {
    # Construct the file name for the corresponding summary file
    summary_file <- file.path(summary_dir, sprintf("summary_lht_list_m%d_r%d.fst", m, r))
    
    # Check if the summary file exists
    if (file.exists(summary_file)) {
      # Read the summary file
      summary_data <- read_fst(summary_file)
      
      # Extract the max for "meno" if it exists
      meno_max <- summary_data[summary_data$stat == "max" & !is.na(summary_data$meno), "meno"]
      
      # Assign the value to meno_max_s3
      if (length(meno_max) > 0) {
        meno_max_s3[m, r + 1] <- meno_max
      } else {
        meno_max_s3[m, r + 1] <- NA
      }
    } else {
      # File doesn't exist; log a warning and set to NA
      warning(sprintf("Summary file not found: %s", summary_file))
      meno_max_s3[m, r + 1] <- NA
    }
  }
}

# Check it
meno_max_s3

### Save the data ----

saveRDS(lng_max_s3,file="./RQ_3/lng_max_s3.RData")
saveRDS(lro_max_s3,file="./RQ_3/lro_max_s3.RData")
saveRDS(asm_max_s3,file="./RQ_3/asm_max_s3.RData")
saveRDS(afr_max_s3,file="./RQ_3/afr_max_s3.RData")
saveRDS(alr_max_s3,file="./RQ_3/alr_max_s3.RData")
saveRDS(meno_max_s3,file="./RQ_3/meno_max_s3.RData")

## CV sorted by life history trait ----

# Get parameter ranges
m_values <- 1:19
r_values <- 1:10

### Longevity ----

# Reinitialize lng_cv_s3 dynamically
lng_cv_s3 <- as.data.frame(matrix(NA, nrow = length(m_values), ncol = length(r_values) + 1))
colnames(lng_cv_s3) <- c("m", paste0("r", r_values))
lng_cv_s3$m <- transf_prob # Update to match your specific production probabilities

# Directory for summary files
summary_dir <- "./RQ_3/lht_summaries_s3"

# Dynamically calculate lng_cv_s3
for (m in 1:nrow(lng_cv_s3)) {
  for (r in 1:10) {
    # Construct the file name for the corresponding summary file
    summary_file <- file.path(summary_dir, sprintf("summary_lht_list_m%d_r%d.fst", m, r))
    
    # Check if the summary file exists
    if (file.exists(summary_file)) {
      # Read the summary file
      summary_data <- read_fst(summary_file)
      
      # Extract the cv for "lng" if it exists
      lng_cv <- summary_data[summary_data$stat == "cv" & !is.na(summary_data$lng), "lng"]
      
      # Assign the value to lng_cv_s3
      if (length(lng_cv) > 0) {
        lng_cv_s3[m, r + 1] <- lng_cv
      } else {
        lng_cv_s3[m, r + 1] <- NA
      }
    } else {
      # File doesn't exist; log a warning and set to NA
      warning(sprintf("Summary file not found: %s", summary_file))
      lng_cv_s3[m, r + 1] <- NA
    }
  }
}
#check it
lng_cv_s3

### Lifetime reproductive ouput ----
# Reinitialize lro_cv_s3 dynamically
lro_cv_s3 <- as.data.frame(matrix(NA, nrow = length(m_values), ncol = length(r_values) + 1))
colnames(lro_cv_s3) <- c("m", paste0("r", r_values))
lro_cv_s3$m <- transf_prob # Update to match your specific production probabilities

# Directory for summary files
summary_dir <- "./RQ_3/lht_summaries_s3"

# Dynamically calculate lro_cv_s3
for (m in 1:nrow(lro_cv_s3)) {
  for (r in 1:10) {
    # Construct the file name for the corresponding summary file
    summary_file <- file.path(summary_dir, sprintf("summary_lht_list_m%d_r%d.fst", m, r))
    
    # Check if the summary file exists
    if (file.exists(summary_file)) {
      # Read the summary file
      summary_data <- read_fst(summary_file)
      
      # Extract the cv for "lro" if it exists
      lro_cv <- summary_data[summary_data$stat == "cv" & !is.na(summary_data$lro), "lro"]
      
      # Assign the value to lro_cv_s3
      if (length(lro_cv) > 0) {
        lro_cv_s3[m, r + 1] <- lro_cv
      } else {
        lro_cv_s3[m, r + 1] <- NA
      }
    } else {
      # File doesn't exist; log a warning and set to NA
      warning(sprintf("Summary file not found: %s", summary_file))
      lro_cv_s3[m, r + 1] <- NA
    }
  }
}

# Check it
lro_cv_s3

### Age at sexual maturity ----

# Reinitialize asm_cv_s3 dynamically
asm_cv_s3 <- as.data.frame(matrix(NA, nrow = length(m_values), ncol = length(r_values) + 1))
colnames(asm_cv_s3) <- c("m", paste0("r", r_values))
asm_cv_s3$m <- transf_prob # Update to match your specific production probabilities

# Directory for summary files
summary_dir <- "./RQ_3/lht_summaries_s3"

# Dynamically calculate asm_cv_s3
for (m in 1:nrow(asm_cv_s3)) {
  for (r in 1:10) {
    # Construct the file name for the corresponding summary file
    summary_file <- file.path(summary_dir, sprintf("summary_lht_list_m%d_r%d.fst", m, r))
    
    # Check if the summary file exists
    if (file.exists(summary_file)) {
      # Read the summary file
      summary_data <- read_fst(summary_file)
      
      # Extract the cv for "asm" if it exists
      asm_cv <- summary_data[summary_data$stat == "cv" & !is.na(summary_data$asm), "asm"]
      
      # Assign the value to asm_cv_s3
      if (length(asm_cv) > 0) {
        asm_cv_s3[m, r + 1] <- asm_cv
      } else {
        asm_cv_s3[m, r + 1] <- NA
      }
    } else {
      # File doesn't exist; log a warning and set to NA
      warning(sprintf("Summary file not found: %s", summary_file))
      asm_cv_s3[m, r + 1] <- NA
    }
  }
}

# Check it
asm_cv_s3

### Age at first reproduction ----
# Reinitialize afr_cv_s3 dynamically
afr_cv_s3 <- as.data.frame(matrix(NA, nrow = length(m_values), ncol = length(r_values) + 1))
colnames(afr_cv_s3) <- c("m", paste0("r", r_values))
afr_cv_s3$m <- transf_prob # Update to match your specific production probabilities

# Directory for summary files
summary_dir <- "./RQ_3/lht_summaries_s3"

# Dynamically calculate afr_cv_s3
for (m in 1:nrow(afr_cv_s3)) {
  for (r in 1:10) {
    # Construct the file name for the corresponding summary file
    summary_file <- file.path(summary_dir, sprintf("summary_lht_list_m%d_r%d.fst", m, r))
    
    # Check if the summary file exists
    if (file.exists(summary_file)) {
      # Read the summary file
      summary_data <- read_fst(summary_file)
      
      # Extract the cv for "afr" if it exists
      afr_cv <- summary_data[summary_data$stat == "cv" & !is.na(summary_data$afr), "afr"]
      
      # Assign the value to afr_cv_s3
      if (length(afr_cv) > 0) {
        afr_cv_s3[m, r + 1] <- afr_cv
      } else {
        afr_cv_s3[m, r + 1] <- NA
      }
    } else {
      # File doesn't exist; log a warning and set to NA
      warning(sprintf("Summary file not found: %s", summary_file))
      afr_cv_s3[m, r + 1] <- NA
    }
  }
}

# Check it
afr_cv_s3

### Age at last reproduction ----
# Reinitialize alr_cv_s3 dynamically
alr_cv_s3 <- as.data.frame(matrix(NA, nrow = length(m_values), ncol = length(r_values) + 1))
colnames(alr_cv_s3) <- c("m", paste0("r", r_values))
alr_cv_s3$m <- transf_prob # Update to match your specific production probabilities

# Directory for summary files
summary_dir <- "./RQ_3/lht_summaries_s3"

# Dynamically calculate alr_cv_s3
for (m in 1:nrow(alr_cv_s3)) {
  for (r in 1:10) {
    # Construct the file name for the corresponding summary file
    summary_file <- file.path(summary_dir, sprintf("summary_lht_list_m%d_r%d.fst", m, r))
    
    # Check if the summary file exists
    if (file.exists(summary_file)) {
      # Read the summary file
      summary_data <- read_fst(summary_file)
      
      # Extract the cv for "alr" if it exists
      alr_cv <- summary_data[summary_data$stat == "cv" & !is.na(summary_data$alr), "alr"]
      
      # Assign the value to alr_cv_s3
      if (length(alr_cv) > 0) {
        alr_cv_s3[m, r + 1] <- alr_cv
      } else {
        alr_cv_s3[m, r + 1] <- NA
      }
    } else {
      # File doesn't exist; log a warning and set to NA
      warning(sprintf("Summary file not found: %s", summary_file))
      alr_cv_s3[m, r + 1] <- NA
    }
  }
}

# Check it
alr_cv_s3

### Age at menopause ----
# Reinitialize meno_cv_s3 dynamically
meno_cv_s3 <- as.data.frame(matrix(NA, nrow = length(m_values), ncol = length(r_values) + 1))
colnames(meno_cv_s3) <- c("m", paste0("r", r_values))
meno_cv_s3$m <- transf_prob # Update to match your specific production probabilities

# Directory for summary files
summary_dir <- "./RQ_3/lht_summaries_s3"

# Dynamically calculate meno_cv_s3
for (m in 1:nrow(meno_cv_s3)) {
  for (r in 1:10) {
    # Construct the file name for the corresponding summary file
    summary_file <- file.path(summary_dir, sprintf("summary_lht_list_m%d_r%d.fst", m, r))
    
    # Check if the summary file exists
    if (file.exists(summary_file)) {
      # Read the summary file
      summary_data <- read_fst(summary_file)
      
      # Extract the cv for "meno" if it exists
      meno_cv <- summary_data[summary_data$stat == "cv" & !is.na(summary_data$meno), "meno"]
      
      # Assign the value to meno_cv_s3
      if (length(meno_cv) > 0) {
        meno_cv_s3[m, r + 1] <- meno_cv
      } else {
        meno_cv_s3[m, r + 1] <- NA
      }
    } else {
      # File doesn't exist; log a warning and set to NA
      warning(sprintf("Summary file not found: %s", summary_file))
      meno_cv_s3[m, r + 1] <- NA
    }
  }
}

# Check it
meno_cv_s3

### Save the data ----

saveRDS(lng_cv_s3,file="./RQ_3/lng_cv_s3.RData")
saveRDS(lro_cv_s3,file="./RQ_3/lro_cv_s3.RData")
saveRDS(asm_cv_s3,file="./RQ_3/asm_cv_s3.RData")
saveRDS(afr_cv_s3,file="./RQ_3/afr_cv_s3.RData")
saveRDS(alr_cv_s3,file="./RQ_3/alr_cv_s3.RData")
saveRDS(meno_cv_s3,file="./RQ_3/meno_cv_s3.RData")

##Average life cycle ----

###Longevity ----

#p = 0.75
apply(lng_mean_s3[2:11],1,mean,na.rm=T)[which(lng_mean_s3$m == 0.75)]
apply(lng_sd_s3[2:11],1,mean,na.rm=T)[which(lng_mean_s3$m == 0.75)]
apply(lng_min_s3[2:11],1,mean,na.rm=T)[which(lng_mean_s3$m == 0.75)]
apply(lng_max_s3[2:11],1,mean,na.rm=T)[which(lng_mean_s3$m == 0.75)]
#p = 0.55
apply(lng_mean_s3[2:11],1,mean,na.rm=T)[which(lng_mean_s3$m == 0.6)]
apply(lng_sd_s3[2:11],1,mean,na.rm=T)[which(lng_mean_s3$m == 0.6)]
apply(lng_min_s3[2:11],1,mean,na.rm=T)[which(lng_mean_s3$m == 0.6)]
apply(lng_max_s3[2:11],1,mean,na.rm=T)[which(lng_mean_s3$m == 0.6)]
#p = 0.90
apply(lng_mean_s3[2:11],1,mean,na.rm=T)[which(lng_mean_s3$m == 0.9)]
apply(lng_sd_s3[2:11],1,mean,na.rm=T)[which(lng_mean_s3$m == 0.9)]
apply(lng_min_s3[2:11],1,mean,na.rm=T)[which(lng_mean_s3$m == 0.9)]
apply(lng_max_s3[2:11],1,mean,na.rm=T)[which(lng_mean_s3$m == 0.9)]

###Lifetime reproductive output ----

#p = 0.7
apply(lro_mean_s3[2:11],1,mean,na.rm=T)[which(lro_mean_s3$m == 0.75)]
apply(lro_sd_s3[2:11],1,mean,na.rm=T)[which(lro_mean_s3$m == 0.75)]
apply(lro_min_s3[2:11],1,mean,na.rm=T)[which(lro_mean_s3$m == 0.75)]
apply(lro_max_s3[2:11],1,mean,na.rm=T)[which(lro_mean_s3$m == 0.75)]
#p = 0.55
apply(lro_mean_s3[2:11],1,mean,na.rm=T)[which(lro_mean_s3$m == 0.6)]
apply(lro_sd_s3[2:11],1,mean,na.rm=T)[which(lro_mean_s3$m == 0.6)]
apply(lro_min_s3[2:11],1,mean,na.rm=T)[which(lro_mean_s3$m == 0.6)]
apply(lro_max_s3[2:11],1,mean,na.rm=T)[which(lro_mean_s3$m == 0.6)]
#p = 0.9
apply(lro_mean_s3[2:11],1,mean,na.rm=T)[which(lro_mean_s3$m == 0.9)]
apply(lro_sd_s3[2:11],1,mean,na.rm=T)[which(lro_mean_s3$m == 0.9)]
apply(lro_min_s3[2:11],1,mean,na.rm=T)[which(lro_mean_s3$m == 0.9)]
apply(lro_max_s3[2:11],1,mean,na.rm=T)[which(lro_mean_s3$m == 0.9)]

###Age at sexual maturity ----

#p = 0.75
apply(asm_mean_s3[2:11],1,mean,na.rm=T)[which(asm_mean_s3$m == 0.75)]
apply(asm_sd_s3[2:11],1,mean,na.rm=T)[which(asm_mean_s3$m == 0.75)]
apply(asm_min_s3[2:11],1,mean,na.rm=T)[which(asm_mean_s3$m == 0.75)]
apply(asm_max_s3[2:11],1,mean,na.rm=T)[which(asm_mean_s3$m == 0.75)]
#p = 0.55
apply(asm_mean_s3[2:11],1,mean,na.rm=T)[which(asm_mean_s3$m == 0.65)]
apply(asm_sd_s3[2:11],1,mean,na.rm=T)[which(asm_mean_s3$m == 0.65)]
apply(asm_min_s3[2:11],1,mean,na.rm=T)[which(asm_mean_s3$m == 0.65)]
apply(asm_max_s3[2:11],1,mean,na.rm=T)[which(asm_mean_s3$m == 0.65)]
#p = 0.9
apply(asm_mean_s3[2:11],1,mean,na.rm=T)[which(asm_mean_s3$m == 0.9)]
apply(asm_sd_s3[2:11],1,mean,na.rm=T)[which(asm_mean_s3$m == 0.9)]
apply(asm_min_s3[2:11],1,mean,na.rm=T)[which(asm_mean_s3$m == 0.9)]
apply(asm_max_s3[2:11],1,mean,na.rm=T)[which(asm_mean_s3$m == 0.9)]

###Age at first reproduction ----

#p = 0.75
apply(afr_mean_s3[2:11],1,mean,na.rm=T)[which(afr_mean_s3$m == 0.75)]
apply(afr_sd_s3[2:11],1,mean,na.rm=T)[which(afr_mean_s3$m == 0.75)]
apply(afr_min_s3[2:11],1,mean,na.rm=T)[which(afr_mean_s3$m == 0.75)]
apply(afr_max_s3[2:11],1,mean,na.rm=T)[which(afr_mean_s3$m == 0.75)]
#p = 0.55
apply(afr_mean_s3[2:11],1,mean,na.rm=T)[which(afr_mean_s3$m == 0.65)]
apply(afr_sd_s3[2:11],1,mean,na.rm=T)[which(afr_mean_s3$m == 0.65)]
apply(afr_min_s3[2:11],1,mean,na.rm=T)[which(afr_mean_s3$m == 0.65)]
apply(afr_max_s3[2:11],1,mean,na.rm=T)[which(afr_mean_s3$m == 0.65)]
#p = 0.9
apply(afr_mean_s3[2:11],1,mean,na.rm=T)[which(afr_mean_s3$m == 0.9)]
apply(afr_sd_s3[2:11],1,mean,na.rm=T)[which(afr_mean_s3$m == 0.9)]
apply(afr_min_s3[2:11],1,mean,na.rm=T)[which(afr_mean_s3$m == 0.9)]
apply(afr_max_s3[2:11],1,mean,na.rm=T)[which(afr_mean_s3$m == 0.9)]

###Age at last reproduction ----

#p = 0.7
apply(alr_mean_s3[2:11],1,mean,na.rm=T)[which(alr_mean_s3$m == 0.75)]
apply(alr_sd_s3[2:11],1,mean,na.rm=T)[which(alr_mean_s3$m == 0.75)]
apply(alr_min_s3[2:11],1,mean,na.rm=T)[which(alr_mean_s3$m == 0.75)]
apply(alr_max_s3[2:11],1,mean,na.rm=T)[which(alr_mean_s3$m == 0.75)]
#p = 0.55
apply(alr_mean_s3[2:11],1,mean,na.rm=T)[which(alr_mean_s3$m == 0.65)]
apply(alr_sd_s3[2:11],1,mean,na.rm=T)[which(alr_mean_s3$m == 0.65)]
apply(alr_min_s3[2:11],1,mean,na.rm=T)[which(alr_mean_s3$m == 0.65)]
apply(alr_max_s3[2:11],1,mean,na.rm=T)[which(alr_mean_s3$m == 0.65)]
#p = 0.9
apply(alr_mean_s3[2:11],1,mean,na.rm=T)[which(alr_mean_s3$m == 0.9)]
apply(alr_sd_s3[2:11],1,mean,na.rm=T)[which(alr_mean_s3$m == 0.9)]
apply(alr_min_s3[2:11],1,mean,na.rm=T)[which(alr_mean_s3$m == 0.9)]
apply(alr_max_s3[2:11],1,mean,na.rm=T)[which(alr_mean_s3$m == 0.9)]

###Age at menopause ----

#p = 0.7
apply(meno_mean_s3[2:11],1,mean,na.rm=T)[which(meno_mean_s3$m == 0.75)]
apply(meno_sd_s3[2:11],1,mean,na.rm=T)[which(meno_mean_s3$m == 0.75)]
apply(meno_min_s3[2:11],1,mean,na.rm=T)[which(meno_mean_s3$m == 0.75)]
apply(meno_max_s3[2:11],1,mean,na.rm=T)[which(meno_mean_s3$m == 0.75)]
#p = 0.55
apply(meno_mean_s3[2:11],1,mean,na.rm=T)[which(meno_mean_s3$m == 0.65)]
apply(meno_sd_s3[2:11],1,mean,na.rm=T)[which(meno_mean_s3$m == 0.65)]
apply(meno_min_s3[2:11],1,mean,na.rm=T)[which(meno_mean_s3$m == 0.65)]
apply(meno_max_s3[2:11],1,mean,na.rm=T)[which(meno_mean_s3$m == 0.65)]
#p = 0.9
apply(meno_mean_s3[2:11],1,mean,na.rm=T)[which(meno_mean_s3$m == 0.9)]
apply(meno_sd_s3[2:11],1,mean,na.rm=T)[which(meno_mean_s3$m == 0.9)]
apply(meno_min_s3[2:11],1,mean,na.rm=T)[which(meno_mean_s3$m == 0.9)]
apply(meno_max_s3[2:11],1,mean,na.rm=T)[which(meno_mean_s3$m == 0.9)]


