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
#install.packages("fst")
library(fst)

#Scenario 2 ----

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

# Directory containing the input .fst files
input_dir <- "./Scenario_2/lht_lists"

# Directory to save the output .fst files
output_dir <- "./RQ_2/lht_summaries_s2"

# Create the output directory if it doesn't exist
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Get all file names in the directory
file_names <- list.files(input_dir, pattern = "lht_list_r\\d+\\.fst", full.names = TRUE)

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
r_values <- 1:10

### Longevity ----

# Reinitialize lht_mean_s2 dynamically
lht_mean_s2 <- as.data.frame(matrix(NA, nrow = length(r_values), ncol = 7))
colnames(lht_mean_s2) <- c("r","lng","lro","asm","afr","alr","meno")
lht_mean_s2$r <- r_values # Update to match your specific production probabilities

# Directory for summary files
summary_dir <- "./RQ_2/lht_summaries_s2"

# Dynamically calculate lht_mean_s2
for (r in 1:nrow(lht_mean_s2)) {
  
    # Construct the file name for the corresponding summary file
    summary_file <- file.path(summary_dir, sprintf("summary_lht_list_r%d.fst", r))
    
    # Check if the summary file exists
    if (file.exists(summary_file)) {
      # Read the summary file
      summary_data <- read_fst(summary_file)
      
      # Extract the mean for "lng" if it exists
      lng_mean <- summary_data[summary_data$stat == "mean" & !is.na(summary_data$lng), "lng"]
      
      # Assign the value to lht_mean_s2
      if (length(lng_mean) > 0) {
        lht_mean_s2[r, "lng"] <- lng_mean
      } else {
        lht_mean_s2[r, "lng"] <- NA
      }
    } else {
      # File doesn't exist; log a warning and set to NA
      warning(sprintf("Summary file not found: %s", summary_file))
      lht_mean_s2[r, "lng"] <- NA
    }
  
}
#check it
lht_mean_s2

### Lifetime reproductive ouput ----

# Directory for summary files
summary_dir <- "./RQ_2/lht_summaries_s2"


# Dynamically calculate lht_mean_s2
for (r in 1:nrow(lht_mean_s2)) {
  
  # Construct the file name for the corresponding summary file
  summary_file <- file.path(summary_dir, sprintf("summary_lht_list_r%d.fst", r))
  
  # Check if the summary file exists
  if (file.exists(summary_file)) {
    # Read the summary file
    summary_data <- read_fst(summary_file)
    
    # Extract the mean for "lro" if it exists
    lro_mean <- summary_data[summary_data$stat == "mean" & !is.na(summary_data$lro), "lro"]
    
    # Assign the value to lht_mean_s2
    if (length(lro_mean) > 0) {
      lht_mean_s2[r, "lro"] <- lro_mean
    } else {
      lht_mean_s2[r, "lro"] <- NA
    }
  } else {
    # File doesn't exist; log a warning and set to NA
    warning(sprintf("Summary file not found: %s", summary_file))
    lht_mean_s2[r, "lro"] <- NA
  }
  
}
#check it
lht_mean_s2


### Age at sexual maturity ----

# Directory for summary files
summary_dir <- "./RQ_2/lht_summaries_s2"


# Dynamically calculate lht_mean_s2
for (r in 1:nrow(lht_mean_s2)) {
  
  # Construct the file name for the corresponding summary file
  summary_file <- file.path(summary_dir, sprintf("summary_lht_list_r%d.fst", r))
  
  # Check if the summary file exists
  if (file.exists(summary_file)) {
    # Read the summary file
    summary_data <- read_fst(summary_file)
    
    # Extract the mean for "asm" if it exists
    asm_mean <- summary_data[summary_data$stat == "mean" & !is.na(summary_data$asm), "asm"]
    
    # Assign the value to lht_mean_s2
    if (length(asm_mean) > 0) {
      lht_mean_s2[r, "asm"] <- asm_mean
    } else {
      lht_mean_s2[r, "asm"] <- NA
    }
  } else {
    # File doesn't exist; log a warning and set to NA
    warning(sprintf("Summary file not found: %s", summary_file))
    lht_mean_s2[r, "asm"] <- NA
  }
  
}
#check it
lht_mean_s2


### Age at first reproduction ----

# Directory for summary files
summary_dir <- "./RQ_2/lht_summaries_s2"


# Dynamically calculate lht_mean_s2
for (r in 1:nrow(lht_mean_s2)) {
  
  # Construct the file name for the corresponding summary file
  summary_file <- file.path(summary_dir, sprintf("summary_lht_list_r%d.fst", r))
  
  # Check if the summary file exists
  if (file.exists(summary_file)) {
    # Read the summary file
    summary_data <- read_fst(summary_file)
    
    # Extract the mean for "afr" if it exists
    afr_mean <- summary_data[summary_data$stat == "mean" & !is.na(summary_data$afr), "afr"]
    
    # Assign the value to lht_mean_s2
    if (length(afr_mean) > 0) {
      lht_mean_s2[r, "afr"] <- afr_mean
    } else {
      lht_mean_s2[r, "afr"] <- NA
    }
  } else {
    # File doesn't exist; log a warning and set to NA
    warning(sprintf("Summary file not found: %s", summary_file))
    lht_mean_s2[r, "afr"] <- NA
  }
  
}
#check it
lht_mean_s2


### Age at last reproduction ----

# Directory for summary files
summary_dir <- "./RQ_2/lht_summaries_s2"


# Dynamically calculate lht_mean_s2
for (r in 1:nrow(lht_mean_s2)) {
  
  # Construct the file name for the corresponding summary file
  summary_file <- file.path(summary_dir, sprintf("summary_lht_list_r%d.fst", r))
  
  # Check if the summary file exists
  if (file.exists(summary_file)) {
    # Read the summary file
    summary_data <- read_fst(summary_file)
    
    # Extract the mean for "alr" if it exists
    alr_mean <- summary_data[summary_data$stat == "mean" & !is.na(summary_data$alr), "alr"]
    
    # Assign the value to lht_mean_s2
    if (length(alr_mean) > 0) {
      lht_mean_s2[r, "alr"] <- alr_mean
    } else {
      lht_mean_s2[r, "alr"] <- NA
    }
  } else {
    # File doesn't exist; log a warning and set to NA
    warning(sprintf("Summary file not found: %s", summary_file))
    lht_mean_s2[r, "alr"] <- NA
  }
  
}
#check it
lht_mean_s2


### Age at menopause ----

# Directory for summary files
summary_dir <- "./RQ_2/lht_summaries_s2"


# Dynamically calculate lht_mean_s2
for (r in 1:nrow(lht_mean_s2)) {
  
  # Construct the file name for the corresponding summary file
  summary_file <- file.path(summary_dir, sprintf("summary_lht_list_r%d.fst", r))
  
  # Check if the summary file exists
  if (file.exists(summary_file)) {
    # Read the summary file
    summary_data <- read_fst(summary_file)
    
    # Extract the mean for "meno" if it exists
    meno_mean <- summary_data[summary_data$stat == "mean" & !is.na(summary_data$meno), "meno"]
    
    # Assign the value to lht_mean_s2
    if (length(meno_mean) > 0) {
      lht_mean_s2[r, "meno"] <- meno_mean
    } else {
      lht_mean_s2[r, "meno"] <- NA
    }
  } else {
    # File doesn't exist; log a warning and set to NA
    warning(sprintf("Summary file not found: %s", summary_file))
    lht_mean_s2[r, "meno"] <- NA
  }
  
}
#check it
lht_mean_s2

### Save the data ----

saveRDS(lht_mean_s2,file="./RQ_2/lht_mean_s2.RData")

## SD sorted by life history trait ----


# Get parameter ranges
r_values <- 1:10

### Longevity ----

# Reinitialize lht_sd_s2 dynamically
lht_sd_s2 <- as.data.frame(matrix(NA, nrow = length(r_values), ncol = 7))
colnames(lht_sd_s2) <- c("r","lng","lro","asm","afr","alr","meno")
lht_sd_s2$r <- r_values # Update to match your specific production probabilities

# Directory for summary files
summary_dir <- "./RQ_2/lht_summaries_s2"

# Dynamically calculate lht_sd_s2
for (r in 1:nrow(lht_sd_s2)) {
  
  # Construct the file name for the corresponding summary file
  summary_file <- file.path(summary_dir, sprintf("summary_lht_list_r%d.fst", r))
  
  # Check if the summary file exists
  if (file.exists(summary_file)) {
    # Read the summary file
    summary_data <- read_fst(summary_file)
    
    # Extract the sd for "lng" if it exists
    lng_sd <- summary_data[summary_data$stat == "sd" & !is.na(summary_data$lng), "lng"]
    
    # Assign the value to lht_sd_s2
    if (length(lng_sd) > 0) {
      lht_sd_s2[r, "lng"] <- lng_sd
    } else {
      lht_sd_s2[r, "lng"] <- NA
    }
  } else {
    # File doesn't exist; log a warning and set to NA
    warning(sprintf("Summary file not found: %s", summary_file))
    lht_sd_s2[r, "lng"] <- NA
  }
  
}
#check it
lht_sd_s2

### Lifetime reproductive ouput ----

# Directory for summary files
summary_dir <- "./RQ_2/lht_summaries_s2"


# Dynamically calculate lht_sd_s2
for (r in 1:nrow(lht_sd_s2)) {
  
  # Construct the file name for the corresponding summary file
  summary_file <- file.path(summary_dir, sprintf("summary_lht_list_r%d.fst", r))
  
  # Check if the summary file exists
  if (file.exists(summary_file)) {
    # Read the summary file
    summary_data <- read_fst(summary_file)
    
    # Extract the sd for "lro" if it exists
    lro_sd <- summary_data[summary_data$stat == "sd" & !is.na(summary_data$lro), "lro"]
    
    # Assign the value to lht_sd_s2
    if (length(lro_sd) > 0) {
      lht_sd_s2[r, "lro"] <- lro_sd
    } else {
      lht_sd_s2[r, "lro"] <- NA
    }
  } else {
    # File doesn't exist; log a warning and set to NA
    warning(sprintf("Summary file not found: %s", summary_file))
    lht_sd_s2[r, "lro"] <- NA
  }
  
}
#check it
lht_sd_s2


### Age at sexual maturity ----

# Directory for summary files
summary_dir <- "./RQ_2/lht_summaries_s2"


# Dynamically calculate lht_sd_s2
for (r in 1:nrow(lht_sd_s2)) {
  
  # Construct the file name for the corresponding summary file
  summary_file <- file.path(summary_dir, sprintf("summary_lht_list_r%d.fst", r))
  
  # Check if the summary file exists
  if (file.exists(summary_file)) {
    # Read the summary file
    summary_data <- read_fst(summary_file)
    
    # Extract the sd for "asm" if it exists
    asm_sd <- summary_data[summary_data$stat == "sd" & !is.na(summary_data$asm), "asm"]
    
    # Assign the value to lht_sd_s2
    if (length(asm_sd) > 0) {
      lht_sd_s2[r, "asm"] <- asm_sd
    } else {
      lht_sd_s2[r, "asm"] <- NA
    }
  } else {
    # File doesn't exist; log a warning and set to NA
    warning(sprintf("Summary file not found: %s", summary_file))
    lht_sd_s2[r, "asm"] <- NA
  }
  
}
#check it
lht_sd_s2


### Age at first reproduction ----

# Directory for summary files
summary_dir <- "./RQ_2/lht_summaries_s2"


# Dynamically calculate lht_sd_s2
for (r in 1:nrow(lht_sd_s2)) {
  
  # Construct the file name for the corresponding summary file
  summary_file <- file.path(summary_dir, sprintf("summary_lht_list_r%d.fst", r))
  
  # Check if the summary file exists
  if (file.exists(summary_file)) {
    # Read the summary file
    summary_data <- read_fst(summary_file)
    
    # Extract the sd for "afr" if it exists
    afr_sd <- summary_data[summary_data$stat == "sd" & !is.na(summary_data$afr), "afr"]
    
    # Assign the value to lht_sd_s2
    if (length(afr_sd) > 0) {
      lht_sd_s2[r, "afr"] <- afr_sd
    } else {
      lht_sd_s2[r, "afr"] <- NA
    }
  } else {
    # File doesn't exist; log a warning and set to NA
    warning(sprintf("Summary file not found: %s", summary_file))
    lht_sd_s2[r, "afr"] <- NA
  }
  
}
#check it
lht_sd_s2


### Age at last reproduction ----

# Directory for summary files
summary_dir <- "./RQ_2/lht_summaries_s2"


# Dynamically calculate lht_sd_s2
for (r in 1:nrow(lht_sd_s2)) {
  
  # Construct the file name for the corresponding summary file
  summary_file <- file.path(summary_dir, sprintf("summary_lht_list_r%d.fst", r))
  
  # Check if the summary file exists
  if (file.exists(summary_file)) {
    # Read the summary file
    summary_data <- read_fst(summary_file)
    
    # Extract the sd for "alr" if it exists
    alr_sd <- summary_data[summary_data$stat == "sd" & !is.na(summary_data$alr), "alr"]
    
    # Assign the value to lht_sd_s2
    if (length(alr_sd) > 0) {
      lht_sd_s2[r, "alr"] <- alr_sd
    } else {
      lht_sd_s2[r, "alr"] <- NA
    }
  } else {
    # File doesn't exist; log a warning and set to NA
    warning(sprintf("Summary file not found: %s", summary_file))
    lht_sd_s2[r, "alr"] <- NA
  }
  
}
#check it
lht_sd_s2


### Age at menopause ----

# Directory for summary files
summary_dir <- "./RQ_2/lht_summaries_s2"


# Dynamically calculate lht_sd_s2
for (r in 1:nrow(lht_sd_s2)) {
  
  # Construct the file name for the corresponding summary file
  summary_file <- file.path(summary_dir, sprintf("summary_lht_list_r%d.fst", r))
  
  # Check if the summary file exists
  if (file.exists(summary_file)) {
    # Read the summary file
    summary_data <- read_fst(summary_file)
    
    # Extract the sd for "meno" if it exists
    meno_sd <- summary_data[summary_data$stat == "sd" & !is.na(summary_data$meno), "meno"]
    
    # Assign the value to lht_sd_s2
    if (length(meno_sd) > 0) {
      lht_sd_s2[r, "meno"] <- meno_sd
    } else {
      lht_sd_s2[r, "meno"] <- NA
    }
  } else {
    # File doesn't exist; log a warning and set to NA
    warning(sprintf("Summary file not found: %s", summary_file))
    lht_sd_s2[r, "meno"] <- NA
  }
  
}
#check it
lht_sd_s2

### Save the data ----

saveRDS(lht_sd_s2,file="./RQ_2/lht_sd_s2.RData")

## Min sorted by life history trait ----


# Get parameter ranges
r_values <- 1:10

### Longevity ----

# Reinitialize lht_min_s2 dynamically
lht_min_s2 <- as.data.frame(matrix(NA, nrow = length(r_values), ncol = 7))
colnames(lht_min_s2) <- c("r","lng","lro","asm","afr","alr","meno")
lht_min_s2$r <- r_values # Update to match your specific production probabilities

# Directory for summary files
summary_dir <- "./RQ_2/lht_summaries_s2"

# Dynamically calculate lht_min_s2
for (r in 1:nrow(lht_min_s2)) {
  
  # Construct the file name for the corresponding summary file
  summary_file <- file.path(summary_dir, sprintf("summary_lht_list_r%d.fst", r))
  
  # Check if the summary file exists
  if (file.exists(summary_file)) {
    # Read the summary file
    summary_data <- read_fst(summary_file)
    
    # Extract the min for "lng" if it exists
    lng_min <- summary_data[summary_data$stat == "min" & !is.na(summary_data$lng), "lng"]
    
    # Assign the value to lht_min_s2
    if (length(lng_min) > 0) {
      lht_min_s2[r, "lng"] <- lng_min
    } else {
      lht_min_s2[r, "lng"] <- NA
    }
  } else {
    # File doesn't exist; log a warning and set to NA
    warning(sprintf("Summary file not found: %s", summary_file))
    lht_min_s2[r, "lng"] <- NA
  }
  
}
#check it
lht_min_s2

### Lifetime reproductive ouput ----

# Directory for summary files
summary_dir <- "./RQ_2/lht_summaries_s2"


# Dynamically calculate lht_min_s2
for (r in 1:nrow(lht_min_s2)) {
  
  # Construct the file name for the corresponding summary file
  summary_file <- file.path(summary_dir, sprintf("summary_lht_list_r%d.fst", r))
  
  # Check if the summary file exists
  if (file.exists(summary_file)) {
    # Read the summary file
    summary_data <- read_fst(summary_file)
    
    # Extract the min for "lro" if it exists
    lro_min <- summary_data[summary_data$stat == "min" & !is.na(summary_data$lro), "lro"]
    
    # Assign the value to lht_min_s2
    if (length(lro_min) > 0) {
      lht_min_s2[r, "lro"] <- lro_min
    } else {
      lht_min_s2[r, "lro"] <- NA
    }
  } else {
    # File doesn't exist; log a warning and set to NA
    warning(sprintf("Summary file not found: %s", summary_file))
    lht_min_s2[r, "lro"] <- NA
  }
  
}
#check it
lht_min_s2


### Age at sexual maturity ----

# Directory for summary files
summary_dir <- "./RQ_2/lht_summaries_s2"


# Dynamically calculate lht_min_s2
for (r in 1:nrow(lht_min_s2)) {
  
  # Construct the file name for the corresponding summary file
  summary_file <- file.path(summary_dir, sprintf("summary_lht_list_r%d.fst", r))
  
  # Check if the summary file exists
  if (file.exists(summary_file)) {
    # Read the summary file
    summary_data <- read_fst(summary_file)
    
    # Extract the min for "asm" if it exists
    asm_min <- summary_data[summary_data$stat == "min" & !is.na(summary_data$asm), "asm"]
    
    # Assign the value to lht_min_s2
    if (length(asm_min) > 0) {
      lht_min_s2[r, "asm"] <- asm_min
    } else {
      lht_min_s2[r, "asm"] <- NA
    }
  } else {
    # File doesn't exist; log a warning and set to NA
    warning(sprintf("Summary file not found: %s", summary_file))
    lht_min_s2[r, "asm"] <- NA
  }
  
}
#check it
lht_min_s2


### Age at first reproduction ----

# Directory for summary files
summary_dir <- "./RQ_2/lht_summaries_s2"


# Dynamically calculate lht_min_s2
for (r in 1:nrow(lht_min_s2)) {
  
  # Construct the file name for the corresponding summary file
  summary_file <- file.path(summary_dir, sprintf("summary_lht_list_r%d.fst", r))
  
  # Check if the summary file exists
  if (file.exists(summary_file)) {
    # Read the summary file
    summary_data <- read_fst(summary_file)
    
    # Extract the min for "afr" if it exists
    afr_min <- summary_data[summary_data$stat == "min" & !is.na(summary_data$afr), "afr"]
    
    # Assign the value to lht_min_s2
    if (length(afr_min) > 0) {
      lht_min_s2[r, "afr"] <- afr_min
    } else {
      lht_min_s2[r, "afr"] <- NA
    }
  } else {
    # File doesn't exist; log a warning and set to NA
    warning(sprintf("Summary file not found: %s", summary_file))
    lht_min_s2[r, "afr"] <- NA
  }
  
}
#check it
lht_min_s2


### Age at last reproduction ----

# Directory for summary files
summary_dir <- "./RQ_2/lht_summaries_s2"


# Dynamically calculate lht_min_s2
for (r in 1:nrow(lht_min_s2)) {
  
  # Construct the file name for the corresponding summary file
  summary_file <- file.path(summary_dir, sprintf("summary_lht_list_r%d.fst", r))
  
  # Check if the summary file exists
  if (file.exists(summary_file)) {
    # Read the summary file
    summary_data <- read_fst(summary_file)
    
    # Extract the min for "alr" if it exists
    alr_min <- summary_data[summary_data$stat == "min" & !is.na(summary_data$alr), "alr"]
    
    # Assign the value to lht_min_s2
    if (length(alr_min) > 0) {
      lht_min_s2[r, "alr"] <- alr_min
    } else {
      lht_min_s2[r, "alr"] <- NA
    }
  } else {
    # File doesn't exist; log a warning and set to NA
    warning(sprintf("Summary file not found: %s", summary_file))
    lht_min_s2[r, "alr"] <- NA
  }
  
}
#check it
lht_min_s2


### Age at menopause ----

# Directory for summary files
summary_dir <- "./RQ_2/lht_summaries_s2"


# Dynamically calculate lht_min_s2
for (r in 1:nrow(lht_min_s2)) {
  
  # Construct the file name for the corresponding summary file
  summary_file <- file.path(summary_dir, sprintf("summary_lht_list_r%d.fst", r))
  
  # Check if the summary file exists
  if (file.exists(summary_file)) {
    # Read the summary file
    summary_data <- read_fst(summary_file)
    
    # Extract the min for "meno" if it exists
    meno_min <- summary_data[summary_data$stat == "min" & !is.na(summary_data$meno), "meno"]
    
    # Assign the value to lht_min_s2
    if (length(meno_min) > 0) {
      lht_min_s2[r, "meno"] <- meno_min
    } else {
      lht_min_s2[r, "meno"] <- NA
    }
  } else {
    # File doesn't exist; log a warning and set to NA
    warning(sprintf("Summary file not found: %s", summary_file))
    lht_min_s2[r, "meno"] <- NA
  }
  
}
#check it
lht_min_s2

### Save the data ----

saveRDS(lht_min_s2,file="./RQ_2/lht_min_s2.RData")

## Max sorted by life history trait ----


# Get parameter ranges
r_values <- 1:10

### Longevity ----

# Reinitialize lht_max_s2 dynamically
lht_max_s2 <- as.data.frame(matrix(NA, nrow = length(r_values), ncol = 7))
colnames(lht_max_s2) <- c("r","lng","lro","asm","afr","alr","meno")
lht_max_s2$r <- r_values # Update to match your specific production probabilities

# Directory for summary files
summary_dir <- "./RQ_2/lht_summaries_s2"

# Dynamically calculate lht_max_s2
for (r in 1:nrow(lht_max_s2)) {
  
  # Construct the file name for the corresponding summary file
  summary_file <- file.path(summary_dir, sprintf("summary_lht_list_r%d.fst", r))
  
  # Check if the summary file exists
  if (file.exists(summary_file)) {
    # Read the summary file
    summary_data <- read_fst(summary_file)
    
    # Extract the max for "lng" if it exists
    lng_max <- summary_data[summary_data$stat == "max" & !is.na(summary_data$lng), "lng"]
    
    # Assign the value to lht_max_s2
    if (length(lng_max) > 0) {
      lht_max_s2[r, "lng"] <- lng_max
    } else {
      lht_max_s2[r, "lng"] <- NA
    }
  } else {
    # File doesn't exist; log a warning and set to NA
    warning(sprintf("Summary file not found: %s", summary_file))
    lht_max_s2[r, "lng"] <- NA
  }
  
}
#check it
lht_max_s2

### Lifetime reproductive ouput ----

# Directory for summary files
summary_dir <- "./RQ_2/lht_summaries_s2"


# Dynamically calculate lht_max_s2
for (r in 1:nrow(lht_max_s2)) {
  
  # Construct the file name for the corresponding summary file
  summary_file <- file.path(summary_dir, sprintf("summary_lht_list_r%d.fst", r))
  
  # Check if the summary file exists
  if (file.exists(summary_file)) {
    # Read the summary file
    summary_data <- read_fst(summary_file)
    
    # Extract the max for "lro" if it exists
    lro_max <- summary_data[summary_data$stat == "max" & !is.na(summary_data$lro), "lro"]
    
    # Assign the value to lht_max_s2
    if (length(lro_max) > 0) {
      lht_max_s2[r, "lro"] <- lro_max
    } else {
      lht_max_s2[r, "lro"] <- NA
    }
  } else {
    # File doesn't exist; log a warning and set to NA
    warning(sprintf("Summary file not found: %s", summary_file))
    lht_max_s2[r, "lro"] <- NA
  }
  
}
#check it
lht_max_s2


### Age at sexual maturity ----

# Directory for summary files
summary_dir <- "./RQ_2/lht_summaries_s2"


# Dynamically calculate lht_max_s2
for (r in 1:nrow(lht_max_s2)) {
  
  # Construct the file name for the corresponding summary file
  summary_file <- file.path(summary_dir, sprintf("summary_lht_list_r%d.fst", r))
  
  # Check if the summary file exists
  if (file.exists(summary_file)) {
    # Read the summary file
    summary_data <- read_fst(summary_file)
    
    # Extract the max for "asm" if it exists
    asm_max <- summary_data[summary_data$stat == "max" & !is.na(summary_data$asm), "asm"]
    
    # Assign the value to lht_max_s2
    if (length(asm_max) > 0) {
      lht_max_s2[r, "asm"] <- asm_max
    } else {
      lht_max_s2[r, "asm"] <- NA
    }
  } else {
    # File doesn't exist; log a warning and set to NA
    warning(sprintf("Summary file not found: %s", summary_file))
    lht_max_s2[r, "asm"] <- NA
  }
  
}
#check it
lht_max_s2


### Age at first reproduction ----

# Directory for summary files
summary_dir <- "./RQ_2/lht_summaries_s2"


# Dynamically calculate lht_max_s2
for (r in 1:nrow(lht_max_s2)) {
  
  # Construct the file name for the corresponding summary file
  summary_file <- file.path(summary_dir, sprintf("summary_lht_list_r%d.fst", r))
  
  # Check if the summary file exists
  if (file.exists(summary_file)) {
    # Read the summary file
    summary_data <- read_fst(summary_file)
    
    # Extract the max for "afr" if it exists
    afr_max <- summary_data[summary_data$stat == "max" & !is.na(summary_data$afr), "afr"]
    
    # Assign the value to lht_max_s2
    if (length(afr_max) > 0) {
      lht_max_s2[r, "afr"] <- afr_max
    } else {
      lht_max_s2[r, "afr"] <- NA
    }
  } else {
    # File doesn't exist; log a warning and set to NA
    warning(sprintf("Summary file not found: %s", summary_file))
    lht_max_s2[r, "afr"] <- NA
  }
  
}
#check it
lht_max_s2


### Age at last reproduction ----

# Directory for summary files
summary_dir <- "./RQ_2/lht_summaries_s2"


# Dynamically calculate lht_max_s2
for (r in 1:nrow(lht_max_s2)) {
  
  # Construct the file name for the corresponding summary file
  summary_file <- file.path(summary_dir, sprintf("summary_lht_list_r%d.fst", r))
  
  # Check if the summary file exists
  if (file.exists(summary_file)) {
    # Read the summary file
    summary_data <- read_fst(summary_file)
    
    # Extract the max for "alr" if it exists
    alr_max <- summary_data[summary_data$stat == "max" & !is.na(summary_data$alr), "alr"]
    
    # Assign the value to lht_max_s2
    if (length(alr_max) > 0) {
      lht_max_s2[r, "alr"] <- alr_max
    } else {
      lht_max_s2[r, "alr"] <- NA
    }
  } else {
    # File doesn't exist; log a warning and set to NA
    warning(sprintf("Summary file not found: %s", summary_file))
    lht_max_s2[r, "alr"] <- NA
  }
  
}
#check it
lht_max_s2


### Age at menopause ----

# Directory for summary files
summary_dir <- "./RQ_2/lht_summaries_s2"


# Dynamically calculate lht_max_s2
for (r in 1:nrow(lht_max_s2)) {
  
  # Construct the file name for the corresponding summary file
  summary_file <- file.path(summary_dir, sprintf("summary_lht_list_r%d.fst", r))
  
  # Check if the summary file exists
  if (file.exists(summary_file)) {
    # Read the summary file
    summary_data <- read_fst(summary_file)
    
    # Extract the max for "meno" if it exists
    meno_max <- summary_data[summary_data$stat == "max" & !is.na(summary_data$meno), "meno"]
    
    # Assign the value to lht_max_s2
    if (length(meno_max) > 0) {
      lht_max_s2[r, "meno"] <- meno_max
    } else {
      lht_max_s2[r, "meno"] <- NA
    }
  } else {
    # File doesn't exist; log a warning and set to NA
    warning(sprintf("Summary file not found: %s", summary_file))
    lht_max_s2[r, "meno"] <- NA
  }
  
}
#check it
lht_max_s2

### Save the data ----

saveRDS(lht_max_s2,file="./RQ_2/lht_max_s2.RData")

## CV sorted by life history trait ----


# Get parameter ranges
r_values <- 1:10

### Longevity ----

# Reinitialize lht_cv_s2 dynamically
lht_cv_s2 <- as.data.frame(matrix(NA, nrow = length(r_values), ncol = 7))
colnames(lht_cv_s2) <- c("r","lng","lro","asm","afr","alr","meno")
lht_cv_s2$r <- r_values # Update to match your specific production probabilities

# Directory for summary files
summary_dir <- "./RQ_2/lht_summaries_s2"

# Dynamically calculate lht_cv_s2
for (r in 1:nrow(lht_cv_s2)) {
  
  # Construct the file name for the corresponding summary file
  summary_file <- file.path(summary_dir, sprintf("summary_lht_list_r%d.fst", r))
  
  # Check if the summary file exists
  if (file.exists(summary_file)) {
    # Read the summary file
    summary_data <- read_fst(summary_file)
    
    # Extract the cv for "lng" if it exists
    lng_cv <- summary_data[summary_data$stat == "cv" & !is.na(summary_data$lng), "lng"]
    
    # Assign the value to lht_cv_s2
    if (length(lng_cv) > 0) {
      lht_cv_s2[r, "lng"] <- lng_cv
    } else {
      lht_cv_s2[r, "lng"] <- NA
    }
  } else {
    # File doesn't exist; log a warning and set to NA
    warning(sprintf("Summary file not found: %s", summary_file))
    lht_cv_s2[r, "lng"] <- NA
  }
  
}
#check it
lht_cv_s2

### Lifetime reproductive ouput ----

# Directory for summary files
summary_dir <- "./RQ_2/lht_summaries_s2"


# Dynamically calculate lht_cv_s2
for (r in 1:nrow(lht_cv_s2)) {
  
  # Construct the file name for the corresponding summary file
  summary_file <- file.path(summary_dir, sprintf("summary_lht_list_r%d.fst", r))
  
  # Check if the summary file exists
  if (file.exists(summary_file)) {
    # Read the summary file
    summary_data <- read_fst(summary_file)
    
    # Extract the cv for "lro" if it exists
    lro_cv <- summary_data[summary_data$stat == "cv" & !is.na(summary_data$lro), "lro"]
    
    # Assign the value to lht_cv_s2
    if (length(lro_cv) > 0) {
      lht_cv_s2[r, "lro"] <- lro_cv
    } else {
      lht_cv_s2[r, "lro"] <- NA
    }
  } else {
    # File doesn't exist; log a warning and set to NA
    warning(sprintf("Summary file not found: %s", summary_file))
    lht_cv_s2[r, "lro"] <- NA
  }
  
}
#check it
lht_cv_s2


### Age at sexual maturity ----

# Directory for summary files
summary_dir <- "./RQ_2/lht_summaries_s2"


# Dynamically calculate lht_cv_s2
for (r in 1:nrow(lht_cv_s2)) {
  
  # Construct the file name for the corresponding summary file
  summary_file <- file.path(summary_dir, sprintf("summary_lht_list_r%d.fst", r))
  
  # Check if the summary file exists
  if (file.exists(summary_file)) {
    # Read the summary file
    summary_data <- read_fst(summary_file)
    
    # Extract the cv for "asm" if it exists
    asm_cv <- summary_data[summary_data$stat == "cv" & !is.na(summary_data$asm), "asm"]
    
    # Assign the value to lht_cv_s2
    if (length(asm_cv) > 0) {
      lht_cv_s2[r, "asm"] <- asm_cv
    } else {
      lht_cv_s2[r, "asm"] <- NA
    }
  } else {
    # File doesn't exist; log a warning and set to NA
    warning(sprintf("Summary file not found: %s", summary_file))
    lht_cv_s2[r, "asm"] <- NA
  }
  
}
#check it
lht_cv_s2


### Age at first reproduction ----

# Directory for summary files
summary_dir <- "./RQ_2/lht_summaries_s2"


# Dynamically calculate lht_cv_s2
for (r in 1:nrow(lht_cv_s2)) {
  
  # Construct the file name for the corresponding summary file
  summary_file <- file.path(summary_dir, sprintf("summary_lht_list_r%d.fst", r))
  
  # Check if the summary file exists
  if (file.exists(summary_file)) {
    # Read the summary file
    summary_data <- read_fst(summary_file)
    
    # Extract the cv for "afr" if it exists
    afr_cv <- summary_data[summary_data$stat == "cv" & !is.na(summary_data$afr), "afr"]
    
    # Assign the value to lht_cv_s2
    if (length(afr_cv) > 0) {
      lht_cv_s2[r, "afr"] <- afr_cv
    } else {
      lht_cv_s2[r, "afr"] <- NA
    }
  } else {
    # File doesn't exist; log a warning and set to NA
    warning(sprintf("Summary file not found: %s", summary_file))
    lht_cv_s2[r, "afr"] <- NA
  }
  
}
#check it
lht_cv_s2


### Age at last reproduction ----

# Directory for summary files
summary_dir <- "./RQ_2/lht_summaries_s2"


# Dynamically calculate lht_cv_s2
for (r in 1:nrow(lht_cv_s2)) {
  
  # Construct the file name for the corresponding summary file
  summary_file <- file.path(summary_dir, sprintf("summary_lht_list_r%d.fst", r))
  
  # Check if the summary file exists
  if (file.exists(summary_file)) {
    # Read the summary file
    summary_data <- read_fst(summary_file)
    
    # Extract the cv for "alr" if it exists
    alr_cv <- summary_data[summary_data$stat == "cv" & !is.na(summary_data$alr), "alr"]
    
    # Assign the value to lht_cv_s2
    if (length(alr_cv) > 0) {
      lht_cv_s2[r, "alr"] <- alr_cv
    } else {
      lht_cv_s2[r, "alr"] <- NA
    }
  } else {
    # File doesn't exist; log a warning and set to NA
    warning(sprintf("Summary file not found: %s", summary_file))
    lht_cv_s2[r, "alr"] <- NA
  }
  
}
#check it
lht_cv_s2


### Age at menopause ----

# Directory for summary files
summary_dir <- "./RQ_2/lht_summaries_s2"


# Dynamically calculate lht_cv_s2
for (r in 1:nrow(lht_cv_s2)) {
  
  # Construct the file name for the corresponding summary file
  summary_file <- file.path(summary_dir, sprintf("summary_lht_list_r%d.fst", r))
  
  # Check if the summary file exists
  if (file.exists(summary_file)) {
    # Read the summary file
    summary_data <- read_fst(summary_file)
    
    # Extract the cv for "meno" if it exists
    meno_cv <- summary_data[summary_data$stat == "cv" & !is.na(summary_data$meno), "meno"]
    
    # Assign the value to lht_cv_s2
    if (length(meno_cv) > 0) {
      lht_cv_s2[r, "meno"] <- meno_cv
    } else {
      lht_cv_s2[r, "meno"] <- NA
    }
  } else {
    # File doesn't exist; log a warning and set to NA
    warning(sprintf("Summary file not found: %s", summary_file))
    lht_cv_s2[r, "meno"] <- NA
  }
  
}
#check it
lht_cv_s2

### Save the data ----

saveRDS(lht_cv_s2,file="./RQ_2/lht_cv_s2.RData")

# ##Average life cycle ----
# 
# ###Longevity ----
# 
# #p = 0.75
# apply(lht_mean_s2[2:11],1,mean,na.rm=T)[which(lht_mean_s2$r == 0.75)]
# apply(lng_sd_s2[2:11],1,mean,na.rm=T)[which(lht_mean_s2$r == 0.75)]
# apply(lng_min_s2[2:11],1,mean,na.rm=T)[which(lht_mean_s2$r == 0.75)]
# apply(lng_max_s2[2:11],1,mean,na.rm=T)[which(lht_mean_s2$r == 0.75)]
# #p = 0.55
# apply(lht_mean_s2[2:11],1,mean,na.rm=T)[which(lht_mean_s2$r == 0.6)]
# apply(lng_sd_s2[2:11],1,mean,na.rm=T)[which(lht_mean_s2$r == 0.6)]
# apply(lng_min_s2[2:11],1,mean,na.rm=T)[which(lht_mean_s2$r == 0.6)]
# apply(lng_max_s2[2:11],1,mean,na.rm=T)[which(lht_mean_s2$r == 0.6)]
# #p = 0.90
# apply(lht_mean_s2[2:11],1,mean,na.rm=T)[which(lht_mean_s2$r == 0.9)]
# apply(lng_sd_s2[2:11],1,mean,na.rm=T)[which(lht_mean_s2$r == 0.9)]
# apply(lng_min_s2[2:11],1,mean,na.rm=T)[which(lht_mean_s2$r == 0.9)]
# apply(lng_max_s2[2:11],1,mean,na.rm=T)[which(lht_mean_s2$r == 0.9)]
# 
# ###Lifetime reproductive output ----
# 
# #p = 0.7
# apply(lro_mean_s2[2:11],1,mean,na.rm=T)[which(lro_mean_s2$r == 0.75)]
# apply(lro_sd_s2[2:11],1,mean,na.rm=T)[which(lro_mean_s2$r == 0.75)]
# apply(lro_min_s2[2:11],1,mean,na.rm=T)[which(lro_mean_s2$r == 0.75)]
# apply(lro_max_s2[2:11],1,mean,na.rm=T)[which(lro_mean_s2$r == 0.75)]
# #p = 0.55
# apply(lro_mean_s2[2:11],1,mean,na.rm=T)[which(lro_mean_s2$r == 0.6)]
# apply(lro_sd_s2[2:11],1,mean,na.rm=T)[which(lro_mean_s2$r == 0.6)]
# apply(lro_min_s2[2:11],1,mean,na.rm=T)[which(lro_mean_s2$r == 0.6)]
# apply(lro_max_s2[2:11],1,mean,na.rm=T)[which(lro_mean_s2$r == 0.6)]
# #p = 0.9
# apply(lro_mean_s2[2:11],1,mean,na.rm=T)[which(lro_mean_s2$r == 0.9)]
# apply(lro_sd_s2[2:11],1,mean,na.rm=T)[which(lro_mean_s2$r == 0.9)]
# apply(lro_min_s2[2:11],1,mean,na.rm=T)[which(lro_mean_s2$r == 0.9)]
# apply(lro_max_s2[2:11],1,mean,na.rm=T)[which(lro_mean_s2$r == 0.9)]
# 
# ###Age at sexual maturity ----
# 
# #p = 0.75
# apply(asm_mean_s2[2:11],1,mean,na.rm=T)[which(asm_mean_s2$r == 0.75)]
# apply(asm_sd_s2[2:11],1,mean,na.rm=T)[which(asm_mean_s2$r == 0.75)]
# apply(asm_min_s2[2:11],1,mean,na.rm=T)[which(asm_mean_s2$r == 0.75)]
# apply(asm_max_s2[2:11],1,mean,na.rm=T)[which(asm_mean_s2$r == 0.75)]
# #p = 0.55
# apply(asm_mean_s2[2:11],1,mean,na.rm=T)[which(asm_mean_s2$r == 0.65)]
# apply(asm_sd_s2[2:11],1,mean,na.rm=T)[which(asm_mean_s2$r == 0.65)]
# apply(asm_min_s2[2:11],1,mean,na.rm=T)[which(asm_mean_s2$r == 0.65)]
# apply(asm_max_s2[2:11],1,mean,na.rm=T)[which(asm_mean_s2$r == 0.65)]
# #p = 0.9
# apply(asm_mean_s2[2:11],1,mean,na.rm=T)[which(asm_mean_s2$r == 0.9)]
# apply(asm_sd_s2[2:11],1,mean,na.rm=T)[which(asm_mean_s2$r == 0.9)]
# apply(asm_min_s2[2:11],1,mean,na.rm=T)[which(asm_mean_s2$r == 0.9)]
# apply(asm_max_s2[2:11],1,mean,na.rm=T)[which(asm_mean_s2$r == 0.9)]
# 
# ###Age at first reproduction ----
# 
# #p = 0.75
# apply(afr_mean_s2[2:11],1,mean,na.rm=T)[which(afr_mean_s2$r == 0.75)]
# apply(afr_sd_s2[2:11],1,mean,na.rm=T)[which(afr_mean_s2$r == 0.75)]
# apply(afr_min_s2[2:11],1,mean,na.rm=T)[which(afr_mean_s2$r == 0.75)]
# apply(afr_max_s2[2:11],1,mean,na.rm=T)[which(afr_mean_s2$r == 0.75)]
# #p = 0.55
# apply(afr_mean_s2[2:11],1,mean,na.rm=T)[which(afr_mean_s2$r == 0.65)]
# apply(afr_sd_s2[2:11],1,mean,na.rm=T)[which(afr_mean_s2$r == 0.65)]
# apply(afr_min_s2[2:11],1,mean,na.rm=T)[which(afr_mean_s2$r == 0.65)]
# apply(afr_max_s2[2:11],1,mean,na.rm=T)[which(afr_mean_s2$r == 0.65)]
# #p = 0.9
# apply(afr_mean_s2[2:11],1,mean,na.rm=T)[which(afr_mean_s2$r == 0.9)]
# apply(afr_sd_s2[2:11],1,mean,na.rm=T)[which(afr_mean_s2$r == 0.9)]
# apply(afr_min_s2[2:11],1,mean,na.rm=T)[which(afr_mean_s2$r == 0.9)]
# apply(afr_max_s2[2:11],1,mean,na.rm=T)[which(afr_mean_s2$r == 0.9)]
# 
# ###Age at last reproduction ----
# 
# #p = 0.7
# apply(alr_mean_s2[2:11],1,mean,na.rm=T)[which(alr_mean_s2$r == 0.75)]
# apply(alr_sd_s2[2:11],1,mean,na.rm=T)[which(alr_mean_s2$r == 0.75)]
# apply(alr_min_s2[2:11],1,mean,na.rm=T)[which(alr_mean_s2$r == 0.75)]
# apply(alr_max_s2[2:11],1,mean,na.rm=T)[which(alr_mean_s2$r == 0.75)]
# #p = 0.55
# apply(alr_mean_s2[2:11],1,mean,na.rm=T)[which(alr_mean_s2$r == 0.65)]
# apply(alr_sd_s2[2:11],1,mean,na.rm=T)[which(alr_mean_s2$r == 0.65)]
# apply(alr_min_s2[2:11],1,mean,na.rm=T)[which(alr_mean_s2$r == 0.65)]
# apply(alr_max_s2[2:11],1,mean,na.rm=T)[which(alr_mean_s2$r == 0.65)]
# #p = 0.9
# apply(alr_mean_s2[2:11],1,mean,na.rm=T)[which(alr_mean_s2$r == 0.9)]
# apply(alr_sd_s2[2:11],1,mean,na.rm=T)[which(alr_mean_s2$r == 0.9)]
# apply(alr_min_s2[2:11],1,mean,na.rm=T)[which(alr_mean_s2$r == 0.9)]
# apply(alr_max_s2[2:11],1,mean,na.rm=T)[which(alr_mean_s2$r == 0.9)]
# 
# ###Age at menopause ----
# 
# #p = 0.7
# apply(meno_mean_s2[2:11],1,mean,na.rm=T)[which(meno_mean_s2$r == 0.75)]
# apply(meno_sd_s2[2:11],1,mean,na.rm=T)[which(meno_mean_s2$r == 0.75)]
# apply(meno_min_s2[2:11],1,mean,na.rm=T)[which(meno_mean_s2$r == 0.75)]
# apply(meno_max_s2[2:11],1,mean,na.rm=T)[which(meno_mean_s2$r == 0.75)]
# #p = 0.55
# apply(meno_mean_s2[2:11],1,mean,na.rm=T)[which(meno_mean_s2$r == 0.65)]
# apply(meno_sd_s2[2:11],1,mean,na.rm=T)[which(meno_mean_s2$r == 0.65)]
# apply(meno_min_s2[2:11],1,mean,na.rm=T)[which(meno_mean_s2$r == 0.65)]
# apply(meno_max_s2[2:11],1,mean,na.rm=T)[which(meno_mean_s2$r == 0.65)]
# #p = 0.9
# apply(meno_mean_s2[2:11],1,mean,na.rm=T)[which(meno_mean_s2$r == 0.9)]
# apply(meno_sd_s2[2:11],1,mean,na.rm=T)[which(meno_mean_s2$r == 0.9)]
# apply(meno_min_s2[2:11],1,mean,na.rm=T)[which(meno_mean_s2$r == 0.9)]
# apply(meno_max_s2[2:11],1,mean,na.rm=T)[which(meno_mean_s2$r == 0.9)]
# 
# 
