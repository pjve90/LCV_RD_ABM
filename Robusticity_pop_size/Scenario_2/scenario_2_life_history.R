# Life cycle variation and resource dynamics ABM: Scenario 2 - life history calculation ----

#Here is the code to calculate the life history tratis from the simulation of Scenario 2, which aims to understand the influence of resource production on the variability of life cycles.

#Production: parameter sweep between 0.1 and 0.9 (17 values)
#Transfers: null
#Habitat quality: baseline

#R settings ----

getwd()
#setwd("./LCV_RD_ABM")

#install packages
#parallel package
#install.packages("parallel")
library(parallel)
#doParallel
#install.packages("doParrallel")
library(doParallel)
#foreach package
#install.packages("foreach")
library(foreach)
#fst package
#install.packages("fst")
library(fst)

#Data wrangling ----

# Initialize an empty list to store the data
raw_sim <- list()

# Loop over the values of d and r
  for (r in 1:10) {
    # Construct the file path
    file_path <- sprintf("./Robusticity_pop_size/Scenario_2/fst_results/results_r%d.fst", r)
    
    # Check if the file exists
    if (file.exists(file_path)) {
      # Read the .fst file
      raw_sim <- read_fst(file_path)
      
      # Apply the filtering criteria
      filtered_sample <- raw_sim[!(raw_sim$id %in% raw_sim$id[raw_sim$year < 100]), ]
      filtered_sample <- filtered_sample[!(filtered_sample$id %in% filtered_sample$id[filtered_sample$year > 200 & filtered_sample$age == 0]), ]
      
      # Check if the directory exists, if not, create it
      if (!dir.exists("./Robusticity_pop_size/Scenario_2/samples/")) {
        dir.create("./Robusticity_pop_size/Scenario_2/samples/", recursive = TRUE)
      }
      
      # Save each filtered sample as an .fst file
      output_file <- sprintf("./Robusticity_pop_size/Scenario_2/samples/raw_sample_r%d.fst", r)
      write_fst(filtered_sample, output_file)
    } else {
      warning(sprintf("File not found: %s", file_path))
    }
  }

#Import functions to calculate life history traits ----

#longevity
source("./Model_code/life_history_lng.R")

#lifetime reproductive output
source("./Model_code/life_history_lro.R")

#age at sexual maturity
source("./Model_code/life_history_asm.R")

#age at first reproduction
source("./Model_code/life_history_afr.R")

#age at last reproduction
source("./Model_code/life_history_alr.R")

#age at menopause
source("./Model_code/life_history_meno.R")

# Calculate life history traits ----

# Define the batch process function
batch_process <- function(batch_index, input_dir, output_dir, chunk_size) {
  # Get a chunk of files dynamically based on the batch index and chunk size
  all_files <- list.files(input_dir, pattern = "raw_sample_r[0-9]+\\.fst$", full.names = TRUE)
  start_idx <- (batch_index - 1) * chunk_size + 1
  end_idx <- min(batch_index * chunk_size, length(all_files))
  
  # If there are no files left for this batch, return
  if (start_idx > length(all_files)) return(NULL)
  
  # Loop through each file in the batch
  for (file_path in all_files[start_idx:end_idx]) {
    # Extract the 'd' and 'r' values from the filename using regex
    matches <- regexpr("r([0-9]+)", basename(file_path))
    if (matches[1] != -1) {
      # The result of regmatches should be accessed with the `matches` object
      match_values <- regmatches(basename(file_path), matches)
      # Split the match_values to get d_value and r_value
      r_value <- as.integer(sub("r([0-9]+)", "\\1", match_values))
      
      # Read the input file
      raw_sample <- read_fst(file_path)
      
      # Skip processing if the data frame is empty
      if (nrow(raw_sample) == 0) {
        lht_list <- list(raw_sample)  # Use the empty raw_sample directly
        
        # Construct the output file path based on d and r
        output_file <- file.path(output_dir, sprintf("lht_list_r%d.fst", r_value))
        
        # Save the empty lht_list as an .fst file
        write_fst(lht_list[[1]], output_file)
        
        next  # Skip to the next file in the batch
      }
      
      # Apply the longevity function to calculate maximum age (longevity)
      lht_list <- list(raw_sample)
      lht_list[[1]] <- longevity(lht_list[[1]], raw_sample)
      lht_list[[1]] <- lifetime_reproductive_output(lht_list[[1]], raw_sample)
      lht_list[[1]] <- asm(lht_list[[1]], raw_sample)
      lht_list[[1]] <- afr(lht_list[[1]], raw_sample)
      lht_list[[1]] <- alr(lht_list[[1]], raw_sample)
      lht_list[[1]] <- meno(lht_list[[1]], raw_sample)
      
      # Construct the output file path based on d and r
      output_file <- file.path(output_dir, sprintf("lht_list_r%d.fst", r_value))
      
      # Save the resulting lht_list as an .fst file
      write_fst(lht_list[[1]], output_file)
    } else {
      warning(sprintf("File does not match expected naming convention: %s", basename(file_path)))
    }
  }
}

# Step 2: Set up parallel processing
num_cores <- 5  # Adjust based on your system
cl <- makeCluster(num_cores)
registerDoParallel(cl)

# Step 3: Define directories and create batches
input_dir <- "./Robusticity_pop_size/Scenario_2/samples/"  # Directory containing raw_sample .fst files
output_dir <- "./Robusticity_pop_size/Scenario_2/lht_lists/"  # Directory for lht_list .fst files

if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}

# Get the total number of files in the input directory
total_files <- length(list.files(input_dir, pattern = "raw_sample_r[0-9]+\\.fst$"))

# Define chunk size (number of files per batch)
chunk_size <- ceiling(total_files / num_cores)

# Create a vector of batch indices
batch_indices <- 1:num_cores

# Step 4: Process batches in parallel
foreach(batch_index = batch_indices, .packages = c("fst")) %dopar% {
  batch_process(batch_index, input_dir, output_dir, chunk_size)
}

# Step 5: Stop the parallel cluster
stopCluster(cl)
