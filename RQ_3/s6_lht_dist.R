#Script to plot the distribution of the life history traits in Scenario 1

# Get parameter values for labeling

# Source the functions
# Stage-specific maximum amount of resource production 
source("./Model_code/production_maxprod_fx.R")
# Stage-specific production probabilities
source("./Model_code/production_prodprob_fx.R")

# Get the values
# Habitat quality
habitat_quality <- 4
# Stage-specific maximum amount of resource production
maxprod <- max_production(habitat_quality)
# Maximum production probability
max_prod_prob <- seq(0.1, 0.9, length = 17)
# Stage-specific production probabilities
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

# Directory for summary files
summary_dir <- "./Scenario_6/lht_lists/"

# Distribution of life history traits ----

### Longevity ----

#get the frequency of each age across parameter values and repetitions
for (d in 1:17) {
  for (m in 1:19) {
    # create a data frame for each parameter value
    assign(paste("d", d, "m", m, sep = "_"), data.frame(matrix(0, nrow = 0, ncol = 101)))
    # retrieve the data frame
    temp_df <- get(paste("d", d, "m", m, sep = "_"))
    # modify the column names
    colnames(temp_df) <- as.character(0:100)
    # reassign the modified data frame in the original data frame
    assign(paste("d", d, "m", m, sep = "_"), temp_df)
    
    # Loop through repetitions
    for (r in 1:10) {
      # Construct the file name for the corresponding summary file
      summary_file <- file.path(summary_dir, sprintf("lht_list_d%d_m%d_r%d.fst", d, m, r))
      
      # Check if the summary file exists
      if (file.exists(summary_file)) {
        # Read the summary file
        results_s6 <- read_fst(summary_file)
        
        # Extract the longevity (assuming it's stored under a column "lng")
        lng_vector <- results_s6$lng
        
        # Create a new row with zeros
        new_row <- rep(0, 101)
        
        # Update the new row based on longevity values
        for (val in lng_vector) {
          if (val >= 0 & val <= 100) {
            new_row[val + 1] <- new_row[val + 1] + 1  # +1 to account for index starting at 1 in R
          }
        }
        
        # Change column names to match
        new_row_df <- as.data.frame(t(new_row))
        colnames(new_row_df) <- as.character(0:100)
        
        # Add new row into the data frame
        temp_df <- get(paste("d", d, "m", m, sep = "_"))  # current data frame
        temp_df <- rbind(temp_df, new_row_df)  # new row
        assign(paste("d", d, "m", m, sep = "_"), temp_df)  # updated data frame
      } else {
        warning(sprintf("Summary file not found: %s", summary_file))
      }
    }
  }
}

#### Plot the densities ----

# Initialize the named list to store valid combinations of (d, m)
valid_indices <- list()

# Loop through all combinations of d and m
for (d in 1:17) {
  for (m in 1:19) {
    # Dynamically construct the variable name for the current data frame
    df_name <- paste("d", d, "m", m, sep = "_")
    
    # Check if the data frame exists to avoid errors
    if (exists(df_name)) {
      # Get the data frame
      df <- get(df_name)
      
      # Check if the sum of means for the data frame is greater than zero
      if (sum(apply(df, 2, mean, na.rm = TRUE)) > 0) {
        # Construct the name for the valid combination
        combination_name <- paste("d", d, "m", m, sep = "_")
        
        # Store the valid combination with the name
        valid_indices[[combination_name]] <- c(d, m)
      }
    }
  }
}

# Check the result
valid_indices

# Step 3: Create a color palette for only the valid data frames
color_palette <- hcl.colors(length(valid_indices), palette = "zissou 1", alpha = 0.5)

# Step 4: Prepare an empty plot
par(mfrow = c(1, 1), mar = c(5, 4, 4, 8))

#get the maximum value for y

# Initialize an empty vector to store the means for all d
all_means <- c()

# Loop through d values from 1 to 17
for (d in 1:17) {
  for (m in 1:19){
    # Dynamically get the data frame name using paste
    current_data <- get(paste("d", d, "m", m, sep = "_"))
    
    # Calculate column means for the current data frame
    column_means <- apply(current_data, 2, mean, na.rm = TRUE)
    
    # Append the column means to the all_means vector
    all_means <- c(all_means, column_means)
  }
}

# Get the maximum value of all the means collected
max_y <- max(all_means, na.rm = TRUE)

#plot it
plot(c(0, 100), c(0,(max_y + 1)),
     xlab = "Age",
     ylab = "Frequency",
     main = "Longevity",
     type = "n")

# Step 5: Loop through valid indices to calculate and plot the KDEs
for(i in 1:length(valid_indices)) {
  # Extract the combination of d and m
  combination <- valid_indices[[i]]
  d <- combination[1]  # First element is d
  m <- combination[2]  # Second element is m
  # Dynamically get the data frame name and data
  data <- get(paste("d", d, "m", m, sep = "_"))
  
  # Plot the line for this valid data frame
  lines(0:(ncol(data)-1),
        apply(data, 2, mean),
        type = "o",
        col = color_palette[i],
        lty = i,
        pch = 16
  )
}

# Step 6: Add a legend
legend_labels <- sapply(valid_indices, function(combination) {
  d <- combination[1]
  m <- combination[2]
  # Combine prod_prob and transf_prob values for each valid (d, m) combination
  prod_value <- prod_prob[2, d]  # Extract the prod_prob for the given d
  transf_value <- transf_prob[m]  # Extract the transf_prob for the given m
  
  # Construct the legend label (you can adjust the format as needed)
  paste("d", d, "_m", m, ": ", round(prod_value, 2), ", ", round(transf_value, 2), sep = "")
})

# Create the legend with the combined labels
legend(x = 105, y = (max_y + 1),
       title = "Resource dynamics\nprobabilities",
       legend = legend_labels, # Use the dynamically created labels
       col = color_palette, # Use the corresponding colors from the palette
       lty = 1:length(valid_indices), # Line types corresponding to each valid combination
       pch = 16, # Points for each combination
       lwd = 2,  # Line width
       bty = "n", # No box around the legend
       xpd = TRUE) # Allow legend to be drawn outside the plot region

### Lifetime reproductive output ----

#get the frequency of each age across parameter values and repetitions
for (d in 1:17) {
  for (m in 1:19) {
    # create a data frame for each parameter value
    assign(paste("d", d, "m", m, sep = "_"), data.frame(matrix(0, nrow = 0, ncol = 101)))
    # retrieve the data frame
    temp_df <- get(paste("d", d, "m", m, sep = "_"))
    # modify the column names
    colnames(temp_df) <- as.character(0:100)
    # reassign the modified data frame in the original data frame
    assign(paste("d", d, "m", m, sep = "_"), temp_df)
    
    # Loop through repetitions
    for (r in 1:10) {
      # Construct the file name for the corresponding summary file
      summary_file <- file.path(summary_dir, sprintf("lht_list_d%d_m%d_r%d.fst", d, m, r))
      
      # Check if the summary file exists
      if (file.exists(summary_file)) {
        # Read the summary file
        results_s6 <- read_fst(summary_file)
        
        # Extract the Lifetime reproductive output (assuming it's stored under a column "lro")
        lro_vector <- results_s6$lro
        
        # Create a new row with zeros
        new_row <- rep(0, 101)
        
        # Update the new row based on Lifetime reproductive output values
        for (val in lro_vector) {
          if (val >= 0 & val <= 100 & !is.na(val)) {
            new_row[val + 1] <- new_row[val + 1] + 1  # +1 to account for index starting at 1 in R
          }
        }
        
        # Change column names to match
        new_row_df <- as.data.frame(t(new_row))
        colnames(new_row_df) <- as.character(0:100)
        
        # Add new row into the data frame
        temp_df <- get(paste("d", d, "m", m, sep = "_"))  # current data frame
        temp_df <- rbind(temp_df, new_row_df)  # new row
        assign(paste("d", d, "m", m, sep = "_"), temp_df)  # updated data frame
      } else {
        warning(sprintf("Summary file not found: %s", summary_file))
      }
    }
  }
}

#### Plot the densities ----

# Initialize the named list to store valid combinations of (d, m)
valid_indices <- list()

# Loop through all combinations of d and m
for (d in 1:17) {
  for (m in 1:19) {
    # Dynamically construct the variable name for the current data frame
    df_name <- paste("d", d, "m", m, sep = "_")
    
    # Check if the data frame exists to avoid errors
    if (exists(df_name)) {
      # Get the data frame
      df <- get(df_name)
      
      # Check if the sum of means for the data frame is greater than zero
      if (sum(apply(df, 2, mean, na.rm = TRUE)) > 0) {
        # Construct the name for the valid combination
        combination_name <- paste("d", d, "m", m, sep = "_")
        
        # Store the valid combination with the name
        valid_indices[[combination_name]] <- c(d, m)
      }
    }
  }
}

# Check the result
valid_indices

# Step 3: Create a color palette for only the valid data frames
color_palette <- hcl.colors(length(valid_indices), palette = "zissou 1", alpha = 0.5)

# Step 4: Prepare an empty plot
par(mfrow = c(1, 1), mar = c(5, 4, 4, 8))

#get the maximum value for y

# Initialize an empty vector to store the means for all d
all_means <- c()

# Loop through d values from 1 to 17
for (d in 1:17) {
  for (m in 1:19){
    # Dynamically get the data frame name using paste
    current_data <- get(paste("d", d, "m", m, sep = "_"))
    
    # Calculate column means for the current data frame
    column_means <- apply(current_data, 2, mean, na.rm = TRUE)
    
    # Append the column means to the all_means vector
    all_means <- c(all_means, column_means)
  }
}

# Get the maximum value of all the means collected
max_y <- max(all_means, na.rm = TRUE)

#plot it
plot(c(0, 100), c(0,(max_y + 1)),
     xlab = "Age",
     ylab = "Frequency",
     main = "Lifetime reproductive output",
     type = "n")

# Step 5: Loop through valid indices to calculate and plot the KDEs
for(i in 1:length(valid_indices)) {
  # Extract the combination of d and m
  combination <- valid_indices[[i]]
  d <- combination[1]  # First element is d
  m <- combination[2]  # Second element is m
  # Dynamically get the data frame name and data
  data <- get(paste("d", d, "m", m, sep = "_"))
  
  # Plot the line for this valid data frame
  lines(0:(ncol(data)-1),
        apply(data, 2, mean),
        type = "o",
        col = color_palette[i],
        lty = i,
        pch = 16
  )
}

# Step 6: Add a legend
legend_labels <- sapply(valid_indices, function(combination) {
  d <- combination[1]
  m <- combination[2]
  # Combine prod_prob and transf_prob values for each valid (d, m) combination
  prod_value <- prod_prob[2, d]  # Extract the prod_prob for the given d
  transf_value <- transf_prob[m]  # Extract the transf_prob for the given m
  
  # Construct the legend label (you can adjust the format as needed)
  paste("d", d, "_m", m, ": ", round(prod_value, 2), ", ", round(transf_value, 2), sep = "")
})

# Create the legend with the combined labels
legend(x = 105, y = (max_y + 1),
       title = "Resource dynamics\nprobabilities",
       legend = legend_labels, # Use the dynamically created labels
       col = color_palette, # Use the corresponding colors from the palette
       lty = 1:length(valid_indices), # Line types corresponding to each valid combination
       pch = 16, # Points for each combination
       lwd = 2,  # Line width
       bty = "n", # No box around the legend
       xpd = TRUE) # Allow legend to be drawn outside the plot region

### Age at sexual maturity ----

#get the frequency of each age across parameter values and repetitions
for (d in 1:17) {
  for (m in 1:19) {
    # create a data frame for each parameter value
    assign(paste("d", d, "m", m, sep = "_"), data.frame(matrix(0, nrow = 0, ncol = 101)))
    # retrieve the data frame
    temp_df <- get(paste("d", d, "m", m, sep = "_"))
    # modify the column names
    colnames(temp_df) <- as.character(0:100)
    # reassign the modified data frame in the original data frame
    assign(paste("d", d, "m", m, sep = "_"), temp_df)
    
    # Loop through repetitions
    for (r in 1:10) {
      # Construct the file name for the corresponding summary file
      summary_file <- file.path(summary_dir, sprintf("lht_list_d%d_m%d_r%d.fst", d, m, r))
      
      # Check if the summary file exists
      if (file.exists(summary_file)) {
        # Read the summary file
        results_s6 <- read_fst(summary_file)
        
        # Extract the Age at sexual maturity (assuming it's stored under a column "asm")
        asm_vector <- results_s6$asm
        
        # Create a new row with zeros
        new_row <- rep(0, 101)
        
        # Update the new row based on Age at sexual maturity values
        for (val in asm_vector) {
          if (val >= 0 & val <= 100 & !is.na(val)) {
            new_row[val + 1] <- new_row[val + 1] + 1  # +1 to account for index starting at 1 in R
          }
        }
        
        # Change column names to match
        new_row_df <- as.data.frame(t(new_row))
        colnames(new_row_df) <- as.character(0:100)
        
        # Add new row into the data frame
        temp_df <- get(paste("d", d, "m", m, sep = "_"))  # current data frame
        temp_df <- rbind(temp_df, new_row_df)  # new row
        assign(paste("d", d, "m", m, sep = "_"), temp_df)  # updated data frame
      } else {
        warning(sprintf("Summary file not found: %s", summary_file))
      }
    }
  }
}

#### Plot the densities ----

# Initialize the named list to store valid combinations of (d, m)
valid_indices <- list()

# Loop through all combinations of d and m
for (d in 1:17) {
  for (m in 1:19) {
    # Dynamically construct the variable name for the current data frame
    df_name <- paste("d", d, "m", m, sep = "_")
    
    # Check if the data frame exists to avoid errors
    if (exists(df_name)) {
      # Get the data frame
      df <- get(df_name)
      
      # Check if the sum of means for the data frame is greater than zero
      if (sum(apply(df, 2, mean, na.rm = TRUE)) > 0) {
        # Construct the name for the valid combination
        combination_name <- paste("d", d, "m", m, sep = "_")
        
        # Store the valid combination with the name
        valid_indices[[combination_name]] <- c(d, m)
      }
    }
  }
}

# Check the result
valid_indices

# Step 3: Create a color palette for only the valid data frames
color_palette <- hcl.colors(length(valid_indices), palette = "zissou 1", alpha = 0.5)

# Step 4: Prepare an empty plot
par(mfrow = c(1, 1), mar = c(5, 4, 4, 8))

#get the maximum value for y

# Initialize an empty vector to store the means for all d
all_means <- c()

# Loop through d values from 1 to 17
for (d in 1:17) {
  for (m in 1:19){
    # Dynamically get the data frame name using paste
    current_data <- get(paste("d", d, "m", m, sep = "_"))
    
    # Calculate column means for the current data frame
    column_means <- apply(current_data, 2, mean, na.rm = TRUE)
    
    # Append the column means to the all_means vector
    all_means <- c(all_means, column_means)
  }
}

# Get the maximum value of all the means collected
max_y <- max(all_means, na.rm = TRUE)

#plot it
plot(c(0, 100), c(0,(max_y + 1)),
     xlab = "Age",
     ylab = "Frequency",
     main = "Age at sexual maturity",
     type = "n")

# Step 5: Loop through valid indices to calculate and plot the KDEs
for(i in 1:length(valid_indices)) {
  # Extract the combination of d and m
  combination <- valid_indices[[i]]
  d <- combination[1]  # First element is d
  m <- combination[2]  # Second element is m
  # Dynamically get the data frame name and data
  data <- get(paste("d", d, "m", m, sep = "_"))
  
  # Plot the line for this valid data frame
  lines(0:(ncol(data)-1),
        apply(data, 2, mean),
        type = "o",
        col = color_palette[i],
        lty = i,
        pch = 16
  )
}

# Step 6: Add a legend
legend_labels <- sapply(valid_indices, function(combination) {
  d <- combination[1]
  m <- combination[2]
  # Combine prod_prob and transf_prob values for each valid (d, m) combination
  prod_value <- prod_prob[2, d]  # Extract the prod_prob for the given d
  transf_value <- transf_prob[m]  # Extract the transf_prob for the given m
  
  # Construct the legend label (you can adjust the format as needed)
  paste("d", d, "_m", m, ": ", round(prod_value, 2), ", ", round(transf_value, 2), sep = "")
})

# Create the legend with the combined labels
legend(x = 105, y = (max_y + 1),
       title = "Resource dynamics\nprobabilities",
       legend = legend_labels, # Use the dynamically created labels
       col = color_palette, # Use the corresponding colors from the palette
       lty = 1:length(valid_indices), # Line types corresponding to each valid combination
       pch = 16, # Points for each combination
       lwd = 2,  # Line width
       bty = "n", # No box around the legend
       xpd = TRUE) # Allow legend to be drawn outside the plot region

###Age at first reproduction ----

#get the frequency of each age across parameter values and repetitions
for (d in 1:17) {
  for (m in 1:19) {
    # create a data frame for each parameter value
    assign(paste("d", d, "m", m, sep = "_"), data.frame(matrix(0, nrow = 0, ncol = 101)))
    # retrieve the data frame
    temp_df <- get(paste("d", d, "m", m, sep = "_"))
    # modify the column names
    colnames(temp_df) <- as.character(0:100)
    # reassign the modified data frame in the original data frame
    assign(paste("d", d, "m", m, sep = "_"), temp_df)
    
    # Loop through repetitions
    for (r in 1:10) {
      # Construct the file name for the corresponding summary file
      summary_file <- file.path(summary_dir, sprintf("lht_list_d%d_m%d_r%d.fst", d, m, r))
      
      # Check if the summary file exists
      if (file.exists(summary_file)) {
        # Read the summary file
        results_s6 <- read_fst(summary_file)
        
        # Extract the Age at first reproduction (assuming it's stored under a column "afr")
        afr_vector <- results_s6$afr
        
        # Create a new row with zeros
        new_row <- rep(0, 101)
        
        # Update the new row based on Age at first reproduction values
        for (val in afr_vector) {
          if (val >= 0 & val <= 100 & !is.na(val)) {
            new_row[val + 1] <- new_row[val + 1] + 1  # +1 to account for index starting at 1 in R
          }
        }
        
        # Change column names to match
        new_row_df <- as.data.frame(t(new_row))
        colnames(new_row_df) <- as.character(0:100)
        
        # Add new row into the data frame
        temp_df <- get(paste("d", d, "m", m, sep = "_"))  # current data frame
        temp_df <- rbind(temp_df, new_row_df)  # new row
        assign(paste("d", d, "m", m, sep = "_"), temp_df)  # updated data frame
      } else {
        warning(sprintf("Summary file not found: %s", summary_file))
      }
    }
  }
}

#### Plot the densities ----

# Initialize the named list to store valid combinations of (d, m)
valid_indices <- list()

# Loop through all combinations of d and m
for (d in 1:17) {
  for (m in 1:19) {
    # Dynamically construct the variable name for the current data frame
    df_name <- paste("d", d, "m", m, sep = "_")
    
    # Check if the data frame exists to avoid errors
    if (exists(df_name)) {
      # Get the data frame
      df <- get(df_name)
      
      # Check if the sum of means for the data frame is greater than zero
      if (sum(apply(df, 2, mean, na.rm = TRUE)) > 0) {
        # Construct the name for the valid combination
        combination_name <- paste("d", d, "m", m, sep = "_")
        
        # Store the valid combination with the name
        valid_indices[[combination_name]] <- c(d, m)
      }
    }
  }
}

# Check the result
valid_indices

# Step 3: Create a color palette for only the valid data frames
color_palette <- hcl.colors(length(valid_indices), palette = "zissou 1", alpha = 0.5)

# Step 4: Prepare an empty plot
par(mfrow = c(1, 1), mar = c(5, 4, 4, 8))

#get the maximum value for y

# Initialize an empty vector to store the means for all d
all_means <- c()

# Loop through d values from 1 to 17
for (d in 1:17) {
  for (m in 1:19){
    # Dynamically get the data frame name using paste
    current_data <- get(paste("d", d, "m", m, sep = "_"))
    
    # Calculate column means for the current data frame
    column_means <- apply(current_data, 2, mean, na.rm = TRUE)
    
    # Append the column means to the all_means vector
    all_means <- c(all_means, column_means)
  }
}

# Get the maximum value of all the means collected
max_y <- max(all_means, na.rm = TRUE)

#plot it
plot(c(0, 100), c(0,(max_y + 1)),
     xlab = "Age",
     ylab = "Frequency",
     main = "Age at first reproduction",
     type = "n")

# Step 5: Loop through valid indices to calculate and plot the KDEs
for(i in 1:length(valid_indices)) {
  # Extract the combination of d and m
  combination <- valid_indices[[i]]
  d <- combination[1]  # First element is d
  m <- combination[2]  # Second element is m
  # Dynamically get the data frame name and data
  data <- get(paste("d", d, "m", m, sep = "_"))
  
  # Plot the line for this valid data frame
  lines(0:(ncol(data)-1),
        apply(data, 2, mean),
        type = "o",
        col = color_palette[i],
        lty = i,
        pch = 16
  )
}

# Step 6: Add a legend
legend_labels <- sapply(valid_indices, function(combination) {
  d <- combination[1]
  m <- combination[2]
  # Combine prod_prob and transf_prob values for each valid (d, m) combination
  prod_value <- prod_prob[2, d]  # Extract the prod_prob for the given d
  transf_value <- transf_prob[m]  # Extract the transf_prob for the given m
  
  # Construct the legend label (you can adjust the format as needed)
  paste("d", d, "_m", m, ": ", round(prod_value, 2), ", ", round(transf_value, 2), sep = "")
})

# Create the legend with the combined labels
legend(x = 105, y = (max_y + 1),
       title = "Resource dynamics\nprobabilities",
       legend = legend_labels, # Use the dynamically created labels
       col = color_palette, # Use the corresponding colors from the palette
       lty = 1:length(valid_indices), # Line types corresponding to each valid combination
       pch = 16, # Points for each combination
       lwd = 2,  # Line width
       bty = "n", # No box around the legend
       xpd = TRUE) # Allow legend to be drawn outside the plot region

###Age at last reproduction ----

#get the frequency of each age across parameter values and repetitions
for (d in 1:17) {
  for (m in 1:19) {
    # create a data frame for each parameter value
    assign(paste("d", d, "m", m, sep = "_"), data.frame(matrix(0, nrow = 0, ncol = 101)))
    # retrieve the data frame
    temp_df <- get(paste("d", d, "m", m, sep = "_"))
    # modify the column names
    colnames(temp_df) <- as.character(0:100)
    # reassign the modified data frame in the original data frame
    assign(paste("d", d, "m", m, sep = "_"), temp_df)
    
    # Loop through repetitions
    for (r in 1:10) {
      # Construct the file name for the corresponding summary file
      summary_file <- file.path(summary_dir, sprintf("lht_list_d%d_m%d_r%d.fst", d, m, r))
      
      # Check if the summary file exists
      if (file.exists(summary_file)) {
        # Read the summary file
        results_s6 <- read_fst(summary_file)
        
        # Extract the Age at last reproduction (assuming it's stored under a column "alr")
        alr_vector <- results_s6$alr
        
        # Create a new row with zeros
        new_row <- rep(0, 101)
        
        # Update the new row based on Age at last reproduction values
        for (val in alr_vector) {
          if (val >= 0 & val <= 100 & !is.na(val)) {
            new_row[val + 1] <- new_row[val + 1] + 1  # +1 to account for index starting at 1 in R
          }
        }
        
        # Change column names to match
        new_row_df <- as.data.frame(t(new_row))
        colnames(new_row_df) <- as.character(0:100)
        
        # Add new row into the data frame
        temp_df <- get(paste("d", d, "m", m, sep = "_"))  # current data frame
        temp_df <- rbind(temp_df, new_row_df)  # new row
        assign(paste("d", d, "m", m, sep = "_"), temp_df)  # updated data frame
      } else {
        warning(sprintf("Summary file not found: %s", summary_file))
      }
    }
  }
}

#### Plot the densities ----

# Initialize the named list to store valid combinations of (d, m)
valid_indices <- list()

# Loop through all combinations of d and m
for (d in 1:17) {
  for (m in 1:19) {
    # Dynamically construct the variable name for the current data frame
    df_name <- paste("d", d, "m", m, sep = "_")
    
    # Check if the data frame exists to avoid errors
    if (exists(df_name)) {
      # Get the data frame
      df <- get(df_name)
      
      # Check if the sum of means for the data frame is greater than zero
      if (sum(apply(df, 2, mean, na.rm = TRUE)) > 0) {
        # Construct the name for the valid combination
        combination_name <- paste("d", d, "m", m, sep = "_")
        
        # Store the valid combination with the name
        valid_indices[[combination_name]] <- c(d, m)
      }
    }
  }
}

# Check the result
valid_indices

# Step 3: Create a color palette for only the valid data frames
color_palette <- hcl.colors(length(valid_indices), palette = "zissou 1", alpha = 0.5)

# Step 4: Prepare an empty plot
par(mfrow = c(1, 1), mar = c(5, 4, 4, 8))

#get the maximum value for y

# Initialize an empty vector to store the means for all d
all_means <- c()

# Loop through d values from 1 to 17
for (d in 1:17) {
  for (m in 1:19){
    # Dynamically get the data frame name using paste
    current_data <- get(paste("d", d, "m", m, sep = "_"))
    
    # Calculate column means for the current data frame
    column_means <- apply(current_data, 2, mean, na.rm = TRUE)
    
    # Append the column means to the all_means vector
    all_means <- c(all_means, column_means)
  }
}

# Get the maximum value of all the means collected
max_y <- max(all_means, na.rm = TRUE)

#plot it
plot(c(0, 100), c(0,(max_y + 1)),
     xlab = "Age",
     ylab = "Frequency",
     main = "Age at last reproduction",
     type = "n")

# Step 5: Loop through valid indices to calculate and plot the KDEs
for(i in 1:length(valid_indices)) {
  # Extract the combination of d and m
  combination <- valid_indices[[i]]
  d <- combination[1]  # First element is d
  m <- combination[2]  # Second element is m
  # Dynamically get the data frame name and data
  data <- get(paste("d", d, "m", m, sep = "_"))
  
  # Plot the line for this valid data frame
  lines(0:(ncol(data)-1),
        apply(data, 2, mean),
        type = "o",
        col = color_palette[i],
        lty = i,
        pch = 16
  )
}

# Step 6: Add a legend
legend_labels <- sapply(valid_indices, function(combination) {
  d <- combination[1]
  m <- combination[2]
  # Combine prod_prob and transf_prob values for each valid (d, m) combination
  prod_value <- prod_prob[2, d]  # Extract the prod_prob for the given d
  transf_value <- transf_prob[m]  # Extract the transf_prob for the given m
  
  # Construct the legend label (you can adjust the format as needed)
  paste("d", d, "_m", m, ": ", round(prod_value, 2), ", ", round(transf_value, 2), sep = "")
})

# Create the legend with the combined labels
legend(x = 105, y = (max_y + 1),
       title = "Resource dynamics\nprobabilities",
       legend = legend_labels, # Use the dynamically created labels
       col = color_palette, # Use the corresponding colors from the palette
       lty = 1:length(valid_indices), # Line types corresponding to each valid combination
       pch = 16, # Points for each combination
       lwd = 2,  # Line width
       bty = "n", # No box around the legend
       xpd = TRUE) # Allow legend to be drawn outside the plot region

###Age at menopause ----

#get the frequency of each age across parameter values and repetitions
for (d in 1:17) {
  for (m in 1:19) {
    # create a data frame for each parameter value
    assign(paste("d", d, "m", m, sep = "_"), data.frame(matrix(0, nrow = 0, ncol = 101)))
    # retrieve the data frame
    temp_df <- get(paste("d", d, "m", m, sep = "_"))
    # modify the column names
    colnames(temp_df) <- as.character(0:100)
    # reassign the modified data frame in the original data frame
    assign(paste("d", d, "m", m, sep = "_"), temp_df)
    
    # Loop through repetitions
    for (r in 1:10) {
      # Construct the file name for the corresponding summary file
      summary_file <- file.path(summary_dir, sprintf("lht_list_d%d_m%d_r%d.fst", d, m, r))
      
      # Check if the summary file exists
      if (file.exists(summary_file)) {
        # Read the summary file
        results_s6 <- read_fst(summary_file)
        
        # Extract the Age at menopause (assuming it's stored under a column "meno")
        meno_vector <- results_s6$meno
        
        # Create a new row with zeros
        new_row <- rep(0, 101)
        
        # Update the new row based on Age at menopause values
        for (val in meno_vector) {
          if (val >= 0 & val <= 100 & !is.na(val)) {
            new_row[val + 1] <- new_row[val + 1] + 1  # +1 to account for index starting at 1 in R
          }
        }
        
        # Change column names to match
        new_row_df <- as.data.frame(t(new_row))
        colnames(new_row_df) <- as.character(0:100)
        
        # Add new row into the data frame
        temp_df <- get(paste("d", d, "m", m, sep = "_"))  # current data frame
        temp_df <- rbind(temp_df, new_row_df)  # new row
        assign(paste("d", d, "m", m, sep = "_"), temp_df)  # updated data frame
      } else {
        warning(sprintf("Summary file not found: %s", summary_file))
      }
    }
  }
}

#### Plot the densities ----

# Initialize the named list to store valid combinations of (d, m)
valid_indices <- list()

# Loop through all combinations of d and m
for (d in 1:17) {
  for (m in 1:19) {
    # Dynamically construct the variable name for the current data frame
    df_name <- paste("d", d, "m", m, sep = "_")
    
    # Check if the data frame exists to avoid errors
    if (exists(df_name)) {
      # Get the data frame
      df <- get(df_name)
      
      # Check if the sum of means for the data frame is greater than zero
      if (sum(apply(df, 2, mean, na.rm = TRUE)) > 0) {
        # Construct the name for the valid combination
        combination_name <- paste("d", d, "m", m, sep = "_")
        
        # Store the valid combination with the name
        valid_indices[[combination_name]] <- c(d, m)
      }
    }
  }
}

# Check the result
valid_indices

# Step 3: Create a color palette for only the valid data frames
color_palette <- hcl.colors(length(valid_indices), palette = "zissou 1", alpha = 0.5)

# Step 4: Prepare an empty plot
par(mfrow = c(1, 1), mar = c(5, 4, 4, 8))

#get the maximum value for y

# Initialize an empty vector to store the means for all d
all_means <- c()

# Loop through d values from 1 to 17
for (d in 1:17) {
  for (m in 1:19){
    # Dynamically get the data frame name using paste
    current_data <- get(paste("d", d, "m", m, sep = "_"))
    
    # Calculate column means for the current data frame
    column_means <- apply(current_data, 2, mean, na.rm = TRUE)
    
    # Append the column means to the all_means vector
    all_means <- c(all_means, column_means)
  }
}

# Get the maximum value of all the means collected
max_y <- max(all_means, na.rm = TRUE)

#plot it
plot(c(0, 100), c(0,(max_y + 1)),
     xlab = "Age",
     ylab = "Frequency",
     main = "Age at menopause",
     type = "n")

# Step 5: Loop through valid indices to calculate and plot the KDEs
for(i in 1:length(valid_indices)) {
  # Extract the combination of d and m
  combination <- valid_indices[[i]]
  d <- combination[1]  # First element is d
  m <- combination[2]  # Second element is m
  # Dynamically get the data frame name and data
  data <- get(paste("d", d, "m", m, sep = "_"))
  
  # Plot the line for this valid data frame
  lines(0:(ncol(data)-1),
        apply(data, 2, mean),
        type = "o",
        col = color_palette[i],
        lty = i,
        pch = 16
  )
}

# Step 6: Add a legend
legend_labels <- sapply(valid_indices, function(combination) {
  d <- combination[1]
  m <- combination[2]
  # Combine prod_prob and transf_prob values for each valid (d, m) combination
  prod_value <- prod_prob[2, d]  # Extract the prod_prob for the given d
  transf_value <- transf_prob[m]  # Extract the transf_prob for the given m
  
  # Construct the legend label (you can adjust the format as needed)
  paste("d", d, "_m", m, ": ", round(prod_value, 2), ", ", round(transf_value, 2), sep = "")
})

# Create the legend with the combined labels
legend(x = 105, y = (max_y + 1),
       title = "Resource dynamics\nprobabilities",
       legend = legend_labels, # Use the dynamically created labels
       col = color_palette, # Use the corresponding colors from the palette
       lty = 1:length(valid_indices), # Line types corresponding to each valid combination
       pch = 16, # Points for each combination
       lwd = 2,  # Line width
       bty = "n", # No box around the legend
       xpd = TRUE) # Allow legend to be drawn outside the plot region

