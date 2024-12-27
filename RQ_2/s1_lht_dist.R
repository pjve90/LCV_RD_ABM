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

# Directory for summary files
summary_dir <- "./Scenario_1/lht_lists/"

# Distribution of life history traits ----

### Longevity ----

#get the frequency of each age across parameter values and repetitions
for (d in 1:17) {
  # create a data frame for each parameter value
  assign(paste("d", d, sep = "_"), data.frame(matrix(0, nrow = 0, ncol = 101)))
  # retrieve the data frame
  temp_df <- get(paste("d", d, sep = "_"))
  # modify the column names
  colnames(temp_df) <- as.character(0:100)
  # reassign the modified data frame in the original data frame
  assign(paste("d", d, sep = "_"), temp_df)
  
  # Loop through repetitions
  for (r in 1:10) {
    # Construct the file name for the corresponding summary file
    summary_file <- file.path(summary_dir, sprintf("lht_list_d%d_r%d.fst", d, r))
    
    # Check if the summary file exists
    if (file.exists(summary_file)) {
      # Read the summary file
      results_s1 <- read_fst(summary_file)
      
      # Extract the longevity (assuming it's stored under a column "lng")
      lng_vector <- results_s1$lng
      
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
      temp_df <- get(paste("d", d, sep = "_"))  # current data frame
      temp_df <- rbind(temp_df, new_row_df)  # new row
      assign(paste("d", d, sep = "_"), temp_df)  # updated data frame
    } else {
      warning(sprintf("Summary file not found: %s", summary_file))
    }
  }
}

#### Plot the densities ----

# Step 1: Create a vector to store indices of data frames with positive longevity
valid_indices <- c()

# Step 2: First pass through to identify valid data frames
for(d in 1:17) {
  # Check if the sum of means is greater than zero
  if (sum(apply(get(paste("d", d, sep = "_")), 2, mean)) > 0) {
    valid_indices <- c(valid_indices, d)  # Store the valid index
  }
}

# Step 3: Create a color palette for only the valid data frames
color_palette <- hcl.colors(length(valid_indices), palette = "zissou 1", alpha = 0.5)

# Step 4: Prepare an empty plot
par(mfrow = c(1, 1), mar = c(5, 4, 4, 8))

#get the maximum value for y

# Initialize an empty vector to store the means for all d
all_means <- c()

# Loop through d values from 1 to 17
for (d in 1:17) {
  # Dynamically get the data frame name using paste
  current_data <- get(paste("d", d, sep = "_"))
  
  # Calculate column means for the current data frame
  column_means <- apply(current_data, 2, mean, na.rm = TRUE)
  
  # Append the column means to the all_means vector
  all_means <- c(all_means, column_means)
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
  d <- valid_indices[i]
  data <- get(paste("d", d, sep = "_"))
  
  # Plot the line for this valid data frame
  lines(0:(ncol(get(paste("d", d, sep = "_")))-1),
        apply(data, 2, mean),
        type = "o",
        col = color_palette[i],
        lty = i,
        pch = 16
  )
}

# Step 6: Add a legend
legend(x = 105, y = (max_y + 1),
       title = "Production\nprobabilities",
       legend = prod_prob[2, valid_indices], # Use valid indices for the legend
       col = color_palette, # Use the corresponding colors from the palette
       lty = c(1:length(valid_indices)),
       pch = 16,
       lwd = 2,
       bty = "n",
       xpd = T)

### Lifetime reproductive output ----

#get the frequency of each age across parameter values and repetitions
for (d in 1:17) {
  # create a data frame for each parameter value
  assign(paste("d", d, sep = "_"), data.frame(matrix(0, nrow = 0, ncol = 101)))
  # retrieve the data frame
  temp_df <- get(paste("d", d, sep = "_"))
  # modify the column names
  colnames(temp_df) <- as.character(0:100)
  # reassign the modified data frame in the original data frame
  assign(paste("d", d, sep = "_"), temp_df)
  
  # Loop through repetitions
  for (r in 1:10) {
    # Construct the file name for the corresponding summary file
    summary_file <- file.path(summary_dir, sprintf("lht_list_d%d_r%d.fst", d, r))
    
    # Check if the summary file exists
    if (file.exists(summary_file)) {
      # Read the summary file
      results_s1 <- read_fst(summary_file)
      
      # Extract the Lifetime reproductive output (assuming it's stored under a column "lro")
      lro_vector <- results_s1$lro
      
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
      temp_df <- get(paste("d", d, sep = "_"))  # current data frame
      temp_df <- rbind(temp_df, new_row_df)  # new row
      assign(paste("d", d, sep = "_"), temp_df)  # updated data frame
    } else {
      warning(sprintf("Summary file not found: %s", summary_file))
    }
  }
}

#### Plot the densities ----

# Step 1: Create a vector to store indices of data frames with positive Lifetime reproductive output
valid_indices <- c()

# Step 2: First pass through to identify valid data frames
for(d in 1:17) {
  # Check if the sum of means is greater than zero
  if (sum(apply(get(paste("d", d, sep = "_")), 2, mean)) > 0) {
    valid_indices <- c(valid_indices, d)  # Store the valid index
  }
}

# Step 3: Create a color palette for only the valid data frames
color_palette <- hcl.colors(length(valid_indices), palette = "zissou 1", alpha = 0.5)

# Step 4: Prepare an empty plot
par(mfrow = c(1, 1), mar = c(5, 4, 4, 8))

#get the maximum value for y

# Initialize an empty vector to store the means for all d
all_means <- c()

# Loop through d values from 1 to 17
for (d in 1:17) {
  # Dynamically get the data frame name using paste
  current_data <- get(paste("d", d, sep = "_"))
  
  # Calculate column means for the current data frame
  column_means <- apply(current_data, 2, mean, na.rm = TRUE)
  
  # Append the column means to the all_means vector
  all_means <- c(all_means, column_means)
}

# Get the maximum value of all the means collected
max_y <- max(all_means, na.rm = TRUE)

#plot it!
plot(c(0, 15), c(0, (max_y+1)),
     xlab = "Number of descendants",
     ylab = "Frequency",
     main = "Lifetime reproductive output",
     type = "n")

# Step 5: Loop through valid indices to calculate and plot the KDEs
for(i in 1:length(valid_indices)) {
  d <- valid_indices[i]
  data <- get(paste("d", d, sep = "_"))
  
  # Plot the line for this valid data frame
  lines(0:(ncol(get(paste("d", d, sep = "_")))-1),
        apply(data, 2, mean),
        type = "o",
        col = color_palette[i],
        lty = i,
        pch = 16
  )
}

# Step 6: Add a legend
legend(x = 16, y = (max_y+1),
       title = "Production\nprobabilities",
       legend = prod_prob[2, valid_indices], # Use valid indices for the legend
       col = color_palette, # Use the corresponding colors from the palette
       lty = c(1:length(valid_indices)),
       pch = 16,
       lwd = 2,
       bty = "n",
       xpd = T)

### Age at sexual maturity ----

#get the frequency of each age across parameter values and repetitions
for (d in 1:17) {
  # create a data frame for each parameter value
  assign(paste("d", d, sep = "_"), data.frame(matrix(0, nrow = 0, ncol = 101)))
  # retrieve the data frame
  temp_df <- get(paste("d", d, sep = "_"))
  # modify the column names
  colnames(temp_df) <- as.character(0:100)
  # reassign the modified data frame in the original data frame
  assign(paste("d", d, sep = "_"), temp_df)
  
  # Loop through repetitions
  for (r in 1:10) {
    # Construct the file name for the corresponding summary file
    summary_file <- file.path(summary_dir, sprintf("lht_list_d%d_r%d.fst", d, r))
    
    # Check if the summary file exists
    if (file.exists(summary_file)) {
      # Read the summary file
      results_s1 <- read_fst(summary_file)
      
      # Extract the Age at sexual maturity (assuming it's stored under a column "asm")
      asm_vector <- results_s1$asm
      
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
      temp_df <- get(paste("d", d, sep = "_"))  # current data frame
      temp_df <- rbind(temp_df, new_row_df)  # new row
      assign(paste("d", d, sep = "_"), temp_df)  # updated data frame
    } else {
      warning(sprintf("Summary file not found: %s", summary_file))
    }
  }
}

#### Plot the densities ----

# Step 1: Create a vector to store indices of data frames with positive Age at sexual maturity
valid_indices <- c()

# Step 2: First pass through to identify valid data frames
for(d in 1:17) {
  # Check if the sum of means is greater than zero
  if (sum(apply(get(paste("d", d, sep = "_")), 2, mean)) > 0) {
    valid_indices <- c(valid_indices, d)  # Store the valid index
  }
}

# Step 3: Create a color palette for only the valid data frames
color_palette <- hcl.colors(length(valid_indices), palette = "zissou 1", alpha = 0.5)

# Step 4: Prepare an empty plot
par(mfrow = c(1, 1), mar = c(5, 4, 4, 8))

#get the maximum value for y

# Initialize an empty vector to store the means for all d
all_means <- c()

# Loop through d values from 1 to 17
for (d in 1:17) {
  # Dynamically get the data frame name using paste
  current_data <- get(paste("d", d, sep = "_"))
  
  # Calculate column means for the current data frame
  column_means <- apply(current_data, 2, mean, na.rm = TRUE)
  
  # Append the column means to the all_means vector
  all_means <- c(all_means, column_means)
}

# Get the maximum value of all the means collected
max_y <- max(all_means, na.rm = TRUE)

#plot it!
plot(c(10, 25), c(0, (max_y+1)),
     xlab = "Age",
     ylab = "Frequency",
     main = "Age at sexual maturity",
     type = "n")

# Step 5: Loop through valid indices to calculate and plot the KDEs
for(i in 1:length(valid_indices)) {
  d <- valid_indices[i]
  data <- get(paste("d", d, sep = "_"))
  
  # Plot the line for this valid data frame
  lines(0:(ncol(get(paste("d", d, sep = "_")))-1),
        apply(data, 2, mean),
        type = "o",
        col = color_palette[i],
        lty = i,
        pch = 16
  )
}

# Step 6: Add a legend
legend(x = 26, y = (max_y+1),
       title = "Production\nprobabilities",
       legend = prod_prob[2, valid_indices], # Use valid indices for the legend
       col = color_palette, # Use the corresponding colors from the palette
       lty = c(1:length(valid_indices)),
       pch = 16,
       lwd = 2,
       bty = "n",
       xpd = T)

###Age at first reproduction ----

#get the frequency of each age across parameter values and repetitions
for (d in 1:17) {
  # create a data frame for each parameter value
  assign(paste("d", d, sep = "_"), data.frame(matrix(0, nrow = 0, ncol = 101)))
  # retrieve the data frame
  temp_df <- get(paste("d", d, sep = "_"))
  # modify the column names
  colnames(temp_df) <- as.character(0:100)
  # reassign the modified data frame in the original data frame
  assign(paste("d", d, sep = "_"), temp_df)
  
  # Loop through repetitions
  for (r in 1:10) {
    # Construct the file name for the corresponding summary file
    summary_file <- file.path(summary_dir, sprintf("lht_list_d%d_r%d.fst", d, r))
    
    # Check if the summary file exists
    if (file.exists(summary_file)) {
      # Read the summary file
      results_s1 <- read_fst(summary_file)
      
      # Extract the Age at first reproduction (assuming it's stored under a column "afr")
      afr_vector <- results_s1$afr
      
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
      temp_df <- get(paste("d", d, sep = "_"))  # current data frame
      temp_df <- rbind(temp_df, new_row_df)  # new row
      assign(paste("d", d, sep = "_"), temp_df)  # updated data frame
    } else {
      warning(sprintf("Summary file not found: %s", summary_file))
    }
  }
}

#### Plot the densities ----

# Step 1: Create a vector to store indices of data frames with positive Age at first reproduction
valid_indices <- c()

# Step 2: First pass through to identify valid data frames
for(d in 1:17) {
  # Check if the sum of means is greater than zero
  if (sum(apply(get(paste("d", d, sep = "_")), 2, mean)) > 0) {
    valid_indices <- c(valid_indices, d)  # Store the valid index
  }
}

# Step 3: Create a color palette for only the valid data frames
color_palette <- hcl.colors(length(valid_indices), palette = "zissou 1", alpha = 0.5)

# Step 4: Prepare an empty plot
par(mfrow = c(1, 1), mar = c(5, 4, 4, 8))

#get the maximum value for y

# Initialize an empty vector to store the means for all d
all_means <- c()

# Loop through d values from 1 to 17
for (d in 1:17) {
  # Dynamically get the data frame name using paste
  current_data <- get(paste("d", d, sep = "_"))
  
  # Calculate column means for the current data frame
  column_means <- apply(current_data, 2, mean, na.rm = TRUE)
  
  # Append the column means to the all_means vector
  all_means <- c(all_means, column_means)
}

# Get the maximum value of all the means collected
max_y <- max(all_means, na.rm = TRUE)

#plot it!
plot(c(15, 30), c(0, (max_y+1)),
     xlab = "Age",
     ylab = "Frequency",
     main = "Age at first reproduction",
     type = "n")

# Step 5: Loop through valid indices to calculate and plot the KDEs
for(i in 1:length(valid_indices)) {
  d <- valid_indices[i]
  data <- get(paste("d", d, sep = "_"))
  
  # Plot the line for this valid data frame
  lines(0:(ncol(get(paste("d", d, sep = "_")))-1),
        apply(data, 2, mean),
        type = "o",
        col = color_palette[i],
        lty = i,
        pch = 16
  )
}

# Step 6: Add a legend
legend(x = 31, y = (max_y+1),
       title = "Production\nprobabilities",
       legend = prod_prob[2, valid_indices], # Use valid indices for the legend
       col = color_palette, # Use the corresponding colors from the palette
       lty = c(1:length(valid_indices)),
       pch = 16,
       lwd = 2,
       bty = "n",
       xpd = T)

###Age at last reproduction ----

#get the frequency of each age across parameter values and repetitions
for (d in 1:17) {
  # create a data frame for each parameter value
  assign(paste("d", d, sep = "_"), data.frame(matrix(0, nrow = 0, ncol = 101)))
  # retrieve the data frame
  temp_df <- get(paste("d", d, sep = "_"))
  # modify the column names
  colnames(temp_df) <- as.character(0:100)
  # reassign the modified data frame in the original data frame
  assign(paste("d", d, sep = "_"), temp_df)
  
  # Loop through repetitions
  for (r in 1:10) {
    # Construct the file name for the corresponding summary file
    summary_file <- file.path(summary_dir, sprintf("lht_list_d%d_r%d.fst", d, r))
    
    # Check if the summary file exists
    if (file.exists(summary_file)) {
      # Read the summary file
      results_s1 <- read_fst(summary_file)
      
      # Extract the Age at last reproduction (assuming it's stored under a column "alr")
      alr_vector <- results_s1$alr
      
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
      temp_df <- get(paste("d", d, sep = "_"))  # current data frame
      temp_df <- rbind(temp_df, new_row_df)  # new row
      assign(paste("d", d, sep = "_"), temp_df)  # updated data frame
    } else {
      warning(sprintf("Summary file not found: %s", summary_file))
    }
  }
}

#### Plot the densities ----

# Step 1: Create a vector to store indices of data frames with positive Age at last reproduction
valid_indices <- c()

# Step 2: First pass through to identify valid data frames
for(d in 1:17) {
  # Check if the sum of means is greater than zero
  if (sum(apply(get(paste("d", d, sep = "_")), 2, mean)) > 0) {
    valid_indices <- c(valid_indices, d)  # Store the valid index
  }
}

# Step 3: Create a color palette for only the valid data frames
color_palette <- hcl.colors(length(valid_indices), palette = "zissou 1", alpha = 0.5)

# Step 4: Prepare an empty plot
par(mfrow = c(1, 1), mar = c(5, 4, 4, 8))

#get the maximum value for y

# Initialize an empty vector to store the means for all d
all_means <- c()

# Loop through d values from 1 to 17
for (d in 1:17) {
  # Dynamically get the data frame name using paste
  current_data <- get(paste("d", d, sep = "_"))
  
  # Calculate column means for the current data frame
  column_means <- apply(current_data, 2, mean, na.rm = TRUE)
  
  # Append the column means to the all_means vector
  all_means <- c(all_means, column_means)
}

# Get the maximum value of all the means collected
max_y <- max(all_means, na.rm = TRUE)

#plot it!
plot(c(20, 65), c(0, (max_y+1)),
     xlab = "Age",
     ylab = "Frequency",
     main = "Age at last reproduction",
     type = "n")

# Step 5: Loop through valid indices to calculate and plot the KDEs
for(i in 1:length(valid_indices)) {
  d <- valid_indices[i]
  data <- get(paste("d", d, sep = "_"))
  
  # Plot the line for this valid data frame
  lines(0:(ncol(get(paste("d", d, sep = "_")))-1),
        apply(data, 2, mean),
        type = "o",
        col = color_palette[i],
        lty = i,
        pch = 16
  )
}

# Step 6: Add a legend
legend(x = 67, y = (max_y+1),
       title = "Production\nprobabilities",
       legend = prod_prob[2, valid_indices], # Use valid indices for the legend
       col = color_palette, # Use the corresponding colors from the palette
       lty = c(1:length(valid_indices)),
       pch = 16,
       lwd = 2,
       bty = "n",
       xpd = T)

###Age at menopause ----

#get the frequency of each age across parameter values and repetitions
for (d in 1:17) {
  # create a data frame for each parameter value
  assign(paste("d", d, sep = "_"), data.frame(matrix(0, nrow = 0, ncol = 101)))
  # retrieve the data frame
  temp_df <- get(paste("d", d, sep = "_"))
  # modify the column names
  colnames(temp_df) <- as.character(0:100)
  # reassign the modified data frame in the original data frame
  assign(paste("d", d, sep = "_"), temp_df)
  
  # Loop through repetitions
  for (r in 1:10) {
    # Construct the file name for the corresponding summary file
    summary_file <- file.path(summary_dir, sprintf("lht_list_d%d_r%d.fst", d, r))
    
    # Check if the summary file exists
    if (file.exists(summary_file)) {
      # Read the summary file
      results_s1 <- read_fst(summary_file)
      
      # Extract the Age at menopause (assuming it's stored under a column "meno")
      meno_vector <- results_s1$meno
      
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
      temp_df <- get(paste("d", d, sep = "_"))  # current data frame
      temp_df <- rbind(temp_df, new_row_df)  # new row
      assign(paste("d", d, sep = "_"), temp_df)  # updated data frame
    } else {
      warning(sprintf("Summary file not found: %s", summary_file))
    }
  }
}

#### Plot the densities ----

# Step 1: Create a vector to store indices of data frames with positive Age at menopause
valid_indices <- c()

# Step 2: First pass through to identify valid data frames
for(d in 1:17) {
  # Check if the sum of means is greater than zero
  if (sum(apply(get(paste("d", d, sep = "_")), 2, mean)) > 0) {
    valid_indices <- c(valid_indices, d)  # Store the valid index
  }
}

# Step 3: Create a color palette for only the valid data frames
color_palette <- hcl.colors(length(valid_indices), palette = "zissou 1", alpha = 0.5)

# Step 4: Prepare an empty plot
par(mfrow = c(1, 1), mar = c(5, 4, 4, 8))

#get the maximum value for y

# Initialize an empty vector to store the means for all d
all_means <- c()

# Loop through d values from 1 to 17
for (d in 1:17) {
  # Dynamically get the data frame name using paste
  current_data <- get(paste("d", d, sep = "_"))
  
  # Calculate column means for the current data frame
  column_means <- apply(current_data, 2, mean, na.rm = TRUE)
  
  # Append the column means to the all_means vector
  all_means <- c(all_means, column_means)
}

# Get the maximum value of all the means collected
max_y <- max(all_means, na.rm = TRUE)

#plot it!
plot(c(35, 70), c(0, (max_y+1)),
     xlab = "Age",
     ylab = "Frequency",
     main = "Age at menopause",
     type = "n")

# Step 5: Loop through valid indices to calculate and plot the KDEs
for(i in 1:length(valid_indices)) {
  d <- valid_indices[i]
  data <- get(paste("d", d, sep = "_"))
  
  # Plot the line for this valid data frame
  lines(0:(ncol(get(paste("d", d, sep = "_")))-1),
        apply(data, 2, mean),
        type = "o",
        col = color_palette[i],
        lty = i,
        pch = 16
  )
}

# Step 6: Add a legend
legend(x = 72, y = (max_y+1),
       title = "Production\nprobabilities",
       legend = prod_prob[2, valid_indices], # Use valid indices for the legend
       col = color_palette, # Use the corresponding colors from the palette
       lty = c(1:length(valid_indices)),
       pch = 16,
       lwd = 2,
       bty = "n",
       xpd = T)
