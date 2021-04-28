# Evaluating a breeding management strategy and population-level inbreeding avoidance for the wild population of Helmeted Honeyeater

In this repository you can find the functions and code to run simulations of a breeding management strategy and different levels of inbreeding avoidance as presented in Robledo-Ruiz et al. (_submitted_) "Testing a novel _in-situ_ breeding management strategy for a critically endangered population".

All functions require as input an MSI matrix in vertical format (dataframe) with each row pertaining to one male-female pair and their MSI score. The columns should be named: 
  - First column "female_band": ID of female
  - Second column "male_band": ID of male
  - Third column "MSI": MSI score of that pair

If you have a matrix with females as columns and males as rows (as the one obtained from PMx), and want to make it vertical:
  - Make sure the first column (that should contain male IDs) is named "UniqueID"
  - Follow the next script:

  ##### Import the file directly obtained from PMx
  matrix <- read.table(file,
                       sep = "\t",
                       header = FALSE)
  
  names(matrix) <- lapply(matrix[1, ], as.character)
  
  matrix <- matrix[-c(1, 2), -2]  # Drop empty column and row
  
  ##### Make MSI matrix a vertical dataframe with appropriate format
  library(tidyr)
  df <- gather(matrix, 
               key   = "male_band", 
               value = "MSI", 
               -UniqueID)  # Exclude the 1st column ("UniqueID") = males
  
  names(df)[names(df) == "UniqueID"] <- "female_band"  # Change name of 1st column
  
  df[] <- lapply(df, as.character)  # Make all columns character
  
  rownames(df) <- 1:(nrow(df))  # Assign row names from 1 to nrow



