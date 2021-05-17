################################################################################
##                     Function to sample n random pairs                      ##
##                                                                            ##
##  Author: Diana Robledo-Ruiz. PhD student, Monash University                ##
##  Date: 2020-03-19                                                          ##
##                                                                            ##
## This function requires:                                                    ##
##   - Input: a matrix of pairs in vertical format (df)                       ##
##   - User specified parameters:                                             ##
##       - n = number of pairs to select                                      ##
##       - max.pairs.(fe)male = maximum number of pairs a (fe)male can have   ##
##   - Two custom made functions (included at the beginning of this script):  ##
##       - Two called "drop.(fe)males" that drop repeated (fe)males           ##
##                                                                            ##
## Index:                                                                     ##
##   Line 25:  Function drop.males                                            ##
##   Line 62:  Function drop.females                                          ##
##   Line 99:  Main function sample.random.pairs                              ##
##   Line 143: Example of use for sample.random.pairs (with recommended      ##
##             pre-treatment)                                                 ##
################################################################################



#################### Defining function DROP PAIRS for MALES ####################
## This function identifies males that are present in more than the max. number
## of pairs and drops the "extra" pairs

drop.males <- function(selection, max.pairs.male) {
  
  # Identify ID of males
  males <- unique(selection[, "male_band"])
  
  # Count how many times each male is present and drop extras
  for (i in 1:length(males)) {
    
    # Store how may times it appeared
    male_info <- nrow(selection[selection$male_band == males[i],]);
    
    # If it is present more than maximum
    if (male_info > max.pairs.male) {
      
      # Identify rows in which that male is present in selected pairs
      rows <- which(grepl(males[i], selection$male_band));
      
      # Store the identifier of "extra" rows (i.e. the ones to drop)
      rows2drop <- rows[(max.pairs.male+1):length(rows)]
    
      # Drop those extra rows from selected pairs
      selection <- selection[-c(rows2drop), ]
      
      # Delete the vector
      rm(rows2drop)
    }
  }
  
  return(selection)
}
################################################################################


################### Defining function DROP PAIRS for FEMALES ###################
## This function identifies females that are present in more than the max. 
## number of pairs and drops the "extra" pairs

drop.females <- function(selection, max.pairs.female) {
  
  # Identify ID of females
  females <- unique(selection[, "female_band"])
  
  # Count how many times each female is present and drop extras
  for (i in 1:length(females)) {
    
    # Store how may times it appeared
    female_info <- nrow(selection[selection$female_band == females[i],]);
    
    # If it is present more than maximum
    if (female_info > max.pairs.female) {
      
      # Identify rows in which that female is present in selected pairs
      rows <- which(grepl(females[i], selection$female_band));
      
      # Store the identifier of "extra" rows (i.e. the ones to drop)
      rows2drop <- rows[(max.pairs.female+1):length(rows)]
    
      # Drop those extra rows from selected pairs
      selection <- selection[-c(rows2drop), ]
      
      # Delete the vector
      rm(rows2drop)
    }
  }
  
  return(selection)
}
################################################################################


################ Defining MAIN function SAMPLE N RANDOM PAIRS ##################
sample.random.pairs <- function(df, n, max.pairs.male, max.pairs.female) {
  
  ##### Select n random pairs
  pairs <- sample(1:nrow(df), size = n, replace = FALSE)  # intergers without replacement (no repeated pairs)
  
  sel <- df[pairs,]
  
  used_pairs <- pairs
  
  
  ##### If was n = 1, no point looking for repeated, return result
  if (nrow(sel) != 1) {
    
    ##### First round of dropping males/females
    sel <- drop.males(selection = sel, max.pairs.male = max.pairs.male)
    sel <- drop.females(selection = sel, max.pairs.female = max.pairs.female)
    
    ##### Iterative loop:
    # Check number of pairs, if not enough, grab more and drop repeated (fe)males
    while (nrow(sel) < n) {
      
      # Identify random row numbers to be added
      next_pairs <- sample((1:nrow(df))[-used_pairs],  # remaining pairs
                           n-(nrow(sel)),              # how many to add
                           replace = FALSE)            # no replacement, to avoid repeated pairs
      
      used_pairs <- c(used_pairs, next_pairs)
      
      # Add those rows to selection
      sel <- rbind(sel, df[next_pairs,])
      
      # Drop males/females if repeated too many times
      sel <- drop.males(selection = sel, max.pairs.male = max.pairs.male)
      sel <- drop.females(selection = sel, max.pairs.female = max.pairs.female)
    }
  }
  ##### Final dataframe with random pairs
  return(sel)
}
################################################################################



############### Example of use (with recommended pre-treatment) ################
## matrix <- read.table(file = "matrix.txt",                                  ##
##                      sep = "\t",                                           ##
##                      header = FALSE)                                       ##
##                                                                            ##
## names(matrix) <- lapply(matrix[1, ], as.character)                         ##
##                                                                            ##
## matrix <- matrix[-c(1, 2), -2]  # Drop empty column and row                ##
##                                                                            ##
## library(tidyr)                                                             ##
##                                                                            ##
## df <- gather(matrix,                                                       ##
##              key   = "male_band",                                          ##
##              value = "MSI",                                                ##
##              -UniqueID)                                                    ##
##                                                                            ##
## names(df)[names(df) == "UniqueID"] <- "female_band"                        ##
##                                                                            ##
## df[] <- lapply(df, as.character)                                           ##
##                                                                            ##
## rownames(df) <- 1:(nrow(df))  # Assign row names from 1 to nrow            ##
##                                                                            ##
## sample.random.pairs(df = df,                                               ##
##                     n = 16,                                                ##
##                     max.pairs.male = 4,                                    ##
##                     max.pairs.female = 3)                                  ##
################################################################################
