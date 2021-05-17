################################################################################
##         Function to find the best n pairs (those with lowest MSI)          ##
##                                                                            ##
##  Author: Diana Robledo-Ruiz. PhD student, Monash University                ##
##  Date: 2020-03-12                                                          ##
##                                                                            ##
## This function requires:                                                    ##
##   - Input: a matrix of pairs in vertical format (df)                       ##
##   - User specified parameters:                                             ##
##       - n = number of pairs to select                                      ##
##       - max.pairs.(fe)male = maximum number of pairs a (fe)male can have   ##
##   - Two custom made functions called "drop.(fe)males" that I include at    ##
##     the beginning of this script                                           ##
##                                                                            ##
## Index:                                                                     ##
##   Line 25: Function drop.males                                             ##
##   Line 62: Function drop.females                                           ##
##   Line 99: Main function find.best.pairs                                  ##
##   Line 146: Example of use for find.best.pairs (with recommended           ##
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


#################### Defining main function FIND BEST PAIRS ####################
find.best.pairs <- function(df, n, max.pairs.male, max.pairs.female) {
   
  df <- df[order(df$MSI), ]  # Sort dataframe by MSI (lowest first)
   
  ##### Select the first n pairs
  sel <- df[1:n,]
  print(paste("Number of initial pairs:" , nrow(sel)))
  
  used_pairs <- n
  
  ##### First round of dropping males/females
  sel <- drop.males(selection = sel, max.pairs.male = max.pairs.male)
  print(paste("Number of pairs left after dropping males:", nrow(sel)))
  
  sel <- drop.females(selection = sel, max.pairs.female = max.pairs.female)
  print(paste("Number of pairs left after dropping females:", nrow(sel)))
  
  
  ##### Iterative loop:
  # Check number of pairs, if not enough, grab more and drop repeated males/females
  while (nrow(sel) < n) {
    
    # Identify row numbers to be added
    next_pairs <- c((used_pairs+1):((used_pairs)+(n-nrow(sel))))
    used_pairs <- next_pairs[length(next_pairs)]
    
    # Add those rows to selection
    sel <- rbind(sel, df[next_pairs,])
    
    # Drop males/females if repeated too many times
    sel <- drop.males(selection = sel, max.pairs.male = max.pairs.male)
    print(paste("Number of pairs left after dropping males:", nrow(sel)))
    sel <- drop.females(selection = sel, max.pairs.female = max.pairs.female)
    print(paste("Number of pairs left after dropping females:", nrow(sel)))
    
    # Spit out finishing message
    if (nrow(sel) == n)
      print(paste("The required", n, "pairs have been selected."))
  }
  
  ##### Final dataframe with best pairs
  return(sel)
}
################################################################################


################################ Example of use ################################
## matrix <- read.table(file = "matrix.txt",                                  ##                                    
##                       sep = "\t",                                          ##
##                       header = FALSE)                                      ##
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
##              -UniqueID)  # Exclude the 1st column ("UniqueID") = males     ##
##                                                                            ##
## names(df)[names(df) == "UniqueID"] <- "female_band"  # Name 1st column     ##
##                                                                            ##
## df[] <- lapply(df, as.character)  # Make all columns character             ##
##                                                                            ##
## rownames(df) <- 1:(nrow(df))  # Assign row names from 1 to nrow            ##
##                                                                            ##
## find.best.pairs(df = df,                                                   ##
##                   n = 16,                                                  ##
##                   max.pairs.male = 4,                                      ##
##                   max.pairs.female = 3)                                    ##
################################################################################
